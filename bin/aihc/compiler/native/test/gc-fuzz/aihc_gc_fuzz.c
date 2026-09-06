/* Fuzz driver for the semispace collector.

   The driver reads scripts from standard input. A script builds a heap,
   changes it, and forces collections. After each collection the driver walks
   the new space and prints every object, root, and static object. The test
   compares that report with a model of the same script.

   The driver keeps a table from object identity to the current address of the
   object. Every object gets its own info table, and the identity field of that
   table is the object identity. After a collection the driver rebuilds the
   address table from the new space, so a script can name an object after the
   object has moved.

   Script commands, one for each line:

     machine G R S        new machine with G globals, R root slots, and an
                          initial space of S bytes
     srt I O C e...       reference table I names O static objects and C
                          child tables
     current_srt I|-1     publish table I as the running function's table
     mvars N              create N empty MVars
     fill K               allocate garbage until K words remain
     reserve W            reserve W words; this can collect
     new ID KIND BITS S   new object with kind node, closure, thunk, or
                          partial, pointer bitmap BITS, and table S or -1
     array ID N S         new array with N elements and table S or -1
     set ID I V           write value V to field or element I
     update ID V          turn thunk ID into an indirection to V
     blackhole ID         start the evaluation of thunk ID
     unblackhole ID V     finish the evaluation of thunk ID with value V
     supdate K V          turn static thunk K into an indirection to V
     sset K I V           write value V to field I of static node K
     global I V           write value V to global I
     root I V             write value V to root slot I
     stable ID            make a stable name for object ID
     mvar_put K V         fill MVar K with value V
     mvar_take K          empty MVar K
     thread SLOT V        set the current thread's function, continuation, or
                          value resume slot
     collect              force a collection
     end                  finish the script

   Values: n (null), hID (heap object), sK (static slot), wHEX (raw word), and
   aID (the address of a heap object as a raw word). Static slots 0-7 are
   thunks, 8-11 are nodes with two pointer fields, and 12-15 are nullary
   constructors. No section lists the static objects: the collector finds
   them by address.

   The driver prints one block for each collection:

     collection C
     space LIVE CAPACITY TARGET OLD_CAPACITY REQUIRED
     obj ID KIND N V...
     global I V
     root I V
     stable V
     mvar K full V | mvar K empty
     thread SLOT V
     blackhole V
     static K thunk | static K ind V | static K node V V
     violation TEXT
     endcollection

   and prints done after the end command. A fatal script error prints fail
   TEXT and stops the process. */

#include "../../runtime/aihc_runtime_internal.h"

#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

enum {
  STATIC_THUNKS = 8,
  STATIC_NODES = 4,
  STATIC_NULLARY = 4,
  STATIC_NODE_FIELDS = 2,
  STATIC_COUNT = STATIC_THUNKS + STATIC_NODES + STATIC_NULLARY,
  LINE_CAPACITY = 1 << 16,
  MAX_TOKENS = 4096,
};

#define OLD_SPACE_FILL 0xAB

typedef struct {
  AihcSlot header;
  AihcSlot target;
} StaticThunk;

typedef struct {
  AihcSlot header;
  AihcSlot fields[STATIC_NODE_FIELDS];
} StaticNode;

typedef struct {
  AihcSlot header;
} StaticNullary;

typedef struct {
  int defined;
  AihcValue *address;
  AihcInfo *info;
  uint8_t *pointers;
  uint64_t *shadow;
  uint8_t *has_shadow;
  uint64_t field_count;
} Entry;

static AihcInfo static_thunk_info[STATIC_THUNKS];
static AihcInfo static_node_info[STATIC_NODES];
static AihcInfo static_nullary_info[STATIC_NULLARY];
static const uint8_t static_node_pointers[STATIC_NODE_FIELDS] = {1, 1};
static StaticThunk static_thunks[STATIC_THUNKS];
static StaticNode static_nodes[STATIC_NODES];
static StaticNullary static_nullary[STATIC_NULLARY];

static AihcMachine *machine;
static Entry *entries;
static size_t entry_capacity;
static AihcSrt **srts;
static size_t srt_count;
static AihcSlot *root_slots;
static uint64_t root_count;
static AihcMVar **mvars;
static size_t mvar_count;
static uint64_t reserved_words;
static size_t command_index;
static AihcValue **object_starts;
static size_t object_start_count;
static size_t object_start_capacity;
static int srts_linked;

static _Noreturn void fail(const char *message) {
  printf("fail %s\n", message);
  fflush(stdout);
  exit(1);
}

/* Violations are collected while a report line is in progress and printed
   before the report ends, so every report line stays whole. */
static const char **violations;
static size_t violation_count;
static size_t violation_capacity;

static void violation(const char *message) {
  if (violation_count == violation_capacity) {
    violation_capacity = violation_capacity == 0 ? 16 : violation_capacity * 2;
    const char **grown =
        realloc(violations, violation_capacity * sizeof(*violations));
    if (grown == NULL) {
      fail("out of memory");
    }
    violations = grown;
  }
  violations[violation_count++] = message;
}

static void print_violations(void) {
  for (size_t index = 0; index < violation_count; ++index) {
    printf("violation %s\n", violations[index]);
  }
  violation_count = 0;
}

static void *checked_calloc(size_t count, size_t size) {
  void *memory = calloc(count == 0 ? 1 : count, size);
  if (memory == NULL) {
    fail("out of memory");
  }
  return memory;
}

static uint64_t parse_unsigned(const char *token) {
  char *end = NULL;
  if (*token < '0' || *token > '9') {
    fail("expected a number");
  }
  uint64_t value = strtoull(token, &end, 10);
  if (*end != 0) {
    fail("invalid number");
  }
  return value;
}

static int64_t parse_signed(const char *token) {
  if (token[0] == '-') {
    return -(int64_t)parse_unsigned(token + 1);
  }
  return (int64_t)parse_unsigned(token);
}

static uint64_t parse_hex(const char *token) {
  char *end = NULL;
  uint64_t value = strtoull(token, &end, 16);
  if (*token == 0 || *end != 0) {
    fail("invalid hex word");
  }
  return value;
}

/* Static slot numbers cover the three pools in order. */
static AihcValue *static_slot_address(uint64_t slot) {
  if (slot < STATIC_THUNKS) {
    return (AihcValue *)&static_thunks[slot];
  }
  if (slot < STATIC_THUNKS + STATIC_NODES) {
    return (AihcValue *)&static_nodes[slot - STATIC_THUNKS];
  }
  if (slot < STATIC_COUNT) {
    return (AihcValue *)&static_nullary[slot - STATIC_THUNKS - STATIC_NODES];
  }
  fail("static slot out of range");
}

static int static_slot_of(const void *address, uint64_t *slot) {
  for (uint64_t index = 0; index < STATIC_COUNT; ++index) {
    if (static_slot_address(index) == address) {
      *slot = index;
      return 1;
    }
  }
  return 0;
}

static void initialize_statics(void) {
  for (int index = 0; index < STATIC_THUNKS; ++index) {
    static_thunk_info[index].object_kind = AIHC_OBJECT_THUNK;
    static_thunk_info[index].frame_kind = AIHC_FRAME_NONE;
    static_thunk_info[index].identity = 0;
    static_thunks[index].header =
        (AihcSlot)(uintptr_t)&static_thunk_info[index];
  }
  for (int index = 0; index < STATIC_NODES; ++index) {
    static_node_info[index].object_kind = AIHC_OBJECT_NODE;
    static_node_info[index].frame_kind = AIHC_FRAME_NONE;
    static_node_info[index].field_count = STATIC_NODE_FIELDS;
    static_node_info[index].field_is_pointer = static_node_pointers;
    static_nodes[index].header = (AihcSlot)(uintptr_t)&static_node_info[index];
  }
  for (int index = 0; index < STATIC_NULLARY; ++index) {
    static_nullary_info[index].object_kind = AIHC_OBJECT_NODE;
    static_nullary_info[index].frame_kind = AIHC_FRAME_NONE;
    static_nullary[index].header =
        (AihcSlot)(uintptr_t)&static_nullary_info[index];
  }
}

/* Put every static object back into its initial state for a new script. */
static void reset_statics(void) {
  for (int index = 0; index < STATIC_THUNKS; ++index) {
    static_thunk_info[index].srt = NULL;
    static_thunks[index].header =
        (AihcSlot)(uintptr_t)&static_thunk_info[index];
    static_thunks[index].target = 0;
  }
  for (int index = 0; index < STATIC_NODES; ++index) {
    static_node_info[index].srt = NULL;
    for (int field = 0; field < STATIC_NODE_FIELDS; ++field) {
      static_nodes[index].fields[field] = 0;
    }
  }
}

static Entry *entry_of(uint64_t identity) {
  if (identity == 0) {
    fail("identity zero is reserved");
  }
  if (identity >= entry_capacity) {
    size_t capacity = entry_capacity == 0 ? 64 : entry_capacity;
    while (capacity <= identity) {
      capacity *= 2;
    }
    Entry *grown = realloc(entries, capacity * sizeof(*entries));
    if (grown == NULL) {
      fail("out of memory");
    }
    memset(grown + entry_capacity, 0,
           (capacity - entry_capacity) * sizeof(*entries));
    entries = grown;
    entry_capacity = capacity;
  }
  return &entries[identity];
}

static Entry *live_entry(uint64_t identity) {
  Entry *entry = entry_of(identity);
  if (!entry->defined) {
    fail("object is not defined");
  }
  if (entry->address == NULL) {
    fail("object is dead");
  }
  return entry;
}

static void free_entries(void) {
  for (size_t index = 0; index < entry_capacity; ++index) {
    free(entries[index].info);
    free(entries[index].pointers);
    free(entries[index].shadow);
    free(entries[index].has_shadow);
  }
  free(entries);
  entries = NULL;
  entry_capacity = 0;
}

static void free_srts(void) {
  for (size_t index = 0; index < srt_count; ++index) {
    free(srts[index]);
  }
  free(srts);
  srts = NULL;
  srt_count = 0;
}

static const AihcSrt *srt_of(int64_t index) {
  if (index < 0) {
    return NULL;
  }
  if ((uint64_t)index >= srt_count || srts[index] == NULL) {
    fail("reference table is not defined");
  }
  return srts[index];
}

/* Parse a value token. A raw word is a non-pointer payload. */
static AihcSlot parse_value(const char *token, int *is_word) {
  *is_word = 0;
  switch (token[0]) {
  case 'n':
    return 0;
  case 'h':
    return (AihcSlot)(uintptr_t)live_entry(parse_unsigned(token + 1))->address;
  case 's':
    return (AihcSlot)(uintptr_t)static_slot_address(parse_unsigned(token + 1));
  case 'w':
    *is_word = 1;
    return parse_hex(token + 1);
  case 'a':
    *is_word = 1;
    return (AihcSlot)(uintptr_t)live_entry(parse_unsigned(token + 1))->address;
  default:
    fail("invalid value");
  }
}

static AihcSlot parse_pointer(const char *token) {
  int is_word = 0;
  AihcSlot value = parse_value(token, &is_word);
  if (is_word) {
    fail("raw word in a pointer slot");
  }
  return value;
}

static AihcValue *parse_object(const char *token) {
  AihcSlot value = parse_pointer(token);
  if (value == 0) {
    fail("null where an object is required");
  }
  return (AihcValue *)(uintptr_t)value;
}

static size_t remaining_words(void) {
  return (size_t)(machine->heap_limit - machine->heap_next) / sizeof(AihcSlot);
}

static int in_range(const void *address, const uint8_t *start, size_t bytes) {
  uintptr_t value = (uintptr_t)address;
  uintptr_t first = (uintptr_t)start;
  return start != NULL && value >= first && value - first < bytes;
}

static int is_object_start(const AihcValue *object) {
  size_t low = 0;
  size_t high = object_start_count;
  while (low < high) {
    size_t middle = low + (high - low) / 2;
    if (object_starts[middle] == object) {
      return 1;
    }
    if ((uintptr_t)object_starts[middle] < (uintptr_t)object) {
      low = middle + 1;
    } else {
      high = middle;
    }
  }
  return 0;
}

static void print_pointer(AihcSlot slot) {
  const void *address = (const void *)(uintptr_t)slot;
  uint64_t static_slot = 0;
  if (slot == 0) {
    printf(" n");
    return;
  }
  if (in_range(address, machine->heap_start,
               (size_t)(machine->heap_limit - machine->heap_start))) {
    const AihcValue *object = address;
    if (!is_object_start(object)) {
      violation("pointer into the middle of the new space");
      printf(" x%" PRIx64, slot);
      return;
    }
    printf(" h%" PRIuPTR, aihc_value_info_table(object)->identity);
    return;
  }
  if (in_range(address, machine->other_space, machine->other_space_bytes)) {
    violation("pointer into the old space");
    printf(" o");
    return;
  }
  if (static_slot_of(address, &static_slot)) {
    printf(" s%" PRIu64, static_slot);
    return;
  }
  violation("pointer outside every space");
  printf(" x%" PRIx64, slot);
}

static const char *kind_name(AihcObjectKind kind) {
  switch (kind) {
  case AIHC_OBJECT_NODE:
    return "node";
  case AIHC_OBJECT_CLOSURE:
    return "closure";
  case AIHC_OBJECT_THUNK:
    return "thunk";
  case AIHC_OBJECT_PARTIAL_CONSTRUCTOR:
    return "partial";
  case AIHC_OBJECT_INDIRECTION:
    return "indirection";
  case AIHC_OBJECT_BLACKHOLE:
    return "blackhole";
  case AIHC_OBJECT_ARRAY:
    return "array";
  default:
    return "invalid";
  }
}

/* The payload of one object. A partial constructor spends field zero on the
   count of the slots it has filled, so its fields start one slot later. */
static AihcSlot *entry_fields(Entry *entry) {
  if (entry->info->object_kind == AIHC_OBJECT_PARTIAL_CONSTRUCTOR) {
    return aihc_partial_fields(entry->address);
  }
  return entry->address->fields;
}

static void print_object(AihcValue *object) {
  const AihcInfo *info = aihc_value_info_table(object);
  uintptr_t identity = info->identity;
  Entry *entry =
      identity == 0 || identity >= entry_capacity ? NULL : &entries[identity];
  if (entry == NULL || !entry->defined) {
    violation("object with an unknown identity survived");
    printf("obj %" PRIuPTR " %s 0\n", identity, kind_name(info->object_kind));
    return;
  }
  if (entry->address != NULL) {
    violation("two objects share one identity");
  }
  entry->address = object;
  if (info->object_kind == AIHC_OBJECT_BLACKHOLE) {
    if (info->field_count != entry->info->field_count ||
        info->field_is_pointer != entry->info->field_is_pointer) {
      violation("blackhole info table does not match its thunk");
    }
  } else if (info != entry->info) {
    violation("object header does not name its own info table");
  }
  if (info->object_kind == AIHC_OBJECT_ARRAY) {
    uint64_t length = aihc_array_length(object);
    AihcSlot *elements = aihc_array_elements(object);
    printf("obj %" PRIuPTR " array %" PRIu64, identity, length);
    for (uint64_t index = 0; index < length; ++index) {
      print_pointer(elements[index]);
    }
    printf("\n");
    return;
  }
  const AihcSlot *fields = entry_fields(entry);
  printf("obj %" PRIuPTR " %s %" PRIuPTR, identity,
         kind_name(info->object_kind), info->field_count);
  for (uint64_t index = 0; index < info->field_count; ++index) {
    if (entry->pointers[index]) {
      print_pointer(fields[index]);
    } else {
      if (entry->has_shadow[index] && entry->shadow[index] != fields[index]) {
        violation("non-pointer field changed");
      }
      printf(" w%" PRIx64, fields[index]);
    }
  }
  printf("\n");
}

/* An evaluated static object that a collection under -Zs did not mark keeps
   a target into the old space. The model knows which slots those are, so the
   driver reports the target without a violation. */
static void print_static_target(AihcSlot slot) {
  const void *address = (const void *)(uintptr_t)slot;
  uint64_t static_slot = 0;
  if (slot == 0) {
    printf(" n");
  } else if (in_range(address, machine->heap_start,
                      (size_t)(machine->heap_limit - machine->heap_start))) {
    const AihcValue *object = address;
    if (is_object_start(object)) {
      printf(" h%" PRIuPTR, aihc_value_info_table(object)->identity);
    } else {
      printf(" x%" PRIx64, slot);
    }
  } else if (in_range(address, machine->other_space,
                      machine->other_space_bytes)) {
    printf(" o");
  } else if (static_slot_of(address, &static_slot)) {
    printf(" s%" PRIu64, static_slot);
  } else {
    printf(" x%" PRIx64, slot);
  }
}

static void print_static_thunk(uint64_t slot) {
  const StaticThunk *thunk = &static_thunks[slot];
  const AihcValue *object = (const AihcValue *)thunk;
  if (aihc_value_kind(object) == AIHC_OBJECT_THUNK) {
    printf("static %" PRIu64 " thunk\n", slot);
  } else if (aihc_value_kind(object) != AIHC_OBJECT_INDIRECTION) {
    violation("static thunk has an invalid kind");
    printf("static %" PRIu64 " invalid\n", slot);
  } else {
    printf("static %" PRIu64 " ind", slot);
    print_static_target(thunk->target);
    printf("\n");
  }
}

static void report_collection(uint64_t required_bytes) {
  uint8_t *start = machine->heap_start;
  uint8_t *next = machine->heap_next;
  size_t capacity = (size_t)(machine->heap_limit - start);
  if (machine->other_space != NULL) {
    memset(machine->other_space, OLD_SPACE_FILL, machine->other_space_bytes);
  }
  printf("collection %zu\n", command_index);
  printf("space %zu %zu %" PRIu64 " %" PRIu64 " %" PRIu64 "\n",
         (size_t)(next - start), capacity, machine->semispace_bytes,
         machine->other_space_bytes, required_bytes);

  object_start_count = 0;
  uint8_t *cursor = start;
  while (cursor < next) {
    AihcValue *object = (AihcValue *)cursor;
    const void *header = (const void *)(uintptr_t)object->header;
    if (in_range(header, start, capacity) ||
        in_range(header, machine->other_space, machine->other_space_bytes)) {
      violation("forwarding header in the new space");
      break;
    }
    if (object_start_count == object_start_capacity) {
      object_start_capacity =
          object_start_capacity == 0 ? 256 : object_start_capacity * 2;
      AihcValue **grown = realloc(object_starts, object_start_capacity *
                                                     sizeof(*object_starts));
      if (grown == NULL) {
        fail("out of memory");
      }
      object_starts = grown;
    }
    object_starts[object_start_count++] = object;
    cursor += sizeof(AihcSlot) * aihc_value_words(object);
  }
  if (cursor != next) {
    violation("object sizes do not end at the allocation pointer");
  }

  for (size_t index = 0; index < entry_capacity; ++index) {
    entries[index].address = NULL;
  }
  for (size_t index = 0; index < object_start_count; ++index) {
    AihcValue *object = object_starts[index];
    if (aihc_value_kind(object) == AIHC_OBJECT_INDIRECTION) {
      violation("indirection in the new space");
    }
    print_object(object);
  }

  for (uint64_t index = 0; index < machine->global_count; ++index) {
    printf("global %" PRIu64, index);
    print_pointer(machine->globals[index]);
    printf("\n");
  }
  for (uint64_t index = 0; index < root_count; ++index) {
    printf("root %" PRIu64, index);
    print_pointer(root_slots[index]);
    printf("\n");
  }
  for (const AihcStableName *name = machine->stable_names; name != NULL;
       name = name->next) {
    printf("stable");
    print_pointer((AihcSlot)(uintptr_t)name->value);
    printf("\n");
  }
  for (size_t index = 0; index < mvar_count; ++index) {
    if (mvars[index]->full) {
      printf("mvar %zu full", index);
      print_pointer(mvars[index]->value);
      printf("\n");
    } else {
      printf("mvar %zu empty\n", index);
    }
  }
  const AihcThread *thread = machine->current_thread;
  printf("thread function");
  print_pointer((AihcSlot)(uintptr_t)thread->resume_function);
  printf("\nthread continuation");
  print_pointer((AihcSlot)(uintptr_t)thread->resume_continuation);
  printf("\n");
  if (thread->resume_kind == AIHC_RESUME_CONTINUE &&
      thread->resume_count == 1) {
    printf("thread value");
    print_pointer(thread->resume_value);
    printf("\n");
  }
  for (const AihcBlackhole *blackhole = machine->blackholes; blackhole != NULL;
       blackhole = blackhole->next) {
    printf("blackhole");
    print_pointer((AihcSlot)(uintptr_t)blackhole->object);
    printf("\n");
  }
  for (uint64_t slot = 0; slot < STATIC_THUNKS; ++slot) {
    print_static_thunk(slot);
  }
  for (uint64_t slot = 0; slot < STATIC_NODES; ++slot) {
    printf("static %" PRIu64 " node", STATIC_THUNKS + slot);
    for (int field = 0; field < STATIC_NODE_FIELDS; ++field) {
      print_pointer(static_nodes[slot].fields[field]);
    }
    printf("\n");
  }
  print_violations();
  printf("endcollection\n");
}

/* Reserve words through the collector's own entry point and report a
   collection when the space changed. */
static void ensure(uint64_t words) {
  uint8_t *before = machine->heap_start;
  aihc_ensure_heap(machine, words, root_count, root_slots);
  if (machine->heap_start != before) {
    report_collection(words * sizeof(AihcSlot));
  }
}

static void command_machine(char **tokens, size_t count) {
  if (count != 4) {
    fail("machine expects three arguments");
  }
  uint64_t global_count = parse_unsigned(tokens[1]);
  uint64_t slot_count = parse_unsigned(tokens[2]);
  uint64_t space_bytes = parse_unsigned(tokens[3]);
  if (space_bytes == 0 || space_bytes % sizeof(AihcSlot) != 0) {
    fail("initial space must be a positive number of words");
  }
  if (machine != NULL) {
    free(machine->heap_start);
    free(machine->other_space);
    machine->heap_start = NULL;
    machine->other_space = NULL;
  }
  free_entries();
  free_srts();
  srts_linked = 0;
  free(root_slots);
  free(mvars);
  mvars = NULL;
  mvar_count = 0;
  reset_statics();
  aihc_current_srt = NULL;
  reserved_words = 0;
  machine = aihc_machine_new(global_count);
  /* Replace the default space so each script chooses its own size. */
  free(machine->heap_start);
  machine->semispace_bytes = space_bytes;
  machine->heap_start = checked_calloc(1, space_bytes);
  machine->heap_next = machine->heap_start;
  machine->heap_limit = machine->heap_start + space_bytes;
  root_count = slot_count;
  root_slots = checked_calloc(slot_count, sizeof(*root_slots));
}

static void command_srt(char **tokens, size_t count) {
  if (count < 4) {
    fail("srt expects an index and two counts");
  }
  uint64_t index = parse_unsigned(tokens[1]);
  uint64_t objects = parse_unsigned(tokens[2]);
  uint64_t children = parse_unsigned(tokens[3]);
  if (count != 4 + objects + children) {
    fail("srt entry count does not match");
  }
  if (srts_linked) {
    fail("srt must come before any object or collection");
  }
  if (index >= srt_count) {
    size_t grown_count = (size_t)index + 1;
    AihcSrt **grown = realloc(srts, grown_count * sizeof(*srts));
    if (grown == NULL) {
      fail("out of memory");
    }
    memset(grown + srt_count, 0, (grown_count - srt_count) * sizeof(*srts));
    srts = grown;
    srt_count = grown_count;
  }
  if (srts[index] != NULL) {
    fail("reference table is already defined");
  }
  AihcSrt *srt = checked_calloc(1, sizeof(*srt) + (size_t)(objects + children) *
                                                      sizeof(uintptr_t));
  srt->object_count = objects;
  srt->child_count = children;
  srts[index] = srt;
  for (uint64_t entry = 0; entry < objects; ++entry) {
    const char *token = tokens[4 + entry];
    if (token[0] != 's') {
      fail("srt objects must be static slots");
    }
    srt->entries[entry] =
        (uintptr_t)static_slot_address(parse_unsigned(token + 1));
  }
  /* Children can name tables that a later line defines, so store the index
     and patch it when the script publishes or attaches a table. */
  for (uint64_t entry = 0; entry < children; ++entry) {
    srt->entries[objects + entry] =
        (uintptr_t)parse_unsigned(tokens[4 + objects + entry]);
  }
}

/* Replace child indices with table addresses once every table exists. */
static void link_srts(void) {
  if (srts_linked) {
    return;
  }
  for (size_t index = 0; index < srt_count; ++index) {
    AihcSrt *srt = srts[index];
    if (srt == NULL) {
      continue;
    }
    for (uintptr_t entry = 0; entry < srt->child_count; ++entry) {
      uintptr_t child = srt->entries[srt->object_count + entry];
      srt->entries[srt->object_count + entry] =
          (uintptr_t)srt_of((int64_t)child);
    }
  }
  srts_linked = 1;
}

static void command_new(char **tokens, size_t count) {
  if (count != 5) {
    fail("new expects four arguments");
  }
  uint64_t identity = parse_unsigned(tokens[1]);
  Entry *entry = entry_of(identity);
  if (entry->defined) {
    fail("object is already defined");
  }
  AihcObjectKind kind = 0;
  if (strcmp(tokens[2], "node") == 0) {
    kind = AIHC_OBJECT_NODE;
  } else if (strcmp(tokens[2], "closure") == 0) {
    kind = AIHC_OBJECT_CLOSURE;
  } else if (strcmp(tokens[2], "thunk") == 0) {
    kind = AIHC_OBJECT_THUNK;
  } else if (strcmp(tokens[2], "partial") == 0) {
    kind = AIHC_OBJECT_PARTIAL_CONSTRUCTOR;
  } else {
    fail("invalid object kind");
  }
  const char *bits = tokens[3];
  uint64_t field_count = strcmp(bits, "-") == 0 ? 0 : strlen(bits);
  AihcInfo *info = checked_calloc(1, sizeof(*info));
  uint8_t *pointers = checked_calloc(field_count, sizeof(*pointers));
  for (uint64_t index = 0; index < field_count; ++index) {
    if (bits[index] != '0' && bits[index] != '1') {
      fail("invalid pointer bitmap");
    }
    pointers[index] = bits[index] == '1';
  }
  info->identity = identity;
  info->field_count = field_count;
  info->field_is_pointer = pointers;
  info->frame_kind = AIHC_FRAME_NONE;
  info->object_kind = kind;
  info->srt = srt_of(parse_signed(tokens[4]));
  /* A partial constructor carries its applied count in field zero and shares
     one info table with the saturated form the count is measured against. */
  int partial = kind == AIHC_OBJECT_PARTIAL_CONSTRUCTOR;
  if (partial) {
    AihcInfo *saturated = checked_calloc(1, sizeof(*saturated));
    saturated->identity = identity;
    saturated->field_count = field_count;
    saturated->field_is_pointer = pointers;
    saturated->frame_kind = AIHC_FRAME_NONE;
    saturated->object_kind = AIHC_OBJECT_NODE;
    info->next = saturated;
  }
  uint64_t words = partial ? 2 + field_count : aihc_object_words(info);
  if (words > reserved_words) {
    fail("block exceeds its reservation");
  }
  reserved_words -= words;
  AihcValue *object = aihc_gc_allocate(machine, words);
  object->header = (AihcSlot)(uintptr_t)info;
  if (partial) {
    object->fields[0] = field_count;
  }
  entry->defined = 1;
  entry->address = object;
  entry->info = info;
  entry->pointers = pointers;
  entry->shadow = checked_calloc(field_count, sizeof(*entry->shadow));
  entry->has_shadow = checked_calloc(field_count, sizeof(*entry->has_shadow));
  entry->field_count = field_count;
}

static void command_array(char **tokens, size_t count) {
  if (count != 4) {
    fail("array expects three arguments");
  }
  uint64_t identity = parse_unsigned(tokens[1]);
  uint64_t length = parse_unsigned(tokens[2]);
  Entry *entry = entry_of(identity);
  if (entry->defined) {
    fail("object is already defined");
  }
  AihcInfo *info = checked_calloc(1, sizeof(*info));
  info->identity = identity;
  info->field_count = 1;
  info->frame_kind = AIHC_FRAME_NONE;
  info->object_kind = AIHC_OBJECT_ARRAY;
  info->srt = srt_of(parse_signed(tokens[3]));
  uint64_t words = 2 + length;
  if (words > reserved_words) {
    fail("block exceeds its reservation");
  }
  reserved_words -= words;
  AihcValue *object = aihc_gc_allocate(machine, words);
  object->header = (AihcSlot)(uintptr_t)info;
  object->fields[0] = length;
  entry->defined = 1;
  entry->address = object;
  entry->info = info;
  entry->pointers = checked_calloc(length, sizeof(*entry->pointers));
  memset(entry->pointers, 1, length);
  entry->shadow = checked_calloc(length, sizeof(*entry->shadow));
  entry->has_shadow = checked_calloc(length, sizeof(*entry->has_shadow));
  entry->field_count = length;
}

static void command_set(char **tokens, size_t count) {
  if (count != 4) {
    fail("set expects three arguments");
  }
  /* Parse the value first: a value lookup can grow the entry table and move
     every entry. */
  uint64_t index = parse_unsigned(tokens[2]);
  int is_word = 0;
  AihcSlot value = parse_value(tokens[3], &is_word);
  Entry *entry = live_entry(parse_unsigned(tokens[1]));
  if (index >= entry->field_count) {
    fail("field index out of range");
  }
  if (entry->pointers[index] == is_word) {
    fail("value kind does not match the field kind");
  }
  if (entry->info->object_kind == AIHC_OBJECT_ARRAY) {
    aihc_array_elements(entry->address)[index] = value;
    return;
  }
  entry_fields(entry)[index] = value;
  if (is_word) {
    entry->shadow[index] = value;
    entry->has_shadow[index] = 1;
  }
}

static void command_fill(char **tokens, size_t count) {
  if (count != 2) {
    fail("fill expects one argument");
  }
  uint64_t keep = parse_unsigned(tokens[1]);
  size_t remaining = remaining_words();
  if (remaining <= keep) {
    return;
  }
  uint64_t words = remaining - keep;
  AihcInfo *info = checked_calloc(1, sizeof(*info));
  info->field_count = words - 1;
  info->frame_kind = AIHC_FRAME_NONE;
  info->object_kind = AIHC_OBJECT_NODE;
  AihcValue *object = aihc_gc_allocate(machine, words);
  object->header = (AihcSlot)(uintptr_t)info;
  /* The filler is garbage. Its info table stays allocated because a later
     walk of the old space must not read freed memory. */
}

static void command_thread(char **tokens, size_t count) {
  if (count != 3) {
    fail("thread expects two arguments");
  }
  AihcThread *thread = machine->current_thread;
  AihcSlot value = parse_pointer(tokens[2]);
  if (strcmp(tokens[1], "function") == 0) {
    thread->resume_function = (AihcValue *)(uintptr_t)value;
  } else if (strcmp(tokens[1], "continuation") == 0) {
    thread->resume_continuation = (AihcValue *)(uintptr_t)value;
  } else if (strcmp(tokens[1], "value") == 0) {
    thread->resume_kind = AIHC_RESUME_CONTINUE;
    thread->resume_count = 1;
    thread->resume_value = value;
  } else {
    fail("invalid thread slot");
  }
}

static void run_command(char **tokens, size_t count) {
  const char *name = tokens[0];
  if (strcmp(name, "machine") == 0) {
    command_machine(tokens, count);
    return;
  }
  if (machine == NULL) {
    fail("machine must come first");
  }
  if (strcmp(name, "srt") == 0) {
    command_srt(tokens, count);
  } else if (strcmp(name, "current_srt") == 0) {
    if (count != 2) {
      fail("current_srt expects one argument");
    }
    link_srts();
    aihc_current_srt = srt_of(parse_signed(tokens[1]));
  } else if (strcmp(name, "mvars") == 0) {
    if (count != 2 || mvars != NULL) {
      fail("mvars expects one argument and runs once");
    }
    mvar_count = (size_t)parse_unsigned(tokens[1]);
    mvars = checked_calloc(mvar_count, sizeof(*mvars));
    for (size_t index = 0; index < mvar_count; ++index) {
      mvars[index] = aihc_mvar_new(machine);
    }
  } else if (strcmp(name, "fill") == 0) {
    command_fill(tokens, count);
  } else if (strcmp(name, "reserve") == 0) {
    if (count != 2) {
      fail("reserve expects one argument");
    }
    link_srts();
    uint64_t words = parse_unsigned(tokens[1]);
    ensure(words);
    reserved_words = words;
  } else if (strcmp(name, "new") == 0) {
    link_srts();
    command_new(tokens, count);
  } else if (strcmp(name, "array") == 0) {
    link_srts();
    command_array(tokens, count);
  } else if (strcmp(name, "set") == 0) {
    command_set(tokens, count);
  } else if (strcmp(name, "update") == 0) {
    if (count != 3) {
      fail("update expects two arguments");
    }
    AihcValue *target = parse_object(tokens[2]);
    Entry *entry = live_entry(parse_unsigned(tokens[1]));
    if (aihc_value_kind(entry->address) != AIHC_OBJECT_THUNK) {
      fail("update expects a thunk");
    }
    aihc_update(entry->address, target);
  } else if (strcmp(name, "blackhole") == 0) {
    if (count != 2) {
      fail("blackhole expects one argument");
    }
    aihc_begin_blackhole(machine,
                         live_entry(parse_unsigned(tokens[1]))->address);
  } else if (strcmp(name, "unblackhole") == 0) {
    if (count != 3) {
      fail("unblackhole expects two arguments");
    }
    AihcValue *target = parse_object(tokens[2]);
    Entry *entry = live_entry(parse_unsigned(tokens[1]));
    aihc_update_blackhole(machine, entry->address, target);
  } else if (strcmp(name, "supdate") == 0) {
    if (count != 3) {
      fail("supdate expects two arguments");
    }
    uint64_t slot = parse_unsigned(tokens[1]);
    if (slot >= STATIC_THUNKS) {
      fail("supdate expects a static thunk");
    }
    aihc_update((AihcValue *)&static_thunks[slot], parse_object(tokens[2]));
  } else if (strcmp(name, "sset") == 0) {
    if (count != 4) {
      fail("sset expects three arguments");
    }
    uint64_t slot = parse_unsigned(tokens[1]);
    uint64_t field = parse_unsigned(tokens[2]);
    if (slot < STATIC_THUNKS || slot >= STATIC_THUNKS + STATIC_NODES ||
        field >= STATIC_NODE_FIELDS) {
      fail("sset expects a static node field");
    }
    AihcSlot value = parse_pointer(tokens[3]);
    if (value != 0 &&
        !in_range((const void *)(uintptr_t)value,
                  (const uint8_t *)static_thunks, sizeof(static_thunks)) &&
        !in_range((const void *)(uintptr_t)value, (const uint8_t *)static_nodes,
                  sizeof(static_nodes)) &&
        !in_range((const void *)(uintptr_t)value,
                  (const uint8_t *)static_nullary, sizeof(static_nullary))) {
      fail("static node fields hold static objects only");
    }
    static_nodes[slot - STATIC_THUNKS].fields[field] = value;
  } else if (strcmp(name, "ssrt") == 0) {
    if (count != 3) {
      fail("ssrt expects two arguments");
    }
    link_srts();
    uint64_t slot = parse_unsigned(tokens[1]);
    const AihcSrt *srt = srt_of(parse_signed(tokens[2]));
    if (slot < STATIC_THUNKS) {
      static_thunk_info[slot].srt = srt;
    } else if (slot < STATIC_THUNKS + STATIC_NODES) {
      static_node_info[slot - STATIC_THUNKS].srt = srt;
    } else {
      fail("ssrt expects a static thunk or node");
    }
  } else if (strcmp(name, "global") == 0) {
    if (count != 3) {
      fail("global expects two arguments");
    }
    uint64_t index = parse_unsigned(tokens[1]);
    if (index >= machine->global_count) {
      fail("global index out of range");
    }
    machine->globals[index] = parse_pointer(tokens[2]);
  } else if (strcmp(name, "root") == 0) {
    if (count != 3) {
      fail("root expects two arguments");
    }
    uint64_t index = parse_unsigned(tokens[1]);
    if (index >= root_count) {
      fail("root index out of range");
    }
    root_slots[index] = parse_pointer(tokens[2]);
  } else if (strcmp(name, "stable") == 0) {
    if (count != 2) {
      fail("stable expects one argument");
    }
    (void)aihc_stable_name_make(machine, parse_object(tokens[1]));
  } else if (strcmp(name, "mvar_put") == 0) {
    if (count != 3) {
      fail("mvar_put expects two arguments");
    }
    uint64_t index = parse_unsigned(tokens[1]);
    if (index >= mvar_count) {
      fail("mvar index out of range");
    }
    mvars[index]->full = 1;
    mvars[index]->value = parse_pointer(tokens[2]);
  } else if (strcmp(name, "mvar_take") == 0) {
    if (count != 2) {
      fail("mvar_take expects one argument");
    }
    uint64_t index = parse_unsigned(tokens[1]);
    if (index >= mvar_count) {
      fail("mvar index out of range");
    }
    mvars[index]->full = 0;
    mvars[index]->value = 0;
  } else if (strcmp(name, "thread") == 0) {
    command_thread(tokens, count);
  } else if (strcmp(name, "collect") == 0) {
    if (count != 1) {
      fail("collect expects no arguments");
    }
    link_srts();
    ensure(remaining_words() + 1);
    reserved_words = 0;
  } else {
    fail("unknown command");
  }
}

static size_t tokenize(char *line, char **tokens) {
  size_t count = 0;
  char *cursor = line;
  while (*cursor != 0) {
    while (*cursor == ' ' || *cursor == '\t' || *cursor == '\n' ||
           *cursor == '\r') {
      ++cursor;
    }
    if (*cursor == 0) {
      break;
    }
    if (count == MAX_TOKENS) {
      fail("too many tokens");
    }
    tokens[count++] = cursor;
    while (*cursor != 0 && *cursor != ' ' && *cursor != '\t' &&
           *cursor != '\n' && *cursor != '\r') {
      ++cursor;
    }
    if (*cursor != 0) {
      *cursor = 0;
      ++cursor;
    }
  }
  return count;
}

int main(int argc, char *const argv[]) {
  aihc_program_arguments_initialize(argc, argv);
  initialize_statics();
  char *line = checked_calloc(LINE_CAPACITY, 1);
  char **tokens = checked_calloc(MAX_TOKENS, sizeof(*tokens));
  while (fgets(line, LINE_CAPACITY, stdin) != NULL) {
    size_t length = strlen(line);
    if (length + 1 == LINE_CAPACITY && line[length - 1] != '\n') {
      fail("line is too long");
    }
    size_t count = tokenize(line, tokens);
    if (count == 0) {
      continue;
    }
    if (strcmp(tokens[0], "end") == 0) {
      printf("done\n");
      fflush(stdout);
      command_index = 0;
      continue;
    }
    run_command(tokens, count);
    ++command_index;
  }
  free(line);
  free(tokens);
  return 0;
}
