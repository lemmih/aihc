#include "aihc_runtime_internal.h"

#include <stdlib.h>
#include <string.h>

/* The collector copies live objects between two spaces. The current space is
   described by heap_start, heap_next, and heap_limit. The other space and its
   capacity wait in other_space and other_space_bytes. semispace_bytes is the
   capacity that the next collection gives the destination space. It doubles
   until it holds twice the live data, so the program does not collect on every
   allocation when its live data grows. The -M limit caps that capacity. */

typedef struct {
  AihcMachine *machine;
  uint8_t *from_start;
  size_t from_bytes;
} AihcForwardingContext;

/* Static objects never move, so the collector does not copy them. It has to
   decide instead which of them are live. An evaluated CAF is an indirection
   into the managed heap, so treating every static object as a root would keep
   everything any CAF has ever produced. A static object is live when a static
   reference table names it, when a table reachable from a live object or from
   the running function names it, or when a live object points at it.

   The collector has no list of static objects. It finds them by address: a
   pointer that is outside both spaces of the managed heap names an object
   that never moves, and every such object carries an info table. Each
   collection records the objects it marks in an open-addressed hash set and
   scans each one once through its info table, so an evaluated CAF gets its
   target forwarded like any heap field.

   An evaluated CAF that only code references is reachable through no pointer.
   aihc_update therefore records every object outside the heap that becomes an
   indirection in a second set that lives for the whole program. By default a
   collection marks all of them. Under -Zs the reference tables decide. */
typedef struct {
  AihcValue **slots;
  size_t capacity;
  size_t count;
} AihcAddressSet;

typedef struct {
  AihcValue **items;
  size_t count;
  size_t capacity;
} AihcStaticWorklist;

typedef struct {
  const AihcSrt **items;
  size_t count;
  size_t capacity;
} AihcSrtWorklist;

static AihcAddressSet aihc_marked_statics;
static AihcAddressSet aihc_updated_statics;
static AihcMachine *aihc_gc_machine;
static AihcStaticWorklist aihc_static_worklist;
static AihcSrtWorklist aihc_srt_worklist;
/* Terminates the list of tables this collection has walked. Tables form a
   cyclic graph across recursive functions, so each one is stamped once and the
   whole list is cleared when the collection ends. */
static AihcSrt aihc_srt_list_end;
static AihcSrt *aihc_srt_stamped;

static size_t aihc_static_slot_of(uintptr_t address, size_t capacity) {
  /* Object addresses are word-aligned, so the low bits carry no information. */
  uintptr_t mixed = address >> 3;
  mixed ^= mixed >> 17;
  mixed *= (uintptr_t)0x27d4eb2dU;
  mixed ^= mixed >> 15;
  return (size_t)mixed & (capacity - 1);
}

static void aihc_address_set_grow(AihcAddressSet *set) {
  size_t capacity = set->capacity == 0 ? 64 : set->capacity * 2;
  if (capacity > SIZE_MAX / sizeof(*set->slots) / 2) {
    aihc_fail("static object set is too large");
  }
  AihcValue **slots = aihc_allocate_zeroed(sizeof(*slots) * capacity);
  for (size_t index = 0; index < set->capacity; ++index) {
    AihcValue *object = set->slots[index];
    if (object == NULL) {
      continue;
    }
    size_t slot = aihc_static_slot_of((uintptr_t)object, capacity);
    while (slots[slot] != NULL) {
      slot = (slot + 1) & (capacity - 1);
    }
    slots[slot] = object;
  }
  free(set->slots);
  set->slots = slots;
  set->capacity = capacity;
}

/* Add one address to a set. Returns whether the address was new. */
static int aihc_address_set_insert(AihcAddressSet *set, AihcValue *object) {
  if (set->count * 2 >= set->capacity) {
    aihc_address_set_grow(set);
  }
  size_t slot = aihc_static_slot_of((uintptr_t)object, set->capacity);
  while (set->slots[slot] != NULL) {
    if (set->slots[slot] == object) {
      return 0;
    }
    slot = (slot + 1) & (set->capacity - 1);
  }
  set->slots[slot] = object;
  ++set->count;
  return 1;
}

static void aihc_address_set_clear(AihcAddressSet *set) {
  if (set->count != 0) {
    memset(set->slots, 0, sizeof(*set->slots) * set->capacity);
  }
  set->count = 0;
}

static void *aihc_worklist_grow(void *items, size_t *capacity,
                                size_t item_bytes) {
  size_t next = *capacity == 0 ? 16 : *capacity * 2;
  if (next > SIZE_MAX / item_bytes) {
    aihc_fail("collector worklist is too large");
  }
  void *grown = realloc(items, next * item_bytes);
  if (grown == NULL) {
    aihc_fail("out of memory");
  }
  *capacity = next;
  return grown;
}

/* Mark one object outside the heap and queue it for scanning. The caller
   has established that the address is outside both spaces. */
static void aihc_mark_static(AihcValue *object) {
  if (object == NULL ||
      !aihc_address_set_insert(&aihc_marked_statics, object)) {
    return;
  }
  if (aihc_static_worklist.count == aihc_static_worklist.capacity) {
    aihc_static_worklist.items = aihc_worklist_grow(
        aihc_static_worklist.items, &aihc_static_worklist.capacity,
        sizeof(*aihc_static_worklist.items));
  }
  aihc_static_worklist.items[aihc_static_worklist.count++] = object;
}

static void aihc_walk_srt(const AihcSrt *srt) {
  if (srt == NULL || srt->walked != NULL) {
    return;
  }
  AihcSrt *stamped = (AihcSrt *)(uintptr_t)srt;
  stamped->walked =
      aihc_srt_stamped == NULL ? &aihc_srt_list_end : aihc_srt_stamped;
  aihc_srt_stamped = stamped;
  if (aihc_srt_worklist.count == aihc_srt_worklist.capacity) {
    aihc_srt_worklist.items =
        aihc_worklist_grow(aihc_srt_worklist.items, &aihc_srt_worklist.capacity,
                           sizeof(*aihc_srt_worklist.items));
  }
  aihc_srt_worklist.items[aihc_srt_worklist.count++] = srt;
}

static void aihc_clear_srt_stamps(void) {
  AihcSrt *stamped = aihc_srt_stamped;
  while (stamped != NULL) {
    AihcSrt *next =
        stamped->walked == &aihc_srt_list_end ? NULL : stamped->walked;
    stamped->walked = NULL;
    stamped = next;
  }
  aihc_srt_stamped = NULL;
}

static int aihc_in_space(const uint8_t *start, size_t bytes,
                         const AihcValue *value) {
  uintptr_t address = (uintptr_t)value;
  uintptr_t first = (uintptr_t)start;
  return address >= first && address - first < bytes;
}

static size_t aihc_semispace_capacity(const AihcMachine *machine) {
  return (size_t)(machine->heap_limit - machine->heap_start);
}

static uint8_t *aihc_semispace_new(size_t bytes) {
  uint8_t *space = malloc(bytes == 0 ? 1 : bytes);
  if (space == NULL) {
    aihc_fail("out of memory");
  }
  return space;
}

static _Noreturn void aihc_semispace_exhausted(const AihcMachine *machine) {
  if (machine->heap_limit_enabled) {
    aihc_fail("heap limit exceeded");
  }
  aihc_fail("live data exceeds semispace");
}

/* Copy one object or return where it already went. Heap indirections are
   not copied: the collector follows them and returns their target, so the new
   space holds no indirection and no chain grows across collections. */
static AihcValue *aihc_forward(AihcForwardingContext *context,
                               AihcValue *value) {
  AihcMachine *machine = context->machine;
  for (;;) {
    if (value == NULL) {
      return value;
    }
    if (!aihc_in_space(context->from_start, context->from_bytes, value)) {
      /* Anything outside from-space is either already copied or an object
         that never moves. Such an object stays where it is, but a live one
         still has to be scanned: an evaluated CAF holds a heap pointer. */
      if (!aihc_in_space(machine->heap_start, aihc_semispace_capacity(machine),
                         value)) {
        aihc_mark_static(value);
      }
      return value;
    }
    AihcValue *forwarded = (AihcValue *)(uintptr_t)value->header;
    if (aihc_in_space(machine->heap_start, aihc_semispace_capacity(machine),
                      forwarded)) {
      return forwarded;
    }
    if (aihc_value_kind(value) != AIHC_OBJECT_INDIRECTION) {
      break;
    }
    value = (AihcValue *)(uintptr_t)value->fields[0];
  }

  uint64_t words = aihc_value_words(value);
  size_t bytes = sizeof(AihcSlot) * words;
  if (bytes > (size_t)(machine->heap_limit - machine->heap_next)) {
    aihc_semispace_exhausted(machine);
  }
  AihcValue *copy = (AihcValue *)machine->heap_next;
  memcpy(copy, value, bytes);
  machine->heap_next += bytes;
  value->header = (AihcSlot)(uintptr_t)copy;
  return copy;
}

static AihcSlot aihc_forward_root(AihcSlot root, void *opaque_context) {
  return (AihcSlot)(uintptr_t)aihc_forward(opaque_context,
                                           (AihcValue *)(uintptr_t)root);
}

/* Scan one object wherever it lives. The object's info table also names the
   static objects its code reaches, so tracing an object walks that table. */
static void aihc_scan_object(AihcForwardingContext *context,
                             AihcValue *object) {
  const AihcInfo *info = aihc_value_info_table(object);
  AihcObjectKind kind = info->object_kind;
  uint64_t count = info->field_count;
  if (kind == AIHC_OBJECT_RUNTIME || kind == AIHC_OBJECT_THREAD) {
    /* Runtime objects hold no heap pointers of their own. The scheduler
       visits the resume record of a thread as a root. */
    return;
  }
  aihc_walk_srt(info->srt);
  if (kind == AIHC_OBJECT_INDIRECTION) {
    /* Only a static object reaches this branch: an evaluated CAF keeps its
       indirection because it cannot move. Heap indirections are never
       copied, so they are never scanned. */
    object->fields[0] = aihc_forward_root(object->fields[0], context);
  } else if (kind == AIHC_OBJECT_ARRAY) {
    uint64_t length = aihc_array_length(object);
    AihcSlot *elements = aihc_array_elements(object);
    for (uint64_t index = 0; index < length; ++index) {
      elements[index] = aihc_forward_root(elements[index], context);
    }
  } else if (kind == AIHC_OBJECT_PARTIAL_CONSTRUCTOR) {
    /* Field zero holds the applied count, and the slots filled so far are a
       prefix of the saturated constructor's, so the shared bitmap answers
       for them. */
    uint64_t applied = aihc_partial_applied(object);
    AihcSlot *fields = aihc_partial_fields(object);
    for (uint64_t index = 0; index < applied; ++index) {
      if (info->field_is_pointer != NULL && info->field_is_pointer[index]) {
        fields[index] = aihc_forward_root(fields[index], context);
      }
    }
  } else if (kind == AIHC_OBJECT_NODE || kind == AIHC_OBJECT_CLOSURE ||
             kind == AIHC_OBJECT_THUNK || kind == AIHC_OBJECT_BLACKHOLE) {
    for (uint64_t index = 0; index < count; ++index) {
      if (info->field_is_pointer != NULL && info->field_is_pointer[index]) {
        object->fields[index] =
            aihc_forward_root(object->fields[index], context);
      }
    }
  } else {
    aihc_fail("collector encountered an invalid object kind");
  }
}

/* Copying, static marking, and table walking all feed one another: a table
   names static objects, a static object holds heap pointers, and a copied
   object names further tables. Run all three to quiescence. */
static void aihc_trace(AihcForwardingContext *context) {
  AihcMachine *machine = context->machine;
  uint8_t *scan = machine->heap_start;
  for (;;) {
    if (aihc_srt_worklist.count != 0) {
      const AihcSrt *srt = aihc_srt_worklist.items[--aihc_srt_worklist.count];
      for (uintptr_t index = 0; index < srt->object_count; ++index) {
        aihc_mark_static((AihcValue *)srt->entries[index]);
      }
      for (uintptr_t index = 0; index < srt->child_count; ++index) {
        aihc_walk_srt((const AihcSrt *)srt->entries[srt->object_count + index]);
      }
      continue;
    }
    if (aihc_static_worklist.count != 0) {
      aihc_scan_object(
          context, aihc_static_worklist.items[--aihc_static_worklist.count]);
      continue;
    }
    if (scan < machine->heap_next) {
      AihcValue *object = (AihcValue *)scan;
      aihc_scan_object(context, object);
      scan += sizeof(AihcSlot) * aihc_value_words(object);
      continue;
    }
    return;
  }
}

/* Select the capacity of the destination space. Live data never exceeds the
   used part of the source space, so that size plus the pending reservation
   always fits unless the -M limit forbids it. */
static size_t aihc_destination_bytes(const AihcMachine *machine,
                                     size_t required_bytes) {
  size_t used = (size_t)(machine->heap_next - machine->heap_start);
  if (required_bytes > SIZE_MAX - used) {
    aihc_fail("heap reservation is too large");
  }
  size_t bytes = used + required_bytes;
  if (bytes < machine->semispace_bytes) {
    bytes = machine->semispace_bytes;
  }
  if (machine->heap_limit_enabled && bytes > machine->heap_max_bytes) {
    bytes = (size_t)machine->heap_max_bytes;
  }
  return bytes;
}

/* Double the target capacity until it holds twice the live data. */
static void aihc_grow_target(AihcMachine *machine, size_t occupied_bytes) {
  size_t target = machine->semispace_bytes;
  if (target == 0) {
    target = 1;
  }
  while (target < occupied_bytes || target - occupied_bytes < occupied_bytes) {
    if (target > SIZE_MAX / 2) {
      target = SIZE_MAX;
      break;
    }
    target *= 2;
  }
  if (machine->heap_limit_enabled && target > machine->heap_max_bytes) {
    target = (size_t)machine->heap_max_bytes;
  }
  machine->semispace_bytes = target;
}

static void aihc_collect(AihcMachine *machine, size_t required_bytes,
                         uint64_t root_count, AihcSlot *roots) {
  uint8_t *from_start = machine->heap_start;
  size_t from_bytes = aihc_semispace_capacity(machine);
  size_t to_bytes = aihc_destination_bytes(machine, required_bytes);
  if (machine->other_space == NULL || machine->other_space_bytes < to_bytes) {
    free(machine->other_space);
    machine->other_space = aihc_semispace_new(to_bytes);
    machine->other_space_bytes = to_bytes;
  }
  uint8_t *to_start = machine->other_space;
  machine->heap_start = to_start;
  machine->heap_next = to_start;
  machine->heap_limit = to_start + machine->other_space_bytes;

  AihcForwardingContext context = {machine, from_start, from_bytes};
  aihc_address_set_clear(&aihc_marked_statics);
  aihc_static_worklist.count = 0;
  aihc_srt_worklist.count = 0;
  if (aihc_rts_static_reference_roots()) {
    /* The running function has no heap object of its own to carry its table,
       so it publishes one on entry. Suspended code is a continuation closure
       and reaches its table through its info table like any other object. */
    aihc_walk_srt(aihc_current_srt);
  } else {
    /* Every evaluated static object stays alive. The tables do not yet name
       everything a running program reaches, so this remains the default. */
    for (size_t slot = 0; slot < aihc_updated_statics.capacity; ++slot) {
      if (aihc_updated_statics.slots[slot] != NULL) {
        aihc_mark_static(aihc_updated_statics.slots[slot]);
      }
    }
  }
  aihc_visit_roots(machine, root_count, roots, aihc_forward_root, &context);
  aihc_trace(&context);
  aihc_clear_srt_stamps();

  machine->other_space = from_start;
  machine->other_space_bytes = from_bytes;
  size_t live_bytes = (size_t)(machine->heap_next - machine->heap_start);
  if (required_bytes > (size_t)(machine->heap_limit - machine->heap_next)) {
    aihc_semispace_exhausted(machine);
  }
  aihc_grow_target(machine, live_bytes + required_bytes);
}

void aihc_gc_init(AihcMachine *machine) {
  aihc_gc_machine = machine;
  machine->semispace_bytes = AIHC_SEMISPACE_BYTES;
  if (machine->heap_limit_enabled &&
      machine->semispace_bytes > machine->heap_max_bytes) {
    machine->semispace_bytes = machine->heap_max_bytes;
  }
  machine->heap_start = aihc_semispace_new(machine->semispace_bytes);
  machine->heap_next = machine->heap_start;
  machine->heap_limit = machine->heap_start + machine->semispace_bytes;
  machine->other_space = NULL;
  machine->other_space_bytes = 0;
}

void aihc_gc_note_update(AihcValue *object) {
  AihcMachine *machine = aihc_gc_machine;
  if (machine != NULL &&
      aihc_in_space(machine->heap_start, aihc_semispace_capacity(machine),
                    object)) {
    return;
  }
  (void)aihc_address_set_insert(&aihc_updated_statics, object);
}

void aihc_gc_ensure(AihcMachine *machine, uint64_t words, uint64_t root_count,
                    AihcSlot *roots) {
  if (words > SIZE_MAX / sizeof(AihcSlot)) {
    aihc_fail("heap reservation is too large");
  }
  size_t bytes = sizeof(AihcSlot) * words;
  if (machine->heap_limit_enabled && bytes > machine->heap_max_bytes) {
    aihc_fail("heap limit exceeded");
  }
  if (bytes > (size_t)(machine->heap_limit - machine->heap_next)) {
    aihc_collect(machine, bytes, root_count, roots);
  }
}

AihcValue *aihc_gc_allocate(AihcMachine *machine, uint64_t words) {
  if (words > SIZE_MAX / sizeof(AihcSlot)) {
    aihc_fail("heap allocation is too large");
  }
  size_t bytes = sizeof(AihcSlot) * words;
  if (bytes > (size_t)(machine->heap_limit - machine->heap_next)) {
    aihc_fail("unchecked allocation exceeded reserved heap");
  }
  AihcValue *value = (AihcValue *)machine->heap_next;
  machine->heap_next += bytes;
  memset(value, 0, bytes);
  return value;
}
