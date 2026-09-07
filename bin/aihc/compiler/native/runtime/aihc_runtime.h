#ifndef AIHC_RUNTIME_H
#define AIHC_RUNTIME_H

#include <stdint.h>

#ifndef AIHC_SEMISPACE_BYTES
#define AIHC_SEMISPACE_BYTES (UINT64_C(1024) * UINT64_C(1024))
#endif

enum {
  AIHC_OBJECT_NODE,
  AIHC_OBJECT_CLOSURE,
  AIHC_OBJECT_THUNK,
  AIHC_OBJECT_PARTIAL_CONSTRUCTOR,
  AIHC_OBJECT_INDIRECTION,
  AIHC_OBJECT_BLACKHOLE,
  AIHC_OBJECT_ARRAY,
  AIHC_OBJECT_THREAD,
  /* An object the runtime allocates outside the managed heap: a byte array,
     an MVar, or a stable name. Compiled code stores its address in pointer
     fields, so it carries a header like every other object, but it never
     moves and holds no heap pointers the collector has to update. */
  AIHC_OBJECT_RUNTIME,
};
typedef uintptr_t AihcObjectKind;

typedef struct AihcValue AihcValue;
typedef struct AihcMachine AihcMachine;
typedef struct AihcInfo AihcInfo;
typedef struct AihcSrt AihcSrt;
typedef struct AihcThread AihcThread;
typedef struct AihcBlackhole AihcBlackhole;
typedef struct AihcIoHandle AihcIoHandle;
typedef struct AihcIoRequest AihcIoRequest;
typedef struct AihcIoBackend AihcIoBackend;
typedef struct AihcMVar AihcMVar;
typedef struct AihcStableName AihcStableName;
typedef uint64_t AihcSlot;
/* The portable entry of an info table. Reserved: Lir stores null until the
   runtime moves to Lir. The exit code of a machine has this type. */
typedef void (*AihcEntry)(AihcMachine *machine);
/* The backend entry is a Lir function with the signature
   (machine, object, continuation, supplied values...). Common runtime code
   preserves it but never calls it. */
typedef void (*AihcBackendEntry)(void);

enum {
  AIHC_RESUME_NONE,
  AIHC_RESUME_APPLY,
  AIHC_RESUME_CONTINUE,
  AIHC_RESUME_RAISE,
};
typedef uint64_t AihcResumeKind;

typedef struct {
  AihcResumeKind kind;
  AihcValue *function;
  AihcValue *continuation;
  AihcSlot value;
  uint64_t count;
} AihcResume;

enum {
  AIHC_FRAME_NONE = 0,
  AIHC_FRAME_NORMAL = 1,
  AIHC_FRAME_CATCH = 2,
  AIHC_FRAME_UPDATE = 3,
  AIHC_FRAME_RESTORE_MASK = 4,
  AIHC_FRAME_STOP = 5,
};
typedef uintptr_t AihcFrameKind;

/* A static reference table names the static objects one function reaches
   without going through a heap object. Tables are chained: a table names the
   tables of the functions its code calls directly, so the collector walks the
   graph instead of the compiler flattening it.

   Backends emit one word-uniform record per table: the walk link, the two
   counts, then object_count static object addresses followed by child_count
   table addresses. The link is mutable, so tables belong in a writable
   section even though info tables themselves are read-only. */
struct AihcSrt {
  /* Null until a collection walks this table, then the next walked table.
     Recursive functions make the table graph cyclic, so the collector links
     every table it walks and clears the whole list when the collection
     ends. */
  AihcSrt *walked;
  uintptr_t object_count;
  uintptr_t child_count;
  uintptr_t entries[];
};

/* Every field of an info table is one word wide, so Lir addresses field k
   at offset k words on every target. See the "Info tables" section of
   docs/lir.md. */
struct AihcInfo {
  uintptr_t identity;
  AihcEntry entry;
  /* The slots an object of this info table holds. A partial constructor is
     the exception: every stage of one constructor shares a single info table,
     so the count of the slots filled so far lives in the object and this word
     is zero. See aihc_value_count. */
  uintptr_t field_count;
  uintptr_t remaining_arity;
  /* One byte per slot of the saturated object. A partial constructor indexes
     the same array as the finished one: the slots it has filled are a prefix
     of the slots the saturated constructor holds. */
  const uint8_t *field_is_pointer;
  /* The info table of the saturated constructor, for a partial constructor.
     Null for every other kind. */
  const AihcInfo *next;
  /* Backend-owned dynamic entry. Lir gives this word its own callable
     type. */
  AihcBackendEntry backend_entry;
  /* Continuation closures have their parent in field zero. This kind is
     backend-independent so the runtime can unwind them uniformly. */
  AihcFrameKind frame_kind;
  AihcObjectKind object_kind;
  /* The static objects this object's code reaches, or null when it reaches
     none. The collector marks them whenever it traces the object. */
  const AihcSrt *srt;
};

struct AihcValue {
  /* Ordinarily an unmodified info-table pointer. During semispace collection,
     a forwarded from-space object temporarily holds its to-space address. */
  AihcSlot header;
  /* A partial constructor and a boxed array both spend field zero on a count
     and hold their payload from field one. Reach that payload through
     aihc_partial_fields and aihc_array_elements rather than by hand. */
  AihcSlot fields[];
};

struct AihcMachine {
  AihcSlot *globals;
  uint64_t global_count;
  AihcEntry exit_code;
  uint8_t *heap_next;
  uint8_t *heap_limit;
  uint8_t *heap_start;
  uint8_t *other_space;
  uint64_t semispace_bytes;
  uint64_t heap_max_bytes;
  uint64_t heap_allocated_bytes;
  uint8_t heap_limit_enabled;
  AihcValue *thread_done_continuation;
  AihcThread *current_thread;
  AihcThread *run_queue_head;
  AihcThread *run_queue_tail;
  AihcBlackhole *blackholes;
  AihcMVar *mvars;
  AihcStableName *stable_names;
  uint64_t next_stable_name;
  AihcIoRequest *io_requests_head;
  AihcIoRequest *io_requests_tail;
  uint64_t io_request_count;
  const AihcIoBackend *io_backend;
  uint64_t allocation_count;
  AihcResume selected_resume;
  int64_t exit_status;
  uint64_t other_space_bytes;
};

_Static_assert(sizeof(AihcValue) == sizeof(AihcSlot),
               "AIHC objects must have a one-word base header");

/* Portable Lir access to pointer-sized runtime records. */
uint64_t aihc_lir_info_kind(const AihcInfo *info);
uint64_t aihc_lir_info_arity(const AihcInfo *info);
uint64_t aihc_lir_info_count(const AihcInfo *info);
AihcBackendEntry aihc_lir_info_entry(const AihcInfo *info);
const AihcInfo *aihc_lir_info_next(const AihcInfo *info);
const uint8_t *aihc_lir_info_bitmap(const AihcInfo *info);
void aihc_lir_take_resume(AihcResume *resume, uint64_t *slots);

static inline const AihcInfo *aihc_value_info_table(const AihcValue *value) {
  return (const AihcInfo *)(uintptr_t)value->header;
}

static inline AihcObjectKind aihc_value_kind(const AihcValue *value) {
  return aihc_value_info_table(value)->object_kind;
}

static inline uintptr_t aihc_value_info(const AihcValue *value) {
  return aihc_value_info_table(value)->identity;
}

static inline AihcEntry aihc_value_entry(const AihcValue *value) {
  return aihc_value_info_table(value)->entry;
}

static inline uint64_t aihc_value_arity(const AihcValue *value) {
  return aihc_value_info_table(value)->remaining_arity;
}

/* The slots a partial constructor has filled. It lives in the object because
   every stage of one constructor shares a single info table. */
static inline uint64_t aihc_partial_applied(const AihcValue *value) {
  return value->fields[0];
}

/* The slots a partial constructor still needs. The saturated info table names
   the full width. */
static inline uint64_t aihc_partial_total(const AihcValue *value) {
  return aihc_value_info_table(value)->next->field_count;
}

static inline AihcSlot *aihc_partial_fields(AihcValue *value) {
  return value->fields + 1;
}

static inline const AihcSlot *
aihc_partial_fields_const(const AihcValue *value) {
  return value->fields + 1;
}

static inline uint64_t aihc_value_count(const AihcValue *value) {
  if (aihc_value_kind(value) == AIHC_OBJECT_PARTIAL_CONSTRUCTOR) {
    return aihc_partial_applied(value);
  }
  return aihc_value_info_table(value)->field_count;
}

static inline AihcSlot *aihc_value_fields(AihcValue *value) {
  return value->fields;
}

static inline const AihcSlot *aihc_value_fields_const(const AihcValue *value) {
  return value->fields;
}

/* The static reference table of the function the machine is running. Compiled
   functions store their own table here on entry, so a collection triggered
   anywhere inside a function - at one of its safepoints or inside a runtime
   helper it called - still sees the static objects that function's code can
   reach. Continuations that are merely suspended need no entry here: they are
   heap objects whose info tables carry their tables. */
extern const AihcSrt *aihc_current_srt;

AihcValue *aihc_make_node(AihcMachine *machine, const AihcInfo *info);
AihcValue *aihc_make_node_unchecked(AihcMachine *machine, const AihcInfo *info);
AihcValue *aihc_make_partial(AihcMachine *machine, const AihcInfo *info,
                             uint64_t applied);
AihcValue *aihc_make_partial_unchecked(AihcMachine *machine,
                                       const AihcInfo *info, uint64_t applied);
void aihc_ensure_heap(AihcMachine *machine, uint64_t words, uint64_t root_count,
                      AihcSlot *roots);
AihcMachine *aihc_machine_new(uint64_t global_count);
uint64_t aihc_allocation_count(const AihcMachine *machine);
void aihc_reset_allocation_count(AihcMachine *machine);
void aihc_no_match(void);
void aihc_unsupported_primitive(void);
/* The runtime removes RTS options before the Haskell machine starts. argv[0]
   stays because getProgName and withProgName use the same mutable vector.
   aihc_program_arguments_initialize flattens argv into one buffer of
   zero-terminated strings; the parser and the argument store behind the other
   four functions live in compiler/native/runtime/aihc_runtime_options.lir. */
void aihc_program_arguments_initialize(int argc, char *const argv[]);
int64_t aihc_runtime_arguments_initialize(const void *buffer, int64_t length);
int64_t aihc_program_arguments_size(void);
int64_t aihc_program_arguments_copy(void *buffer, int64_t capacity);
int64_t aihc_program_arguments_replace(const void *buffer, int64_t length);
void aihc_set_field(AihcValue *value, uint64_t index, AihcSlot field);
/* Boxed arrays are contiguous managed objects. GrinEnsureHeap reserves their
   length-dependent storage before this initializer advances the heap.

   The boxed arrays, the mutable references, the stable names, and the byte
   arrays below are defined by the Lir runtime units in
   compiler/native/runtime. See the "Runtime units" section of docs/lir.md. */
AihcValue *aihc_array_new(AihcMachine *machine, int64_t count,
                          AihcSlot initial);
AihcSlot aihc_array_index(AihcValue *array, int64_t index);
AihcSlot aihc_array_write(AihcValue *array, int64_t index, AihcSlot value);
uint64_t aihc_array_same(AihcValue *left, AihcValue *right);
AihcValue *aihc_mutvar_new(AihcMachine *machine, AihcSlot initial);
AihcSlot aihc_mutvar_read(AihcValue *mutvar);
AihcSlot aihc_mutvar_write(AihcValue *mutvar, AihcSlot value);
uint64_t aihc_mutvar_compare_and_swap(AihcValue *mutvar, AihcSlot expected,
                                      AihcSlot replacement);
uint64_t aihc_mutvar_same(AihcValue *left, AihcValue *right);
/* Stable-name handles are auxiliary, non-moving objects. The machine-owned
   table keeps their referents synchronized with a moving collector. */
void *aihc_stable_name_make(AihcMachine *machine, AihcValue *value);
uint64_t aihc_stable_name_equal(const void *left, const void *right);
int64_t aihc_stable_name_hash(const void *name);
/* State and allocation helpers used by native code. None of these functions
   transfers control to a generated user function. */
AihcValue *aihc_apply_slow(AihcMachine *machine, AihcValue *function,
                           uint64_t count, const AihcSlot *arguments,
                           AihcValue **continuation);
void aihc_begin_blackhole(AihcMachine *machine, AihcValue *value);
const AihcResume *aihc_block_on_blackhole(AihcMachine *machine,
                                          AihcValue *value,
                                          AihcValue *continuation);
void aihc_update(AihcValue *object, AihcValue *value);
void aihc_update_blackhole(AihcMachine *machine, AihcValue *object,
                           AihcValue *value);
const AihcResume *aihc_raise(AihcMachine *machine, AihcValue *exception,
                             AihcValue *continuation);
AihcSlot aihc_fork(AihcMachine *machine, AihcValue *action);
void *aihc_mvar_new(AihcMachine *machine);
const AihcResume *aihc_mvar_read(AihcMachine *machine, void *mvar,
                                 AihcValue *continuation);
const AihcResume *aihc_mvar_take(AihcMachine *machine, void *mvar,
                                 AihcValue *continuation);
const AihcResume *aihc_mvar_put(AihcMachine *machine, void *mvar,
                                AihcSlot value, AihcValue *continuation);
const AihcResume *aihc_yield(AihcMachine *machine, AihcValue *continuation);
const AihcResume *aihc_await_io(AihcMachine *machine, void *request,
                                AihcValue *continuation);
const AihcResume *aihc_thread_done(AihcMachine *machine);
void *aihc_io_stdin(void);
void *aihc_io_stdout(void);
void *aihc_io_stderr(void);
int64_t aihc_io_open_result_error(void *result);
int64_t aihc_io_close(void *handle);
int64_t aihc_memory_write_byte(void *buffer, int64_t offset, int64_t value);
int64_t aihc_memory_read_byte(const void *buffer, int64_t offset);
_Noreturn int64_t aihc_io_raise_error(int64_t error);
uint64_t aihc_byte_array_copy_to_addr(void *opaque_array, int64_t offset,
                                      void *destination, int64_t length);
uint64_t aihc_byte_array_compare(void *opaque_left, int64_t left_offset,
                                 void *opaque_right, int64_t right_offset,
                                 int64_t length);
/* Proof-of-concept byte arrays use stable auxiliary allocations and are not
   released. Freeze and thaw are representation-preserving compiler
   primitives. */
void *aihc_byte_array_new(int64_t size);
void *aihc_byte_array_new_pinned(int64_t size);
void *aihc_byte_array_new_aligned_pinned(int64_t size, int64_t alignment);
uint64_t aihc_byte_array_is_pinned(void *array);
void *aihc_byte_array_contents(void *array);
uint64_t aihc_byte_array_shrink(void *array, int64_t size);
void *aihc_byte_array_resize(void *array, int64_t size);
uint64_t aihc_byte_array_get_size(void *array);
uint64_t aihc_byte_array_copy_from_addr(void *source, void *array,
                                        int64_t offset, int64_t length);
uint64_t aihc_byte_array_index_word(void *array, int64_t index);
uint64_t aihc_byte_array_read_word(void *array, int64_t index);
uint64_t aihc_byte_array_write_word(void *array, int64_t index, uint64_t value);
uint64_t aihc_byte_array_copy(void *source, int64_t source_offset,
                              void *destination, int64_t destination_offset,
                              int64_t length);
uint64_t aihc_byte_array_index_byte_word8(void *opaque_array, int64_t offset);
uint64_t aihc_byte_array_index_byte_word16(void *opaque_array, int64_t offset);
uint64_t aihc_byte_array_index_byte_word32(void *opaque_array, int64_t offset);
uint64_t aihc_byte_array_index_byte_word64(void *opaque_array, int64_t offset);
void *aihc_io_submit_read(void *handle, void *buffer, int64_t offset,
                          int64_t length);
void *aihc_io_submit_write(void *handle, void *buffer, int64_t offset,
                           int64_t length);
void *aihc_io_submit_open(void *path, int64_t length, int64_t mode);
int64_t aihc_io_take_result(void *request);
void *aihc_io_take_open_result(void *request);
void aihc_set_thread_done_continuation(AihcMachine *machine,
                                       AihcValue *thread_done_continuation);
void aihc_set_exit_status(AihcMachine *machine, int64_t status);
int64_t aihc_get_exit_status(const AihcMachine *machine);
_Noreturn void aihc_exit_process(int64_t status);
AihcEntry aihc_halt(AihcMachine *machine);
#endif
