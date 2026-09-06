#include "aihc_runtime.h"
#include "aihc_runtime_internal.h"

#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#if UINTPTR_MAX == UINT64_MAX
_Static_assert(offsetof(AihcMachine, globals) == 0, "machine globals ABI");
_Static_assert(offsetof(AihcMachine, heap_next) == 24, "machine heap-next ABI");
_Static_assert(offsetof(AihcMachine, heap_limit) == 32,
               "machine heap-limit ABI");
_Static_assert(offsetof(AihcMachine, exit_code) == 16, "machine exit-code ABI");
_Static_assert(offsetof(AihcInfo, remaining_arity) == 24,
               "info-table remaining-arity ABI");
_Static_assert(offsetof(AihcInfo, backend_entry) == 48,
               "info-table backend-entry ABI");
_Static_assert(offsetof(AihcInfo, frame_kind) == 56,
               "info-table frame-kind ABI");
_Static_assert(offsetof(AihcInfo, object_kind) == 64,
               "info-table object-kind ABI");
_Static_assert(offsetof(AihcInfo, srt) == 72, "info-table SRT ABI");
_Static_assert(sizeof(AihcInfo) == 80, "info-table size ABI");
_Static_assert(offsetof(AihcSrt, object_count) == 8, "SRT object-count ABI");
_Static_assert(offsetof(AihcSrt, child_count) == 16, "SRT child-count ABI");
_Static_assert(offsetof(AihcSrt, entries) == 24, "SRT entries ABI");
_Static_assert(offsetof(AihcResume, kind) == 0, "resume kind ABI");
_Static_assert(offsetof(AihcResume, function) == 8, "resume function ABI");
_Static_assert(offsetof(AihcResume, continuation) == 16,
               "resume continuation ABI");
_Static_assert(offsetof(AihcResume, value) == 24, "resume value ABI");
_Static_assert(offsetof(AihcResume, count) == 32, "resume count ABI");
_Static_assert(offsetof(AihcStableName, value) == 8, "stable-name value ABI");
_Static_assert(offsetof(AihcStableName, hash) == 16, "stable-name hash ABI");
_Static_assert(offsetof(AihcStableName, next) == 24, "stable-name next ABI");
_Static_assert(sizeof(AihcStableName) == 32, "stable-name size ABI");
#elif UINTPTR_MAX == UINT32_MAX
_Static_assert(offsetof(AihcMachine, exit_code) == 16, "machine exit-code ABI");
_Static_assert(offsetof(AihcInfo, remaining_arity) == 12,
               "info-table remaining-arity ABI");
_Static_assert(offsetof(AihcInfo, backend_entry) == 24,
               "info-table backend-entry ABI");
_Static_assert(offsetof(AihcInfo, frame_kind) == 28,
               "info-table frame-kind ABI");
_Static_assert(offsetof(AihcInfo, object_kind) == 32,
               "info-table object-kind ABI");
_Static_assert(offsetof(AihcInfo, srt) == 36, "info-table SRT ABI");
_Static_assert(sizeof(AihcInfo) == 40, "info-table size ABI");
_Static_assert(offsetof(AihcSrt, object_count) == 4, "SRT object-count ABI");
_Static_assert(offsetof(AihcSrt, child_count) == 8, "SRT child-count ABI");
_Static_assert(offsetof(AihcSrt, entries) == 12, "SRT entries ABI");
_Static_assert(offsetof(AihcResume, kind) == 0, "resume kind ABI");
_Static_assert(offsetof(AihcResume, function) == 8, "resume function ABI");
_Static_assert(offsetof(AihcResume, continuation) == 12,
               "resume continuation ABI");
_Static_assert(offsetof(AihcResume, value) == 16, "resume value ABI");
_Static_assert(offsetof(AihcResume, count) == 24, "resume count ABI");
_Static_assert(offsetof(AihcStableName, value) == 8, "stable-name value ABI");
_Static_assert(offsetof(AihcStableName, hash) == 16, "stable-name hash ABI");
_Static_assert(offsetof(AihcStableName, next) == 24, "stable-name next ABI");
_Static_assert(sizeof(AihcStableName) == 32, "stable-name size ABI");
#endif

const AihcSrt *aihc_current_srt = NULL;

_Noreturn void aihc_fail(const char *message) { aihc_host_fail(message); }

static const AihcResume *aihc_schedule(AihcMachine *machine);

static const uint8_t aihc_indirection_field_is_pointer[] = {1};
static const AihcInfo aihc_indirection_info = {
    .field_count = 1,
    .field_is_pointer = aihc_indirection_field_is_pointer,
    .frame_kind = AIHC_FRAME_NONE,
    .object_kind = AIHC_OBJECT_INDIRECTION,
};
static const AihcInfo aihc_thread_info = {
    .frame_kind = AIHC_FRAME_NONE,
    .object_kind = AIHC_OBJECT_THREAD,
};
const AihcInfo aihc_runtime_object_info = {
    .frame_kind = AIHC_FRAME_NONE,
    .object_kind = AIHC_OBJECT_RUNTIME,
};

void aihc_unsupported_primitive(void) {
  aihc_fail("primitive is not implemented by the native runtime");
}

void aihc_record_allocation(AihcMachine *machine) {
  if (machine->allocation_count == UINT64_MAX) {
    aihc_fail("allocation counter overflow");
  }
  ++machine->allocation_count;
}

/* The byte count is a uint64_t rather than a size_t so that the runtime units
   written in Lir can call this with an i64 on a 32-bit target as well. A
   request the address space cannot hold fails here rather than wrapping. */
void *aihc_allocate_zeroed(uint64_t bytes) {
  if (bytes > (uint64_t)SIZE_MAX) {
    aihc_fail("allocation is too large");
  }
  void *pointer = calloc(1, (size_t)bytes);
  if (pointer == NULL) {
    aihc_fail("out of memory");
  }
  return pointer;
}

void *aihc_allocate_auxiliary(AihcMachine *machine, uint64_t bytes) {
  void *pointer = aihc_allocate_zeroed(bytes);
  aihc_record_allocation(machine);
  return pointer;
}

static AihcSlot aihc_make_header(const AihcInfo *info) {
  if (info == NULL) {
    aihc_fail("info table is null");
  }
  switch (info->object_kind) {
  case AIHC_OBJECT_CLOSURE:
  case AIHC_OBJECT_THUNK:
  case AIHC_OBJECT_NODE:
  case AIHC_OBJECT_PARTIAL_CONSTRUCTOR:
    return (AihcSlot)(uintptr_t)info;
  default:
    aihc_fail("attempted to allocate an invalid object kind");
  }
}

uint64_t aihc_object_words(const AihcInfo *info) {
  uint64_t field_words = info->field_count;
  if (field_words == 0 && (info->object_kind == AIHC_OBJECT_THUNK ||
                           info->object_kind == AIHC_OBJECT_BLACKHOLE ||
                           info->object_kind == AIHC_OBJECT_INDIRECTION)) {
    field_words = 1;
  }
  return 1 + field_words;
}

uint64_t aihc_value_words(const AihcValue *value) {
  if (aihc_value_kind(value) == AIHC_OBJECT_ARRAY) {
    return 2 + aihc_array_length(value);
  }
  return aihc_object_words(aihc_value_info_table(value));
}

uint64_t aihc_array_length(const AihcValue *array) {
  if (array == NULL || aihc_value_kind(array) != AIHC_OBJECT_ARRAY) {
    aihc_fail("boxed-array primitive received a non-array");
  }
  return array->fields[0];
}

AihcSlot *aihc_array_elements(AihcValue *array) {
  (void)aihc_array_length(array);
  return array->fields + 1;
}

/* aihc_array_new, aihc_array_index, aihc_array_write, aihc_array_same, and
   the info table they share live in compiler/native/runtime/aihc_array.lir.
   aihc_array_length and aihc_array_elements stay here: the collector walks
   arrays through them, including the ones the GC fuzz harness builds with
   info tables of its own. */

/* aihc_mutvar_*, aihc_stable_name_*, the byte-array primitives, and the RTS
   option parser live in compiler/native/runtime/aihc_mutvar.lir,
   aihc_stable_name.lir, aihc_byte_array.lir, and aihc_runtime_options.lir.
   The two accessors below stay here: the offsets of the machine fields they
   reach follow the target word size, and a Lir unit is one file for every
   target. */

AihcStableName **aihc_stable_names(AihcMachine *machine) {
  return &machine->stable_names;
}

uint64_t aihc_stable_name_take_hash(AihcMachine *machine) {
  if (machine->next_stable_name > (uint64_t)INT64_MAX) {
    aihc_fail("stable-name counter overflow");
  }
  return machine->next_stable_name++;
}

void aihc_memory_copy(void *destination, const void *source, uint64_t length) {
  memcpy(destination, source, (size_t)length);
}

void aihc_memory_move(void *destination, const void *source, uint64_t length) {
  memmove(destination, source, (size_t)length);
}

void aihc_memory_free(void *pointer) { free(pointer); }

/* The RTS option parser and the argument store live in
   compiler/native/runtime/aihc_runtime_options.lir. This flattens argv for
   it: the width of a C pointer is the one thing a Lir unit does not know. */
void aihc_program_arguments_initialize(int argc, char *const argv[]) {
  if (argc < 0 || (argc != 0 && argv == NULL)) {
    aihc_fail("invalid initial program arguments");
  }
  size_t length = 0;
  for (int index = 0; index < argc; ++index) {
    if (argv[index] == NULL) {
      aihc_fail("null initial program argument");
    }
    size_t argument_length = strlen(argv[index]);
    if ((uint64_t)argument_length >= (uint64_t)INT64_MAX - (uint64_t)length) {
      aihc_fail("program arguments are too large");
    }
    length += argument_length + 1;
  }
  uint8_t *arguments = aihc_allocate_zeroed(length == 0 ? 1 : length);
  size_t offset = 0;
  for (int index = 0; index < argc; ++index) {
    size_t argument_length = strlen(argv[index]);
    memcpy(arguments + offset, argv[index], argument_length);
    offset += argument_length + 1;
  }
  if (aihc_runtime_arguments_initialize(arguments, (int64_t)length) != 0) {
    free(arguments);
    aihc_fail("invalid initial program arguments");
  }
  free(arguments);
}

static void aihc_visit_value(AihcValue **value, AihcRootVisitor visitor,
                             void *context) {
  *value =
      (AihcValue *)(uintptr_t)visitor((AihcSlot)(uintptr_t)*value, context);
}

static void aihc_visit_thread(AihcThread *thread, AihcRootVisitor visitor,
                              void *context) {
  if (thread == NULL) {
    return;
  }
  aihc_visit_value(&thread->resume_function, visitor, context);
  aihc_visit_value(&thread->resume_continuation, visitor, context);
  if ((thread->resume_kind == AIHC_RESUME_CONTINUE ||
       thread->resume_kind == AIHC_RESUME_APPLY) &&
      thread->resume_count == 1) {
    thread->resume_value = visitor(thread->resume_value, context);
  }
}

/* Static objects are not visited here. They never move, so a collector marks
   and scans the ones it finds reachable instead of treating all of them as
   roots. */
void aihc_visit_roots(AihcMachine *machine, uint64_t root_count,
                      AihcSlot *roots, AihcRootVisitor visitor, void *context) {
  for (uint64_t index = 0; index < machine->global_count; ++index) {
    machine->globals[index] = visitor(machine->globals[index], context);
  }
  for (uint64_t index = 0; index < root_count; ++index) {
    roots[index] = visitor(roots[index], context);
  }
  aihc_visit_value(&machine->thread_done_continuation, visitor, context);
  aihc_visit_value(&machine->selected_resume.function, visitor, context);
  aihc_visit_value(&machine->selected_resume.continuation, visitor, context);
  if ((machine->selected_resume.kind == AIHC_RESUME_CONTINUE ||
       machine->selected_resume.kind == AIHC_RESUME_APPLY) &&
      machine->selected_resume.count == 1) {
    machine->selected_resume.value =
        visitor(machine->selected_resume.value, context);
  }
  aihc_visit_thread(machine->current_thread, visitor, context);
  for (AihcThread *thread = machine->run_queue_head; thread != NULL;
       thread = thread->next) {
    aihc_visit_thread(thread, visitor, context);
  }
  for (AihcBlackhole *blackhole = machine->blackholes; blackhole != NULL;
       blackhole = blackhole->next) {
    aihc_visit_value(&blackhole->object, visitor, context);
    for (AihcBlackholeWaiter *waiter = blackhole->waiters_head; waiter != NULL;
         waiter = waiter->next) {
      aihc_visit_value(&waiter->continuation, visitor, context);
      aihc_visit_thread(waiter->thread, visitor, context);
    }
  }
  for (AihcMVar *mvar = machine->mvars; mvar != NULL; mvar = mvar->next) {
    if (mvar->full) {
      mvar->value = visitor(mvar->value, context);
    }
    AihcMVarWaiter *waiter_lists[] = {mvar->readers_head, mvar->takers_head,
                                      mvar->putters_head};
    for (size_t list = 0; list < 3; ++list) {
      for (AihcMVarWaiter *waiter = waiter_lists[list]; waiter != NULL;
           waiter = waiter->next) {
        if (list == 2) {
          waiter->value = visitor(waiter->value, context);
        }
        aihc_visit_value(&waiter->continuation, visitor, context);
        aihc_visit_thread(waiter->thread, visitor, context);
      }
    }
  }
  for (AihcStableName *name = machine->stable_names; name != NULL;
       name = name->next) {
    aihc_visit_value(&name->value, visitor, context);
  }
  for (AihcIoRequest *request = machine->io_requests_head; request != NULL;
       request = request->next) {
    aihc_visit_value(&request->continuation, visitor, context);
    aihc_visit_thread(request->thread, visitor, context);
  }
}

void aihc_ensure_heap(AihcMachine *machine, uint64_t words, uint64_t root_count,
                      AihcSlot *roots) {
  aihc_gc_ensure(machine, words, root_count, roots);
}

AihcValue *aihc_make_node_unchecked(AihcMachine *machine,
                                    const AihcInfo *info) {
  uint64_t words = aihc_object_words(info);
  AihcValue *value = aihc_gc_allocate(machine, words);
  aihc_record_allocation(machine);
  value->header = aihc_make_header(info);
  return value;
}

AihcValue *aihc_make_node(AihcMachine *machine, const AihcInfo *info) {
  uint64_t words = aihc_object_words(info);
  aihc_ensure_heap(machine, words, 0, NULL);
  return aihc_make_node_unchecked(machine, info);
}

uint64_t aihc_allocation_count(const AihcMachine *machine) {
  return machine->allocation_count;
}

void aihc_reset_allocation_count(AihcMachine *machine) {
  machine->allocation_count = 0;
}

const AihcInfo *aihc_next_application_info(const AihcInfo *info,
                                           uint64_t supplied_count) {
  const AihcInfo *next = info->next;
  if (info->remaining_arity == 0 || next == NULL ||
      next->remaining_arity + 1 != info->remaining_arity ||
      next->field_count < info->field_count ||
      next->field_count - info->field_count != supplied_count) {
    aihc_fail("application does not match static info-table transition");
  }
  return next;
}

static AihcValue *aihc_copy_with_fields(AihcMachine *machine,
                                        AihcValue **value_pointer,
                                        uint64_t count, const AihcSlot *fields,
                                        AihcValue **continuation_pointer) {
  AihcValue *value = *value_pointer;
  const AihcInfo *info = aihc_value_info_table(value);
  const AihcInfo *next_info = aihc_next_application_info(info, count);
  uint64_t original_count = info->field_count;

  uint64_t pointer_count = 0;
  for (uint64_t index = 0; index < count; ++index) {
    if (next_info->field_is_pointer[original_count + index]) {
      ++pointer_count;
    }
  }
  AihcSlot roots[2 + pointer_count];
  roots[0] = (AihcSlot)value;
  roots[1] = (AihcSlot)*continuation_pointer;
  uint64_t root_index = 2;
  for (uint64_t index = 0; index < count; ++index) {
    if (next_info->field_is_pointer[original_count + index]) {
      roots[root_index++] = fields[index];
    }
  }

  aihc_ensure_heap(machine, aihc_object_words(next_info), 2 + pointer_count,
                   roots);
  value = (AihcValue *)roots[0];
  *value_pointer = value;
  *continuation_pointer = (AihcValue *)roots[1];

  AihcValue *copy = aihc_make_node_unchecked(machine, next_info);
  AihcSlot *original_fields = aihc_value_fields(value);
  AihcSlot *copy_fields = aihc_value_fields(copy);
  for (uint64_t index = 0; index < original_count; ++index) {
    copy_fields[index] = original_fields[index];
  }
  root_index = 2;
  for (uint64_t index = 0; index < count; ++index) {
    copy_fields[original_count + index] =
        next_info->field_is_pointer[original_count + index]
            ? roots[root_index++]
            : fields[index];
  }
  return copy;
}

static AihcThread *aihc_thread_new(AihcMachine *machine) {
  AihcThread *thread = aihc_allocate_auxiliary(machine, sizeof(*thread));
  thread->header = (AihcSlot)(uintptr_t)&aihc_thread_info;
  return thread;
}

static void aihc_enqueue_thread(AihcMachine *machine, AihcThread *thread) {
  if (thread->next != NULL) {
    aihc_fail("attempted to enqueue an already queued thread");
  }
  if (machine->run_queue_tail == NULL) {
    machine->run_queue_head = thread;
  } else {
    machine->run_queue_tail->next = thread;
  }
  machine->run_queue_tail = thread;
}

static AihcThread *aihc_dequeue_thread(AihcMachine *machine) {
  AihcThread *thread = machine->run_queue_head;
  if (thread == NULL) {
    aihc_fail("no runnable threads");
  }
  machine->run_queue_head = thread->next;
  if (machine->run_queue_head == NULL) {
    machine->run_queue_tail = NULL;
  }
  thread->next = NULL;
  return thread;
}

static AihcBlackhole *aihc_find_blackhole(AihcMachine *machine,
                                          AihcValue *object) {
  for (AihcBlackhole *blackhole = machine->blackholes; blackhole != NULL;
       blackhole = blackhole->next) {
    if (blackhole->object == object) {
      return blackhole;
    }
  }
  AihcBlackhole *blackhole =
      aihc_allocate_auxiliary(machine, sizeof(*blackhole));
  blackhole->object = object;
  blackhole->owner = machine->current_thread;
  blackhole->next = machine->blackholes;
  machine->blackholes = blackhole;
  return blackhole;
}

static void aihc_add_blackhole_waiter(AihcMachine *machine, AihcValue *object,
                                      AihcValue *continuation) {
  AihcBlackhole *blackhole = aihc_find_blackhole(machine, object);
  if (blackhole->owner == machine->current_thread) {
    aihc_fail("blackholed thunk re-entered");
  }
  AihcBlackholeWaiter *waiter =
      aihc_allocate_auxiliary(machine, sizeof(*waiter));
  waiter->thread = machine->current_thread;
  waiter->continuation = continuation;
  if (blackhole->waiters_tail == NULL) {
    blackhole->waiters_head = waiter;
  } else {
    blackhole->waiters_tail->next = waiter;
  }
  blackhole->waiters_tail = waiter;
}

static AihcBlackhole *aihc_remove_blackhole(AihcMachine *machine,
                                            AihcValue *object) {
  AihcBlackhole **link = &machine->blackholes;
  while (*link != NULL && (*link)->object != object) {
    link = &(*link)->next;
  }
  if (*link == NULL) {
    return NULL;
  }
  AihcBlackhole *blackhole = *link;
  *link = blackhole->next;
  return blackhole;
}

void aihc_set_field(AihcValue *value, uint64_t index, AihcSlot field) {
  aihc_value_fields(value)[index] = field;
}

void aihc_set_exit_status(AihcMachine *machine, int64_t status) {
  machine->exit_status = status;
}

int64_t aihc_get_exit_status(const AihcMachine *machine) {
  return machine->exit_status;
}

AihcMachine *aihc_machine_new(uint64_t global_count) {
  AihcMachine *machine = aihc_allocate_zeroed(sizeof(*machine));
  machine->allocation_count = 1;
  machine->heap_max_bytes = aihc_rts_heap_max_bytes();
  machine->heap_limit_enabled = aihc_rts_heap_limit_enabled() != 0;
  machine->global_count = global_count;
  machine->globals = aihc_allocate_auxiliary(
      machine,
      sizeof(*machine->globals) * (global_count == 0 ? 1 : global_count));
  machine->next_stable_name = 1;
  aihc_gc_init(machine);
  machine->current_thread = aihc_thread_new(machine);
  machine->io_backend = aihc_host_io_backend();
  return machine;
}

void aihc_no_match(void) { aihc_fail("no matching case alternative"); }

AihcValue *aihc_apply_slow(AihcMachine *machine, AihcValue *function,
                           uint64_t count, const AihcSlot *arguments,
                           AihcValue **continuation) {
  if (function == NULL) {
    aihc_fail("attempted to apply null");
  }
  while (aihc_value_kind(function) == AIHC_OBJECT_INDIRECTION) {
    function = (AihcValue *)(uintptr_t)function->fields[0];
    if (function == NULL) {
      aihc_fail("indirection points to null");
    }
  }
  switch (aihc_value_kind(function)) {
  case AIHC_OBJECT_CLOSURE: {
    uint64_t arity = aihc_value_arity(function);
    if (arity <= 1) {
      aihc_fail("closure application does not require the slow path");
    }
    return aihc_copy_with_fields(machine, &function, count, arguments,
                                 continuation);
  }
  case AIHC_OBJECT_PARTIAL_CONSTRUCTOR: {
    uint64_t arity = aihc_value_arity(function);
    if (arity == 0) {
      aihc_fail("saturated constructor was applied");
    }
    return aihc_copy_with_fields(machine, &function, count, arguments,
                                 continuation);
  }
  default:
    aihc_fail("attempted to apply a non-function value");
  }
}

static void aihc_suspend_apply(AihcThread *thread, AihcValue *function,
                               AihcValue *continuation) {
  thread->resume_kind = AIHC_RESUME_APPLY;
  thread->resume_function = function;
  thread->resume_continuation = continuation;
  thread->resume_count = 0;
}

static void aihc_suspend_raise(AihcThread *thread, AihcValue *exception,
                               AihcValue *continuation) {
  thread->resume_kind = AIHC_RESUME_RAISE;
  thread->resume_function = exception;
  thread->resume_continuation = continuation;
  thread->resume_count = 0;
}

static void aihc_suspend_continue(AihcThread *thread, AihcValue *continuation,
                                  uint64_t count, AihcSlot value) {
  if (count > 1) {
    aihc_fail("suspended continuation has too many immediate values");
  }
  thread->resume_kind = AIHC_RESUME_CONTINUE;
  thread->resume_function = continuation;
  thread->resume_continuation = NULL;
  thread->resume_value = value;
  thread->resume_count = count;
}

static const AihcResume *aihc_select_thread(AihcMachine *machine,
                                            AihcThread *thread) {
  AihcResume *resume = &machine->selected_resume;
  resume->kind = thread->resume_kind;
  resume->function = thread->resume_function;
  resume->continuation = thread->resume_continuation;
  resume->value = thread->resume_value;
  resume->count = thread->resume_count;
  thread->resume_kind = AIHC_RESUME_NONE;
  thread->resume_function = NULL;
  thread->resume_continuation = NULL;
  thread->resume_value = 0;
  thread->resume_count = 0;
  machine->current_thread = thread;
  if (resume->kind != AIHC_RESUME_APPLY &&
      resume->kind != AIHC_RESUME_CONTINUE &&
      resume->kind != AIHC_RESUME_RAISE) {
    aihc_fail("thread has no suspended continuation");
  }
  return resume;
}

int64_t aihc_io_error(int error) { return -((int64_t)error) - 1; }

void aihc_resume_io_request(AihcMachine *machine, AihcIoRequest *request,
                            int64_t result) {
  AihcThread *thread = request->thread;
  AihcValue *continuation = request->continuation;
  request->state = AIHC_IO_COMPLETED;
  request->result = result;
  request->thread = NULL;
  request->continuation = NULL;
  request->next = NULL;
  aihc_suspend_continue(thread, continuation, 0, 0);
  aihc_enqueue_thread(machine, thread);
}

static const AihcResume *aihc_schedule(AihcMachine *machine) {
  for (;;) {
    (void)machine->io_backend->poll(machine, 0);
    if (machine->run_queue_head != NULL) {
      return aihc_select_thread(machine, aihc_dequeue_thread(machine));
    }
    if (machine->io_request_count != 0) {
      if (machine->io_backend->poll(machine, 1) == AIHC_IO_POLL_SUSPENDED) {
        return NULL;
      }
      continue;
    }
    aihc_fail("no runnable threads");
  }
}

static AihcIoRequest *aihc_io_submit(AihcIoKind kind, AihcIoHandle *handle,
                                     uint8_t *buffer, int64_t offset,
                                     int64_t length) {
  AihcIoRequest *request = aihc_allocate_zeroed(sizeof(*request));
  request->kind = kind;
  request->state = AIHC_IO_SUBMITTED;
  request->handle = handle;
  if (handle == NULL || handle->closed) {
    request->state = AIHC_IO_COMPLETED;
    request->result = aihc_io_error(AIHC_IO_ERROR_BAD_DESCRIPTOR);
    return request;
  }
  if (offset < 0 || length < 0 || (uint64_t)offset > SIZE_MAX ||
      (uint64_t)length > SIZE_MAX - (size_t)offset ||
      (buffer == NULL && length != 0)) {
    request->state = AIHC_IO_COMPLETED;
    request->result = aihc_io_error(AIHC_IO_ERROR_INVALID_ARGUMENT);
    return request;
  }
  uint32_t required_capability =
      kind == AIHC_IO_READ ? AIHC_IO_READABLE : AIHC_IO_WRITABLE;
  if ((handle->capabilities & required_capability) == 0) {
    request->state = AIHC_IO_COMPLETED;
    request->result = aihc_io_error(AIHC_IO_ERROR_BAD_DESCRIPTOR);
    return request;
  }
  request->buffer = buffer;
  request->offset = (size_t)offset;
  request->length = (size_t)length;
  return request;
}

static AihcIoRequest *aihc_io_submit_open_request(uint8_t *path,
                                                  int64_t requested_length,
                                                  int64_t requested_mode) {
  AihcIoRequest *request = aihc_allocate_zeroed(sizeof(*request));
  request->kind = AIHC_IO_OPEN;
  request->state = AIHC_IO_SUBMITTED;
  if (requested_length < 0 || (uint64_t)requested_length > SIZE_MAX ||
      (path == NULL && requested_length != 0) || requested_mode < 0 ||
      requested_mode > 3) {
    request->state = AIHC_IO_COMPLETED;
    request->result =
        (int64_t)(uintptr_t)aihc_io_open_error(AIHC_IO_ERROR_INVALID_ARGUMENT);
    return request;
  }
  request->buffer = path;
  request->length = (size_t)requested_length;
  request->mode = requested_mode;
  return request;
}

void *aihc_io_open_error(int error) {
  return (void *)((((uintptr_t)error) << 1) | (uintptr_t)1);
}

int64_t aihc_io_open_result_error(void *result) {
  uintptr_t encoded = (uintptr_t)result;
  return (encoded & (uintptr_t)1) == 0 ? 0 : (int64_t)(encoded >> 1);
}

int64_t aihc_memory_write_byte(void *opaque_buffer, int64_t offset,
                               int64_t value) {
  if (opaque_buffer == NULL || offset < 0 || value < 0 || value > UINT8_MAX) {
    return aihc_io_error(AIHC_IO_ERROR_INVALID_ARGUMENT);
  }
  ((uint8_t *)opaque_buffer)[(size_t)offset] = (uint8_t)value;
  return 0;
}

int64_t aihc_memory_read_byte(const void *opaque_buffer, int64_t offset) {
  if (opaque_buffer == NULL || offset < 0) {
    return aihc_io_error(AIHC_IO_ERROR_INVALID_ARGUMENT);
  }
  const uint8_t *buffer = opaque_buffer;
  return buffer[offset];
}

void *aihc_io_submit_read(void *opaque_handle, void *opaque_buffer,
                          int64_t offset, int64_t length) {
  return aihc_io_submit(AIHC_IO_READ, opaque_handle, opaque_buffer, offset,
                        length);
}

void *aihc_io_submit_write(void *opaque_handle, void *opaque_buffer,
                           int64_t offset, int64_t length) {
  return aihc_io_submit(AIHC_IO_WRITE, opaque_handle, opaque_buffer, offset,
                        length);
}

void *aihc_io_submit_open(void *opaque_path, int64_t length, int64_t mode) {
  return aihc_io_submit_open_request(opaque_path, length, mode);
}

int64_t aihc_io_take_result(void *opaque_request) {
  AihcIoRequest *request = opaque_request;
  if (request == NULL || request->state != AIHC_IO_COMPLETED) {
    aihc_fail("attempted to consume an incomplete IO request");
  }
  int64_t result = request->result;
  request->state = AIHC_IO_CONSUMED;
  free(request);
  return result;
}

void *aihc_io_take_open_result(void *opaque_request) {
  return (void *)(uintptr_t)aihc_io_take_result(opaque_request);
}

static const AihcResume *aihc_resume_current(AihcMachine *machine,
                                             AihcValue *continuation) {
  aihc_suspend_continue(machine->current_thread, continuation, 0, 0);
  return aihc_select_thread(machine, machine->current_thread);
}

static const AihcResume *aihc_resume_current_value(AihcMachine *machine,
                                                   AihcValue *continuation,
                                                   AihcSlot value) {
  aihc_suspend_continue(machine->current_thread, continuation, 1, value);
  return aihc_select_thread(machine, machine->current_thread);
}

static AihcMVarWaiter *aihc_mvar_waiter_new(AihcMachine *machine,
                                            AihcValue *continuation,
                                            AihcSlot value) {
  AihcMVarWaiter *waiter = aihc_allocate_auxiliary(machine, sizeof(*waiter));
  waiter->thread = machine->current_thread;
  waiter->continuation = continuation;
  waiter->value = value;
  return waiter;
}

static void aihc_mvar_append_waiter(AihcMVarWaiter **head,
                                    AihcMVarWaiter **tail,
                                    AihcMVarWaiter *waiter) {
  if (*tail == NULL) {
    *head = waiter;
  } else {
    (*tail)->next = waiter;
  }
  *tail = waiter;
}

static AihcMVarWaiter *aihc_mvar_pop_waiter(AihcMVarWaiter **head,
                                            AihcMVarWaiter **tail) {
  AihcMVarWaiter *waiter = *head;
  if (waiter == NULL) {
    return NULL;
  }
  *head = waiter->next;
  if (*head == NULL) {
    *tail = NULL;
  }
  waiter->next = NULL;
  return waiter;
}

static void aihc_mvar_wake(AihcMachine *machine, AihcMVarWaiter *waiter,
                           uint64_t count, AihcSlot value) {
  aihc_suspend_continue(waiter->thread, waiter->continuation, count, value);
  aihc_enqueue_thread(machine, waiter->thread);
  free(waiter);
}

static AihcMVar *aihc_checked_mvar(void *opaque_mvar) {
  AihcMVar *mvar = opaque_mvar;
  if (mvar == NULL) {
    aihc_fail("attempted an operation on a null MVar");
  }
  return mvar;
}

void *aihc_mvar_new(AihcMachine *machine) {
  AihcMVar *mvar = aihc_allocate_auxiliary(machine, sizeof(*mvar));
  mvar->header = (AihcSlot)(uintptr_t)&aihc_runtime_object_info;
  mvar->next = machine->mvars;
  machine->mvars = mvar;
  return mvar;
}

const AihcResume *aihc_mvar_read(AihcMachine *machine, void *opaque_mvar,
                                 AihcValue *continuation) {
  AihcMVar *mvar = aihc_checked_mvar(opaque_mvar);
  if (mvar->full) {
    return aihc_resume_current_value(machine, continuation, mvar->value);
  }
  AihcMVarWaiter *waiter = aihc_mvar_waiter_new(machine, continuation, 0);
  aihc_mvar_append_waiter(&mvar->readers_head, &mvar->readers_tail, waiter);
  return aihc_schedule(machine);
}

const AihcResume *aihc_mvar_take(AihcMachine *machine, void *opaque_mvar,
                                 AihcValue *continuation) {
  AihcMVar *mvar = aihc_checked_mvar(opaque_mvar);
  if (!mvar->full) {
    AihcMVarWaiter *waiter = aihc_mvar_waiter_new(machine, continuation, 0);
    aihc_mvar_append_waiter(&mvar->takers_head, &mvar->takers_tail, waiter);
    return aihc_schedule(machine);
  }

  AihcSlot value = mvar->value;
  AihcMVarWaiter *putter =
      aihc_mvar_pop_waiter(&mvar->putters_head, &mvar->putters_tail);
  if (putter == NULL) {
    mvar->full = 0;
    mvar->value = 0;
  } else {
    mvar->value = putter->value;
    aihc_mvar_wake(machine, putter, 0, 0);
  }
  return aihc_resume_current_value(machine, continuation, value);
}

const AihcResume *aihc_mvar_put(AihcMachine *machine, void *opaque_mvar,
                                AihcSlot value, AihcValue *continuation) {
  AihcMVar *mvar = aihc_checked_mvar(opaque_mvar);
  if (mvar->full) {
    AihcMVarWaiter *waiter = aihc_mvar_waiter_new(machine, continuation, value);
    aihc_mvar_append_waiter(&mvar->putters_head, &mvar->putters_tail, waiter);
    return aihc_schedule(machine);
  }

  AihcMVarWaiter *reader;
  while ((reader = aihc_mvar_pop_waiter(&mvar->readers_head,
                                        &mvar->readers_tail)) != NULL) {
    aihc_mvar_wake(machine, reader, 1, value);
  }
  AihcMVarWaiter *taker =
      aihc_mvar_pop_waiter(&mvar->takers_head, &mvar->takers_tail);
  if (taker == NULL) {
    mvar->full = 1;
    mvar->value = value;
  } else {
    aihc_mvar_wake(machine, taker, 1, value);
  }
  return aihc_resume_current(machine, continuation);
}

const AihcResume *aihc_await_io(AihcMachine *machine, void *opaque_request,
                                AihcValue *continuation) {
  AihcIoRequest *request = opaque_request;
  if (request == NULL) {
    aihc_fail("attempted to await a null IO request");
  }
  if (request->state == AIHC_IO_COMPLETED) {
    return aihc_resume_current(machine, continuation);
  }
  if (request->state != AIHC_IO_SUBMITTED) {
    aihc_fail("attempted to await an IO request more than once");
  }

  int error = machine->io_backend->prepare(request);
  if (error != 0) {
    request->state = AIHC_IO_COMPLETED;
    request->result = aihc_io_error(error);
    return aihc_resume_current(machine, continuation);
  }

  int64_t result = 0;
  if (machine->io_backend->try_request(request, &result)) {
    request->state = AIHC_IO_COMPLETED;
    request->result = machine->io_backend->finish_request(request, result);
    return aihc_resume_current(machine, continuation);
  }

  request->state = AIHC_IO_PENDING;
  request->thread = machine->current_thread;
  request->continuation = continuation;
  if (machine->io_requests_tail == NULL) {
    machine->io_requests_head = request;
  } else {
    machine->io_requests_tail->next = request;
  }
  machine->io_requests_tail = request;
  ++machine->io_request_count;
  return aihc_schedule(machine);
}

void aihc_begin_blackhole(AihcMachine *machine, AihcValue *value) {
  if (value == NULL || aihc_value_kind(value) != AIHC_OBJECT_THUNK) {
    aihc_fail("attempted to blackhole a non-thunk value");
  }
  const AihcInfo *original_info = aihc_value_info_table(value);
  AihcBlackhole *blackhole = aihc_find_blackhole(machine, value);
  blackhole->original_info = original_info;
  blackhole->info = *original_info;
  blackhole->info.object_kind = AIHC_OBJECT_BLACKHOLE;
  value->header = (AihcSlot)(uintptr_t)&blackhole->info;
}

const AihcResume *aihc_block_on_blackhole(AihcMachine *machine,
                                          AihcValue *value,
                                          AihcValue *continuation) {
  if (value == NULL || aihc_value_kind(value) != AIHC_OBJECT_BLACKHOLE) {
    aihc_fail("attempted to block on a value that is not blackholed");
  }
  aihc_add_blackhole_waiter(machine, value, continuation);
  return aihc_schedule(machine);
}

const AihcResume *aihc_complete_io(AihcMachine *machine, int64_t result) {
  AihcIoRequest *request = machine->io_requests_head;
  if (request == NULL) {
    aihc_fail("IO completion has no pending request");
  }
  machine->io_requests_head = request->next;
  --machine->io_request_count;
  int64_t request_result = machine->io_backend->finish_request(request, result);
  aihc_resume_io_request(machine, request, request_result);

  while (machine->io_requests_head != NULL) {
    request = machine->io_requests_head;
    if (!machine->io_backend->try_request(request, &request_result)) {
      break;
    }
    request_result =
        machine->io_backend->finish_request(request, request_result);
    machine->io_requests_head = request->next;
    --machine->io_request_count;
    aihc_resume_io_request(machine, request, request_result);
  }
  if (machine->io_requests_head == NULL) {
    machine->io_requests_tail = NULL;
  }
  return aihc_schedule(machine);
}

void aihc_update(AihcValue *object, AihcValue *value) {
  if (object == NULL || value == NULL) {
    aihc_fail("attempted to update with null");
  }
  object->fields[0] = (AihcSlot)value;
  object->header = (AihcSlot)(uintptr_t)&aihc_indirection_info;
  aihc_gc_note_update(object);
}

void aihc_update_blackhole(AihcMachine *machine, AihcValue *object,
                           AihcValue *value) {
  if (object == NULL || aihc_value_kind(object) != AIHC_OBJECT_BLACKHOLE) {
    aihc_fail("attempted to update a cell that is not blackholed");
  }
  AihcBlackhole *blackhole = aihc_remove_blackhole(machine, object);
  if (blackhole == NULL) {
    aihc_fail("blackholed object has no scheduler record");
  }
  aihc_update(object, value);
  AihcBlackholeWaiter *waiter = blackhole->waiters_head;
  while (waiter != NULL) {
    AihcBlackholeWaiter *next = waiter->next;
    aihc_suspend_continue(waiter->thread, waiter->continuation, 1,
                          (AihcSlot)value);
    aihc_enqueue_thread(machine, waiter->thread);
    free(waiter);
    waiter = next;
  }
  free(blackhole);
}

static void aihc_abandon_blackhole(AihcMachine *machine, AihcValue *object,
                                   AihcValue *exception) {
  if (object == NULL || aihc_value_kind(object) != AIHC_OBJECT_BLACKHOLE) {
    aihc_fail("exception update frame does not contain a blackhole");
  }
  AihcBlackhole *blackhole = aihc_remove_blackhole(machine, object);
  if (blackhole == NULL) {
    aihc_fail("blackholed object has no scheduler record");
  }
  object->header = (AihcSlot)(uintptr_t)blackhole->original_info;
  AihcBlackholeWaiter *waiter = blackhole->waiters_head;
  while (waiter != NULL) {
    AihcBlackholeWaiter *next = waiter->next;
    aihc_suspend_raise(waiter->thread, exception, waiter->continuation);
    aihc_enqueue_thread(machine, waiter->thread);
    free(waiter);
    waiter = next;
  }
  free(blackhole);
}

const AihcResume *aihc_raise(AihcMachine *machine, AihcValue *exception,
                             AihcValue *continuation) {
  if (exception == NULL) {
    aihc_fail("attempted to raise a null exception");
  }
  for (;;) {
    if (continuation == NULL ||
        aihc_value_kind(continuation) != AIHC_OBJECT_CLOSURE) {
      aihc_fail("exception chain contains a non-continuation value");
    }
    const AihcInfo *info = aihc_value_info_table(continuation);
    const AihcSlot *fields = aihc_value_fields_const(continuation);
    switch (info->frame_kind) {
    case AIHC_FRAME_NORMAL:
      if (info->field_count < 1) {
        aihc_fail("normal continuation has no parent");
      }
      continuation = (AihcValue *)(uintptr_t)fields[0];
      break;
    case AIHC_FRAME_CATCH: {
      if (info->field_count < 2) {
        aihc_fail("catch continuation has an invalid layout");
      }
      AihcResume *resume = &machine->selected_resume;
      resume->kind = AIHC_RESUME_APPLY;
      resume->function = (AihcValue *)(uintptr_t)fields[1];
      resume->continuation = (AihcValue *)(uintptr_t)fields[0];
      resume->value = (AihcSlot)(uintptr_t)exception;
      resume->count = 1;
      return resume;
    }
    case AIHC_FRAME_UPDATE:
      if (info->field_count < 2) {
        aihc_fail("update continuation has an invalid layout");
      }
      aihc_abandon_blackhole(machine, (AihcValue *)(uintptr_t)fields[1],
                             exception);
      continuation = (AihcValue *)(uintptr_t)fields[0];
      break;
    case AIHC_FRAME_STOP:
      aihc_fail("uncaught Haskell exception");
    case AIHC_FRAME_RESTORE_MASK:
      aihc_fail("restore-mask continuation is not implemented");
    default:
      aihc_fail("exception chain contains a non-frame closure");
    }
  }
}

AihcSlot aihc_fork(AihcMachine *machine, AihcValue *action) {
  if (machine->thread_done_continuation == NULL) {
    aihc_fail("thread completion continuation is not initialized");
  }
  AihcThread *child = aihc_thread_new(machine);
  aihc_suspend_apply(child, action, machine->thread_done_continuation);
  aihc_enqueue_thread(machine, child);
  return (AihcSlot)child;
}

const AihcResume *aihc_yield(AihcMachine *machine, AihcValue *continuation) {
  AihcThread *current = machine->current_thread;
  aihc_suspend_continue(current, continuation, 0, 0);
  aihc_enqueue_thread(machine, current);
  return aihc_schedule(machine);
}

const AihcResume *aihc_thread_done(AihcMachine *machine) {
  return aihc_schedule(machine);
}

void aihc_set_thread_done_continuation(AihcMachine *machine,
                                       AihcValue *thread_done_continuation) {
  if (thread_done_continuation == NULL ||
      aihc_value_kind(thread_done_continuation) != AIHC_OBJECT_CLOSURE ||
      aihc_value_arity(thread_done_continuation) != 1) {
    aihc_fail("invalid thread completion continuation");
  }
  machine->thread_done_continuation = thread_done_continuation;
}

AihcEntry aihc_halt(AihcMachine *machine) { return machine->exit_code; }
