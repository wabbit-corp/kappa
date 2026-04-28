#include <stdint.h>
#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct KValue KValue;
typedef KValue* (*KappaFunction)(void* env, KValue** args, int argc);
typedef struct KActionResult KActionResult;
typedef struct KContinuation KContinuation;
typedef KActionResult* (*KappaIoExecutor)(void* env);
typedef KActionResult* (*KappaContinuationResume)(KContinuation* continuation, KValue* value);

typedef enum KValueTag
{
    K_TAG_INT = 1,
    K_TAG_FLOAT = 2,
    K_TAG_BOOL = 3,
    K_TAG_STRING = 4,
    K_TAG_CHAR = 5,
    K_TAG_UNIT = 6,
    K_TAG_DATA = 7,
    K_TAG_CLOSURE = 8,
    K_TAG_REF = 9,
    K_TAG_DICTIONARY = 10,
    K_TAG_ARRAY = 11,
    K_TAG_IO_ACTION = 12,
    K_TAG_EFFECT_LABEL = 13,
    K_TAG_EFFECT_OPERATION = 14,
    K_TAG_RESUMPTION = 15
} KValueTag;

typedef struct KDataValue
{
    int type_id;
    int ctor_tag;
    int field_count;
    KValue** fields;
} KDataValue;

typedef struct KClosureValue
{
    KappaFunction function;
    void* env;
    int arity;
    const char* debug_name;
    int applied_count;
    KValue** applied_args;
} KClosureValue;

typedef struct KRefValue
{
    KValue* value;
} KRefValue;

typedef struct KDictionaryValue
{
    const char* module_name;
    const char* trait_name;
    const char* instance_key;
} KDictionaryValue;

typedef struct KArrayValue
{
    int length;
    KValue** items;
} KArrayValue;

typedef struct KIoActionValue
{
    KappaIoExecutor execute;
    void* env;
} KIoActionValue;

typedef struct KappaEffectOperationMetadata
{
    const char* operation_id;
    const char* name;
    int parameter_arity;
    int allows_multiple_resumptions;
} KappaEffectOperationMetadata;

typedef struct KEffectLabelValue
{
    const char* name;
    const char* interface_id;
    const char* label_id;
    int operation_count;
    KappaEffectOperationMetadata* operations;
} KEffectLabelValue;

typedef struct KEffectOperationValue
{
    KValue* label;
    KappaEffectOperationMetadata* operation;
    int applied_count;
    KValue** applied_args;
} KEffectOperationValue;

typedef struct KResumptionValue
{
    KContinuation* continuation;
    int allows_multiple_resumptions;
    int consumed;
    const char* display_name;
} KResumptionValue;

typedef enum KActionResultTag
{
    K_ACTION_RETURN = 1,
    K_ACTION_EFFECT_REQUEST = 2
} KActionResultTag;

typedef struct KActionReturn
{
    KValue* value;
} KActionReturn;

typedef struct KActionEffectRequest
{
    KValue* label;
    KappaEffectOperationMetadata* operation;
    int argument_count;
    KValue** arguments;
    KContinuation* continuation;
} KActionEffectRequest;

struct KActionResult
{
    KActionResultTag tag;
    union
    {
        KActionReturn action_return;
        KActionEffectRequest effect_request;
    } as;
};

struct KContinuation
{
    KappaContinuationResume resume;
};

typedef struct KReturnContinuation
{
    KContinuation base;
} KReturnContinuation;

typedef struct KBindContinuation
{
    KContinuation base;
    KContinuation* next;
    KValue* binder;
} KBindContinuation;

typedef struct KHandleContinuation
{
    KContinuation base;
    int is_deep;
    KValue* label;
    KValue* return_clause;
    KValue* dispatcher;
    KContinuation* next;
} KHandleContinuation;

struct KValue
{
    KValueTag tag;
    union
    {
        int64_t int_value;
        double float_value;
        int bool_value;
        const char* string_value;
        int32_t char_value;
        KDataValue data_value;
        KClosureValue closure_value;
        KRefValue ref_value;
        KDictionaryValue dictionary_value;
        KArrayValue array_value;
        KIoActionValue io_action_value;
        KEffectLabelValue effect_label_value;
        KEffectOperationValue effect_operation_value;
        KResumptionValue resumption_value;
    } as;
};

typedef struct KappaPanicFrame KappaPanicFrame;

struct KappaPanicFrame
{
    jmp_buf env;
    KappaPanicFrame* previous;
};

static KappaPanicFrame* kappa_panic_frame = NULL;
static const char* kappa_current_panic_message = NULL;

__KAPPA_TYPE_ID_ENUM__

static KActionResult* kappa_bind_action_result(KActionResult* step, KValue* binder);
static KActionResult* kappa_handle_action_result(int is_deep, KValue* label, KValue* return_clause, KValue* dispatcher, KActionResult* step);
static KValue* kappa_execute_action_result(KActionResult* step);
static KValue* kappa_apply_value(KValue* callee, KValue** args, int argc);
static KValue* kappa_expect_effect_label(KValue* value);
static KValue* kappa_expect_io_action(KValue* value);
static void kappa_expect_unit(KValue* value);

static void* kappa_alloc(size_t size)
{
    void* memory = malloc(size);
    if (memory == NULL)
    {
        fprintf(stderr, "runtime error: out of memory\n");
        exit(1);
    }

    return memory;
}

static void kappa_panic(const char* message)
{
    if (kappa_panic_frame != NULL)
    {
        kappa_current_panic_message = message;
        longjmp(kappa_panic_frame->env, 1);
    }

    fprintf(stderr, "runtime error: %s\n", message);
    exit(1);
}

static void kappa_panic_arity(const char* name, int expected, int actual)
{
    fprintf(stderr, "runtime error: callable '%s' expected %d argument(s) but received %d\n", name, expected, actual);
    exit(1);
}

static KValue* kappa_alloc_value(KValueTag tag)
{
    KValue* value = (KValue*)kappa_alloc(sizeof(KValue));
    value->tag = tag;
    return value;
}

static KValue* kappa_box_int(int64_t value)
{
    KValue* boxed = kappa_alloc_value(K_TAG_INT);
    boxed->as.int_value = value;
    return boxed;
}

static KValue* kappa_box_float(double value)
{
    KValue* boxed = kappa_alloc_value(K_TAG_FLOAT);
    boxed->as.float_value = value;
    return boxed;
}

static KValue* kappa_box_string(const char* value)
{
    KValue* boxed = kappa_alloc_value(K_TAG_STRING);
    boxed->as.string_value = value;
    return boxed;
}

static char* kappa_duplicate_string(const char* value)
{
    size_t length = strlen(value);
    char* copy = (char*)kappa_alloc(length + 1);
    memcpy(copy, value, length + 1);
    return copy;
}

static KValue** kappa_copy_values(KValue** values, int count)
{
    if (count <= 0)
    {
        return NULL;
    }

    size_t bytes = sizeof(KValue*) * (size_t)count;
    KValue** copy = (KValue**)kappa_alloc(bytes);
    memcpy(copy, values, bytes);
    return copy;
}

static KValue** kappa_combine_values(KValue** left, int left_count, KValue** right, int right_count)
{
    int total_count = left_count + right_count;

    if (total_count <= 0)
    {
        return NULL;
    }

    KValue** combined = (KValue**)kappa_alloc(sizeof(KValue*) * (size_t)total_count);

    if (left_count > 0)
    {
        memcpy(combined, left, sizeof(KValue*) * (size_t)left_count);
    }

    if (right_count > 0)
    {
        memcpy(combined + left_count, right, sizeof(KValue*) * (size_t)right_count);
    }

    return combined;
}

static KValue* kappa_box_char(int32_t value)
{
    KValue* boxed = kappa_alloc_value(K_TAG_CHAR);
    boxed->as.char_value = value;
    return boxed;
}

static KValue* kappa_box_bool(int value)
{
    static KValue true_value = { K_TAG_BOOL, { .bool_value = 1 } };
    static KValue false_value = { K_TAG_BOOL, { .bool_value = 0 } };
    return value ? &true_value : &false_value;
}

static KValue* kappa_unit(void)
{
    static KValue unit_value = { K_TAG_UNIT, { .int_value = 0 } };
    return &unit_value;
}

static int64_t kappa_expect_int(KValue* value)
{
    if (value == NULL || value->tag != K_TAG_INT)
    {
        kappa_panic("expected Int value");
    }

    return value->as.int_value;
}

static double kappa_expect_float(KValue* value)
{
    if (value == NULL || value->tag != K_TAG_FLOAT)
    {
        kappa_panic("expected Float value");
    }

    return value->as.float_value;
}

static int kappa_expect_bool(KValue* value)
{
    if (value == NULL || value->tag != K_TAG_BOOL)
    {
        kappa_panic("expected Bool value");
    }

    return value->as.bool_value;
}

static const char* kappa_expect_string(KValue* value)
{
    if (value == NULL || value->tag != K_TAG_STRING)
    {
        kappa_panic("expected String value");
    }

    return value->as.string_value;
}

static void kappa_expect_unit(KValue* value)
{
    if (value == NULL || value->tag != K_TAG_UNIT)
    {
        kappa_panic("expected Unit value");
    }
}

static int kappa_is_unit(KValue* value)
{
    return value != NULL && value->tag == K_TAG_UNIT;
}

static int kappa_is_int_value(KValue* value, int64_t expected)
{
    return value != NULL && value->tag == K_TAG_INT && value->as.int_value == expected;
}

static int kappa_is_float_value(KValue* value, double expected)
{
    return value != NULL && value->tag == K_TAG_FLOAT && value->as.float_value == expected;
}

static int kappa_is_string_value(KValue* value, const char* expected)
{
    return value != NULL && value->tag == K_TAG_STRING && strcmp(value->as.string_value, expected) == 0;
}

static int kappa_is_char_value(KValue* value, int32_t expected)
{
    return value != NULL && value->tag == K_TAG_CHAR && value->as.char_value == expected;
}

static KValue* kappa_make_data(int type_id, int ctor_tag, int field_count, KValue** fields)
{
    KValue* value = kappa_alloc_value(K_TAG_DATA);
    value->as.data_value.type_id = type_id;
    value->as.data_value.ctor_tag = ctor_tag;
    value->as.data_value.field_count = field_count;

    if (field_count == 0)
    {
        value->as.data_value.fields = NULL;
    }
    else
    {
        size_t field_bytes = sizeof(KValue*) * (size_t)field_count;
        value->as.data_value.fields = (KValue**)kappa_alloc(field_bytes);
        memcpy(value->as.data_value.fields, fields, field_bytes);
    }

    return value;
}

static int kappa_is_ctor(KValue* value, int type_id, int ctor_tag)
{
    if (value == NULL)
    {
        return 0;
    }

    if (type_id == __KAPPA_PRELUDE_BOOL_TYPE_ID__ && value->tag == K_TAG_BOOL)
    {
        int actual_tag = value->as.bool_value ? 0 : 1;
        return actual_tag == ctor_tag;
    }

    return value->tag == K_TAG_DATA
        && value->as.data_value.type_id == type_id
        && value->as.data_value.ctor_tag == ctor_tag;
}

static KValue* kappa_get_field(KValue* value, int index)
{
    if (value == NULL || value->tag != K_TAG_DATA)
    {
        kappa_panic("attempted to project a field from a non-data value");
    }

    if (index < 0 || index >= value->as.data_value.field_count)
    {
        kappa_panic("attempted to project an out-of-range field");
    }

    return value->as.data_value.fields[index];
}

static KValue* kappa_make_closure_with_applied(KappaFunction function, void* env, int arity, const char* debug_name, int applied_count, KValue** applied_args)
{
    KValue* value = kappa_alloc_value(K_TAG_CLOSURE);
    value->as.closure_value.function = function;
    value->as.closure_value.env = env;
    value->as.closure_value.arity = arity;
    value->as.closure_value.debug_name = debug_name;
    value->as.closure_value.applied_count = applied_count;
    value->as.closure_value.applied_args = applied_args;
    return value;
}

static KValue* kappa_make_closure(KappaFunction function, void* env, int arity, const char* debug_name)
{
    return kappa_make_closure_with_applied(function, env, arity, debug_name, 0, NULL);
}

static KValue* kappa_make_ref(KValue* initial)
{
    KValue* value = kappa_alloc_value(K_TAG_REF);
    value->as.ref_value.value = initial;
    return value;
}

static KValue* kappa_ref_read(KValue* reference)
{
    if (reference == NULL || reference->tag != K_TAG_REF)
    {
        kappa_panic("expected Ref value");
    }

    return reference->as.ref_value.value;
}

static KValue* kappa_ref_write(KValue* reference, KValue* value)
{
    if (reference == NULL || reference->tag != K_TAG_REF)
    {
        kappa_panic("expected Ref value");
    }

    reference->as.ref_value.value = value;
    return kappa_unit();
}

static KValue* kappa_make_dictionary(const char* module_name, const char* trait_name, const char* instance_key)
{
    KValue* value = kappa_alloc_value(K_TAG_DICTIONARY);
    value->as.dictionary_value.module_name = module_name;
    value->as.dictionary_value.trait_name = trait_name;
    value->as.dictionary_value.instance_key = instance_key;
    return value;
}

static KValue* kappa_make_array(int length, KValue** items)
{
    KValue* value = kappa_alloc_value(K_TAG_ARRAY);
    value->as.array_value.length = length;
    value->as.array_value.items = kappa_copy_values(items, length);
    return value;
}

static int kappa_array_length(KValue* value)
{
    if (value == NULL || value->tag != K_TAG_ARRAY)
    {
        kappa_panic("expected argument array value");
    }

    return value->as.array_value.length;
}

static KValue* kappa_array_get(KValue* value, int index)
{
    if (value == NULL || value->tag != K_TAG_ARRAY)
    {
        kappa_panic("expected argument array value");
    }

    if (index < 0 || index >= value->as.array_value.length)
    {
        kappa_panic("attempted to read an out-of-range argument array element");
    }

    return value->as.array_value.items[index];
}

static KValue* kappa_make_io_action(KappaIoExecutor execute, void* env)
{
    KValue* value = kappa_alloc_value(K_TAG_IO_ACTION);
    value->as.io_action_value.execute = execute;
    value->as.io_action_value.env = env;
    return value;
}

static KValue* kappa_expect_io_action(KValue* value)
{
    if (value == NULL || value->tag != K_TAG_IO_ACTION)
    {
        kappa_panic("expected IO action value");
    }

    return value;
}

static KValue* kappa_expect_effect_label(KValue* value)
{
    if (value == NULL || value->tag != K_TAG_EFFECT_LABEL)
    {
        kappa_panic("expected effect label value");
    }

    return value;
}

static KappaEffectOperationMetadata* kappa_find_effect_operation(KEffectLabelValue* label, const char* operation_id, const char* operation_name)
{
    int index = 0;

    while (index < label->operation_count)
    {
        KappaEffectOperationMetadata* operation = &label->operations[index];

        if (strcmp(operation->name, operation_name) == 0)
        {
            if (strcmp(operation->operation_id, operation_id) != 0)
            {
                kappa_panic("effect operation id did not match the selected effect label metadata");
            }

            return operation;
        }

        index += 1;
    }

    kappa_panic("effect label does not declare the selected operation");
    return NULL;
}

static KValue* kappa_make_effect_label(const char* name, const char* interface_id, const char* label_id, int operation_count, KappaEffectOperationMetadata* operations)
{
    KValue* value = kappa_alloc_value(K_TAG_EFFECT_LABEL);
    value->as.effect_label_value.name = name;
    value->as.effect_label_value.interface_id = interface_id;
    value->as.effect_label_value.label_id = label_id;
    value->as.effect_label_value.operation_count = operation_count;

    if (operation_count <= 0)
    {
        value->as.effect_label_value.operations = NULL;
    }
    else
    {
        size_t bytes = sizeof(KappaEffectOperationMetadata) * (size_t)operation_count;
        value->as.effect_label_value.operations = (KappaEffectOperationMetadata*)kappa_alloc(bytes);
        memcpy(value->as.effect_label_value.operations, operations, bytes);
    }

    return value;
}

static KValue* kappa_make_effect_operation_with_applied(KValue* label, KappaEffectOperationMetadata* operation, int applied_count, KValue** applied_args)
{
    KValue* value = kappa_alloc_value(K_TAG_EFFECT_OPERATION);
    value->as.effect_operation_value.label = label;
    value->as.effect_operation_value.operation = operation;
    value->as.effect_operation_value.applied_count = applied_count;
    value->as.effect_operation_value.applied_args = applied_args;
    return value;
}

typedef struct KEffectRequestActionEnv
{
    KValue* label;
    KappaEffectOperationMetadata* operation;
    int argument_count;
    KValue** arguments;
} KEffectRequestActionEnv;

static KActionResult* kappa_resume_return(KContinuation* continuation, KValue* value);
static KActionResult* kappa_resume_bind(KContinuation* continuation, KValue* value);
static KActionResult* kappa_resume_handle(KContinuation* continuation, KValue* value);

static KActionResult* kappa_make_action_return(KValue* value)
{
    KActionResult* result = (KActionResult*)kappa_alloc(sizeof(KActionResult));
    result->tag = K_ACTION_RETURN;
    result->as.action_return.value = value;
    return result;
}

static KActionResult* kappa_make_effect_request(KValue* label, KappaEffectOperationMetadata* operation, int argument_count, KValue** arguments, KContinuation* continuation)
{
    KActionResult* result = (KActionResult*)kappa_alloc(sizeof(KActionResult));
    result->tag = K_ACTION_EFFECT_REQUEST;
    result->as.effect_request.label = label;
    result->as.effect_request.operation = operation;
    result->as.effect_request.argument_count = argument_count;
    result->as.effect_request.arguments = kappa_copy_values(arguments, argument_count);
    result->as.effect_request.continuation = continuation;
    return result;
}

static KContinuation* kappa_return_continuation(void)
{
    static KReturnContinuation continuation = { { kappa_resume_return } };
    return &continuation.base;
}

static KContinuation* kappa_make_bind_continuation(KContinuation* next, KValue* binder)
{
    KBindContinuation* continuation = (KBindContinuation*)kappa_alloc(sizeof(KBindContinuation));
    continuation->base.resume = kappa_resume_bind;
    continuation->next = next;
    continuation->binder = binder;
    return &continuation->base;
}

static KContinuation* kappa_make_handle_continuation(int is_deep, KValue* label, KValue* return_clause, KValue* dispatcher, KContinuation* next)
{
    KHandleContinuation* continuation = (KHandleContinuation*)kappa_alloc(sizeof(KHandleContinuation));
    continuation->base.resume = kappa_resume_handle;
    continuation->is_deep = is_deep;
    continuation->label = label;
    continuation->return_clause = return_clause;
    continuation->dispatcher = dispatcher;
    continuation->next = next;
    return &continuation->base;
}

static KActionResult* kappa_resume_return(KContinuation* continuation, KValue* value)
{
    (void)continuation;
    return kappa_make_action_return(value);
}

static KActionResult* kappa_resume_bind(KContinuation* continuation, KValue* value)
{
    KBindContinuation* bind = (KBindContinuation*)continuation;
    return kappa_bind_action_result(bind->next->resume(bind->next, value), bind->binder);
}

static KActionResult* kappa_resume_handle(KContinuation* continuation, KValue* value)
{
    KHandleContinuation* handle = (KHandleContinuation*)continuation;
    return kappa_handle_action_result(
        handle->is_deep,
        handle->label,
        handle->return_clause,
        handle->dispatcher,
        handle->next->resume(handle->next, value));
}

static KActionResult* kappa_execute_effect_request(void* raw_env)
{
    KEffectRequestActionEnv* env = (KEffectRequestActionEnv*)raw_env;
    return kappa_make_effect_request(env->label, env->operation, env->argument_count, env->arguments, kappa_return_continuation());
}

static KValue* kappa_make_effect_operation(KValue* label_value, const char* operation_id, const char* operation_name)
{
    KValue* label = kappa_expect_effect_label(label_value);
    KappaEffectOperationMetadata* operation =
        kappa_find_effect_operation(&label->as.effect_label_value, operation_id, operation_name);

    return kappa_make_effect_operation_with_applied(label, operation, 0, NULL);
}

typedef struct KPureActionEnv
{
    KValue* value;
} KPureActionEnv;

typedef struct KBindActionEnv
{
    KValue* action;
    KValue* binder;
} KBindActionEnv;

typedef struct KResumeActionEnv
{
    KContinuation* continuation;
    KValue* value;
} KResumeActionEnv;

typedef struct KHandleActionEnv
{
    int is_deep;
    KValue* label;
    KValue* body;
    KValue* return_clause;
    KValue* dispatcher;
} KHandleActionEnv;

static KActionResult* kappa_execute_pure_action(void* raw_env)
{
    KPureActionEnv* env = (KPureActionEnv*)raw_env;
    return kappa_make_action_return(env->value);
}

static KActionResult* kappa_execute_bind_action(void* raw_env)
{
    KBindActionEnv* env = (KBindActionEnv*)raw_env;
    KValue* action = kappa_expect_io_action(env->action);
    return kappa_bind_action_result(action->as.io_action_value.execute(action->as.io_action_value.env), env->binder);
}

static KActionResult* kappa_execute_resume_action(void* raw_env)
{
    KResumeActionEnv* env = (KResumeActionEnv*)raw_env;
    return env->continuation->resume(env->continuation, env->value);
}

static KActionResult* kappa_execute_handle_action(void* raw_env)
{
    KHandleActionEnv* env = (KHandleActionEnv*)raw_env;
    KValue* body = kappa_expect_io_action(env->body);
    return kappa_handle_action_result(
        env->is_deep,
        env->label,
        env->return_clause,
        env->dispatcher,
        body->as.io_action_value.execute(body->as.io_action_value.env));
}

static KValue* kappa_pure(KValue* value)
{
    KPureActionEnv* env = (KPureActionEnv*)kappa_alloc(sizeof(KPureActionEnv));
    env->value = value;
    return kappa_make_io_action(kappa_execute_pure_action, env);
}

static KValue* kappa_bind(KValue* action, KValue* binder)
{
    KBindActionEnv* env = (KBindActionEnv*)kappa_alloc(sizeof(KBindActionEnv));
    env->action = action;
    env->binder = binder;
    return kappa_make_io_action(kappa_execute_bind_action, env);
}

typedef struct KThenClosureEnv
{
    KValue* second;
} KThenClosureEnv;

static KValue* kappa_then_binder(void* raw_env, KValue** args, int argc)
{
    KThenClosureEnv* env = (KThenClosureEnv*)raw_env;

    if (argc != 1)
    {
        kappa_panic_arity("__kappa_then", 1, argc);
    }

    (void)args;
    return env->second;
}

static KValue* kappa_then(KValue* first, KValue* second)
{
    KThenClosureEnv* env = (KThenClosureEnv*)kappa_alloc(sizeof(KThenClosureEnv));
    env->second = second;
    return kappa_bind(first, kappa_make_closure(kappa_then_binder, env, 1, "__kappa_then"));
}

static KValue* kappa_handle(int is_deep, KValue* label, KValue* body, KValue* return_clause, KValue* dispatcher)
{
    KHandleActionEnv* env = (KHandleActionEnv*)kappa_alloc(sizeof(KHandleActionEnv));
    env->is_deep = is_deep;
    env->label = label;
    env->body = body;
    env->return_clause = return_clause;
    env->dispatcher = dispatcher;
    return kappa_make_io_action(kappa_execute_handle_action, env);
}

static KValue* kappa_make_resumption(const char* display_name, int allows_multiple_resumptions, KContinuation* continuation)
{
    KValue* value = kappa_alloc_value(K_TAG_RESUMPTION);
    value->as.resumption_value.continuation = continuation;
    value->as.resumption_value.allows_multiple_resumptions = allows_multiple_resumptions;
    value->as.resumption_value.consumed = 0;
    value->as.resumption_value.display_name = display_name;
    return value;
}

static KValue* kappa_apply_closure(KValue* callee, KValue** args, int argc)
{
    KClosureValue* closure = &callee->as.closure_value;
    int total_count = closure->applied_count + argc;
    KValue** combined = kappa_combine_values(closure->applied_args, closure->applied_count, args, argc);

    if (total_count < closure->arity)
    {
        return kappa_make_closure_with_applied(
            closure->function,
            closure->env,
            closure->arity,
            closure->debug_name,
            total_count,
            combined);
    }

    KValue* value = closure->function(closure->env, combined, closure->arity);

    if (total_count == closure->arity)
    {
        return value;
    }

    return kappa_apply_value(value, combined + closure->arity, total_count - closure->arity);
}

static KValue* kappa_apply_effect_operation(KValue* callee, KValue** args, int argc)
{
    KEffectOperationValue* operation = &callee->as.effect_operation_value;
    int total_count = operation->applied_count + argc;
    int expected = operation->operation->parameter_arity;
    KValue** combined = kappa_combine_values(operation->applied_args, operation->applied_count, args, argc);

    if (total_count < expected)
    {
        return kappa_make_effect_operation_with_applied(
            operation->label,
            operation->operation,
            total_count,
            combined);
    }

    KEffectRequestActionEnv* env = (KEffectRequestActionEnv*)kappa_alloc(sizeof(KEffectRequestActionEnv));
    env->label = operation->label;
    env->operation = operation->operation;
    env->argument_count = expected;
    env->arguments = kappa_copy_values(combined, expected);

    KValue* value = kappa_make_io_action(kappa_execute_effect_request, env);

    if (total_count == expected)
    {
        return value;
    }

    return kappa_apply_value(value, combined + expected, total_count - expected);
}

static KValue* kappa_apply_resumption(KValue* callee, KValue** args, int argc)
{
    KResumptionValue* resumption = &callee->as.resumption_value;

    if (argc <= 0)
    {
        return callee;
    }

    if (!resumption->allows_multiple_resumptions && resumption->consumed)
    {
        kappa_panic("consumed one-shot resumption cannot be resumed again");
    }

    if (!resumption->allows_multiple_resumptions)
    {
        resumption->consumed = 1;
    }

    KResumeActionEnv* env = (KResumeActionEnv*)kappa_alloc(sizeof(KResumeActionEnv));
    env->continuation = resumption->continuation;
    env->value = args[0];

    KValue* value = kappa_make_io_action(kappa_execute_resume_action, env);

    if (argc == 1)
    {
        return value;
    }

    return kappa_apply_value(value, args + 1, argc - 1);
}

static KValue* kappa_apply_value(KValue* callee, KValue** args, int argc)
{
    if (callee == NULL)
    {
        kappa_panic("attempted to call a null value");
    }

    switch (callee->tag)
    {
        case K_TAG_CLOSURE:
            return kappa_apply_closure(callee, args, argc);
        case K_TAG_EFFECT_OPERATION:
            return kappa_apply_effect_operation(callee, args, argc);
        case K_TAG_RESUMPTION:
            return kappa_apply_resumption(callee, args, argc);
        default:
            kappa_panic("attempted to call a non-function value");
            return kappa_unit();
    }
}

static KValue* kappa_invoke_value(KValue* callee, KValue** args, int argc)
{
    if (callee == NULL)
    {
        kappa_panic("attempted to call a null value");
    }

    switch (callee->tag)
    {
        case K_TAG_CLOSURE:
        {
            int expected = callee->as.closure_value.arity - callee->as.closure_value.applied_count;
            if (expected != argc)
            {
                kappa_panic_arity(callee->as.closure_value.debug_name, expected, argc);
            }

            return kappa_apply_closure(callee, args, argc);
        }
        case K_TAG_EFFECT_OPERATION:
        {
            int expected = callee->as.effect_operation_value.operation->parameter_arity - callee->as.effect_operation_value.applied_count;
            if (expected != argc)
            {
                kappa_panic_arity(callee->as.effect_operation_value.operation->name, expected, argc);
            }

            return kappa_apply_effect_operation(callee, args, argc);
        }
        case K_TAG_RESUMPTION:
            if (argc != 1)
            {
                kappa_panic_arity(callee->as.resumption_value.display_name, 1, argc);
            }

            return kappa_apply_resumption(callee, args, argc);
        default:
            kappa_panic("attempted to call a non-function value");
            return kappa_unit();
    }
}

static KValue* kappa_int_add(KValue* left, KValue* right)
{
    return kappa_box_int(kappa_expect_int(left) + kappa_expect_int(right));
}

static KValue* kappa_int_subtract(KValue* left, KValue* right)
{
    return kappa_box_int(kappa_expect_int(left) - kappa_expect_int(right));
}

static KValue* kappa_int_multiply(KValue* left, KValue* right)
{
    return kappa_box_int(kappa_expect_int(left) * kappa_expect_int(right));
}

static KValue* kappa_int_divide(KValue* left, KValue* right)
{
    int64_t divisor = kappa_expect_int(right);
    if (divisor == 0)
    {
        kappa_panic("division by zero");
    }

    return kappa_box_int(kappa_expect_int(left) / divisor);
}

static KValue* kappa_int_negate(KValue* value)
{
    return kappa_box_int(-kappa_expect_int(value));
}

static KValue* kappa_int_less(KValue* left, KValue* right)
{
    return kappa_box_bool(kappa_expect_int(left) < kappa_expect_int(right));
}

static KValue* kappa_int_less_equal(KValue* left, KValue* right)
{
    return kappa_box_bool(kappa_expect_int(left) <= kappa_expect_int(right));
}

static KValue* kappa_int_greater(KValue* left, KValue* right)
{
    return kappa_box_bool(kappa_expect_int(left) > kappa_expect_int(right));
}

static KValue* kappa_int_greater_equal(KValue* left, KValue* right)
{
    return kappa_box_bool(kappa_expect_int(left) >= kappa_expect_int(right));
}

static KValue* kappa_bool_not(KValue* value)
{
    return kappa_box_bool(!kappa_expect_bool(value));
}

static KValue* kappa_bool_and(KValue* left, KValue* right)
{
    return kappa_box_bool(kappa_expect_bool(left) && kappa_expect_bool(right));
}

static KValue* kappa_bool_or(KValue* left, KValue* right)
{
    return kappa_box_bool(kappa_expect_bool(left) || kappa_expect_bool(right));
}

static KValue* kappa_value_equal(KValue* left, KValue* right)
{
    if (left == right)
    {
        return kappa_box_bool(1);
    }

    if (left == NULL || right == NULL || left->tag != right->tag)
    {
        return kappa_box_bool(0);
    }

    switch (left->tag)
    {
        case K_TAG_INT:
            return kappa_box_bool(left->as.int_value == right->as.int_value);
        case K_TAG_FLOAT:
            return kappa_box_bool(left->as.float_value == right->as.float_value);
        case K_TAG_BOOL:
            return kappa_box_bool(left->as.bool_value == right->as.bool_value);
        case K_TAG_STRING:
            return kappa_box_bool(strcmp(left->as.string_value, right->as.string_value) == 0);
        case K_TAG_CHAR:
            return kappa_box_bool(left->as.char_value == right->as.char_value);
        case K_TAG_UNIT:
            return kappa_box_bool(1);
        case K_TAG_EFFECT_LABEL:
            return kappa_box_bool(strcmp(left->as.effect_label_value.label_id, right->as.effect_label_value.label_id) == 0);
        default:
            return kappa_box_bool(0);
    }
}

static KValue* kappa_value_not_equal(KValue* left, KValue* right)
{
    KValue* equal = kappa_value_equal(left, right);
    return kappa_box_bool(!kappa_expect_bool(equal));
}

static void kappa_write_value(KValue* value)
{
    if (value == NULL)
    {
        fputs("<null>", stdout);
        return;
    }

    switch (value->tag)
    {
        case K_TAG_INT:
            printf("%lld", (long long)value->as.int_value);
            return;
        case K_TAG_FLOAT:
            printf("%.17g", value->as.float_value);
            return;
        case K_TAG_BOOL:
            fputs(value->as.bool_value ? "True" : "False", stdout);
            return;
        case K_TAG_STRING:
            fputs(value->as.string_value, stdout);
            return;
        case K_TAG_CHAR:
            putchar((char)value->as.char_value);
            return;
        case K_TAG_UNIT:
            fputs("()", stdout);
            return;
        case K_TAG_DATA:
            printf("<data:%d:%d>", value->as.data_value.type_id, value->as.data_value.ctor_tag);
            return;
        case K_TAG_CLOSURE:
            printf("<closure:%s>", value->as.closure_value.debug_name);
            return;
        case K_TAG_REF:
            fputs("<ref>", stdout);
            return;
        case K_TAG_DICTIONARY:
            printf("<dict:%s:%s:%s>", value->as.dictionary_value.module_name, value->as.dictionary_value.trait_name, value->as.dictionary_value.instance_key);
            return;
        case K_TAG_ARRAY:
            printf("<array:%d>", value->as.array_value.length);
            return;
        case K_TAG_IO_ACTION:
            fputs("<io>", stdout);
            return;
        case K_TAG_EFFECT_LABEL:
            printf("<effect-label %s>", value->as.effect_label_value.name);
            return;
        case K_TAG_EFFECT_OPERATION:
        {
            KValue* label = value->as.effect_operation_value.label;
            printf(
                "<effect-op %s.%s/%d [%d]>",
                label->as.effect_label_value.name,
                value->as.effect_operation_value.operation->name,
                value->as.effect_operation_value.operation->parameter_arity,
                value->as.effect_operation_value.applied_count);
            return;
        }
        case K_TAG_RESUMPTION:
            printf("<resumption:%s>", value->as.resumption_value.display_name);
            return;
        default:
            fputs("<unknown>", stdout);
            return;
    }
}

static KValue* kappa_builtin_print(KValue* value, int append_newline)
{
    kappa_write_value(value);
    if (append_newline)
    {
        putchar('\n');
    }
    return kappa_unit();
}

static KValue* kappa_builtin_print_int(KValue* value)
{
    printf("%lld\n", (long long)kappa_expect_int(value));
    return kappa_unit();
}

static KValue* kappa_builtin_print_string(KValue* value)
{
    fputs(kappa_expect_string(value), stdout);
    return kappa_unit();
}

static KValue* kappa_int_to_string(KValue* value)
{
    int64_t integer = kappa_expect_int(value);
    int length = snprintf(NULL, 0, "%lld", (long long)integer);

    if (length < 0)
    {
        kappa_panic("failed to format Int as String");
    }

    char* buffer = (char*)kappa_alloc((size_t)length + 1);
    snprintf(buffer, (size_t)length + 1, "%lld", (long long)integer);
    return kappa_box_string(buffer);
}

static KActionResult* kappa_bind_action_result(KActionResult* step, KValue* binder)
{
    switch (step->tag)
    {
        case K_ACTION_RETURN:
        {
            KValue* argument = step->as.action_return.value;
            KValue* binder_args[1] = { argument };
            KValue* continuation_result = kappa_apply_value(binder, binder_args, 1);
            KValue* action = kappa_expect_io_action(continuation_result);
            return action->as.io_action_value.execute(action->as.io_action_value.env);
        }
        case K_ACTION_EFFECT_REQUEST:
            return kappa_make_effect_request(
                step->as.effect_request.label,
                step->as.effect_request.operation,
                step->as.effect_request.argument_count,
                step->as.effect_request.arguments,
                kappa_make_bind_continuation(step->as.effect_request.continuation, binder));
        default:
            kappa_panic("unknown IO action result");
            return NULL;
    }
}

static KValue* kappa_execute_action_result(KActionResult* step)
{
    switch (step->tag)
    {
        case K_ACTION_RETURN:
            return step->as.action_return.value;
        case K_ACTION_EFFECT_REQUEST:
            kappa_panic("unhandled effect operation reached the native runtime");
            return NULL;
        default:
            kappa_panic("unknown IO action result");
            return NULL;
    }
}

static KActionResult* kappa_handle_action_result(int is_deep, KValue* label_value, KValue* return_clause, KValue* dispatcher, KActionResult* step)
{
    KValue* label = kappa_expect_effect_label(label_value);

    switch (step->tag)
    {
        case K_ACTION_RETURN:
        {
            KValue* return_args[1] = { step->as.action_return.value };
            KValue* handled = kappa_apply_value(return_clause, return_args, 1);
            KValue* action = kappa_expect_io_action(handled);
            return action->as.io_action_value.execute(action->as.io_action_value.env);
        }
        case K_ACTION_EFFECT_REQUEST:
        {
            KActionEffectRequest* request = &step->as.effect_request;
            KValue* request_label = kappa_expect_effect_label(request->label);

            if (strcmp(request_label->as.effect_label_value.label_id, label->as.effect_label_value.label_id) == 0)
            {
                KContinuation* resumption_continuation =
                    is_deep
                        ? kappa_make_handle_continuation(is_deep, label, return_clause, dispatcher, request->continuation)
                        : request->continuation;
                KValue* resumption =
                    kappa_make_resumption(
                        request->operation->name,
                        request->operation->allows_multiple_resumptions,
                        resumption_continuation);
                KValue* dispatcher_args[3] =
                {
                    kappa_box_string(request->operation->name),
                    kappa_make_array(request->argument_count, request->arguments),
                    resumption
                };
                KValue* handled = kappa_apply_value(dispatcher, dispatcher_args, 3);
                KValue* action = kappa_expect_io_action(handled);
                return action->as.io_action_value.execute(action->as.io_action_value.env);
            }

            return kappa_make_effect_request(
                request->label,
                request->operation,
                request->argument_count,
                request->arguments,
                kappa_make_handle_continuation(is_deep, label, return_clause, dispatcher, request->continuation));
        }
        default:
            kappa_panic("unknown IO action result");
            return NULL;
    }
}

static KValue* kappa_execute_value(KValue* value)
{
    if (value != NULL && value->tag == K_TAG_IO_ACTION)
    {
        return kappa_execute_action_result(value->as.io_action_value.execute(value->as.io_action_value.env));
    }

    return value;
}

static void kappa_print_result_if_needed(KValue* value)
{
    if (!kappa_is_unit(value))
    {
        kappa_write_value(value);
        putchar('\n');
    }
}
