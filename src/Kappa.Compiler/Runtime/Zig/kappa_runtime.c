#include <stdint.h>
#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct KValue KValue;
typedef KValue* (*KappaFunction)(void* env, KValue** args, int argc);

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
    K_TAG_DICTIONARY = 10
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

static KValue* kappa_make_closure(KappaFunction function, void* env, int arity, const char* debug_name)
{
    KValue* value = kappa_alloc_value(K_TAG_CLOSURE);
    value->as.closure_value.function = function;
    value->as.closure_value.env = env;
    value->as.closure_value.arity = arity;
    value->as.closure_value.debug_name = debug_name;
    return value;
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

static KValue* kappa_invoke_value(KValue* callee, KValue** args, int argc)
{
    if (callee == NULL || callee->tag != K_TAG_CLOSURE)
    {
        kappa_panic("attempted to call a non-function value");
    }

    if (callee->as.closure_value.arity != argc)
    {
        kappa_panic_arity(callee->as.closure_value.debug_name, callee->as.closure_value.arity, argc);
    }

    return callee->as.closure_value.function(callee->as.closure_value.env, args, argc);
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
    if (value == NULL || value->tag != K_TAG_STRING)
    {
        kappa_panic("expected String value");
    }

    fputs(value->as.string_value, stdout);
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

static void kappa_print_result_if_needed(KValue* value)
{
    if (!kappa_is_unit(value))
    {
        kappa_write_value(value);
        putchar('\n');
    }
}

