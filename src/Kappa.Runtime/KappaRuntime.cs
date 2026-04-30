namespace Kappa.Runtime;

public sealed class KappaRuntimeException : Exception
{
    public KappaRuntimeException(string message) : base(message)
    {
    }
}

public delegate object? KappaInvoke(object? environment, object?[] arguments);

public interface IKappaCallable
{
    int Arity { get; }

    object? Apply(object?[] arguments);
}

public sealed class KappaClosure : IKappaCallable
{
    public int Arity { get; }
    public object? Environment { get; }
    public KappaInvoke Invoke { get; }
    public object?[] AppliedArguments { get; }

    public KappaClosure(int arity, object? environment, KappaInvoke invoke)
        : this(arity, environment, invoke, Array.Empty<object?>())
    {
    }

    private KappaClosure(int arity, object? environment, KappaInvoke invoke, object?[] appliedArguments)
    {
        Arity = arity;
        Environment = environment;
        Invoke = invoke;
        AppliedArguments = appliedArguments;
    }

    public object? Apply(object?[] arguments)
    {
        var combined = KappaRuntime.CombineArguments(AppliedArguments, arguments);

        if (combined.Length < Arity)
        {
            return new KappaClosure(Arity, Environment, Invoke, combined);
        }

        var invokedArguments = KappaRuntime.CopyPrefix(combined, Arity);
        var value = Invoke(Environment, invokedArguments);

        if (combined.Length == Arity)
        {
            return value;
        }

        return KappaRuntime.Apply(value, KappaRuntime.CopySuffix(combined, Arity));
    }
}

public sealed class KappaBuiltin : IKappaCallable
{
    public string Name { get; }
    public int Arity { get; }
    public Func<object?[], object?> Invoke { get; }
    public object?[] AppliedArguments { get; }

    public KappaBuiltin(string name, int arity, Func<object?[], object?> invoke)
        : this(name, arity, invoke, Array.Empty<object?>())
    {
    }

    private KappaBuiltin(string name, int arity, Func<object?[], object?> invoke, object?[] appliedArguments)
    {
        Name = name;
        Arity = arity;
        Invoke = invoke;
        AppliedArguments = appliedArguments;
    }

    public object? Apply(object?[] arguments)
    {
        var combined = KappaRuntime.CombineArguments(AppliedArguments, arguments);

        if (combined.Length < Arity)
        {
            return new KappaBuiltin(Name, Arity, Invoke, combined);
        }

        var invokedArguments = KappaRuntime.CopyPrefix(combined, Arity);
        var value = Invoke(invokedArguments);

        if (combined.Length == Arity)
        {
            return value;
        }

        return KappaRuntime.Apply(value, KappaRuntime.CopySuffix(combined, Arity));
    }
}

public sealed class KappaConstructorCallable : IKappaCallable
{
    public string Name { get; }
    public int Arity { get; }
    public Func<object?[], object?> Invoke { get; }
    public object?[] AppliedArguments { get; }

    public KappaConstructorCallable(string name, int arity, Func<object?[], object?> invoke)
        : this(name, arity, invoke, Array.Empty<object?>())
    {
    }

    private KappaConstructorCallable(string name, int arity, Func<object?[], object?> invoke, object?[] appliedArguments)
    {
        Name = name;
        Arity = arity;
        Invoke = invoke;
        AppliedArguments = appliedArguments;
    }

    public object? Apply(object?[] arguments)
    {
        var combined = KappaRuntime.CombineArguments(AppliedArguments, arguments);

        if (combined.Length < Arity)
        {
            return new KappaConstructorCallable(Name, Arity, Invoke, combined);
        }

        var invokedArguments = KappaRuntime.CopyPrefix(combined, Arity);
        var value = Invoke(invokedArguments);

        if (combined.Length == Arity)
        {
            return value;
        }

        return KappaRuntime.Apply(value, KappaRuntime.CopySuffix(combined, Arity));
    }
}

public sealed class KappaErasedDataValue
{
    public string ModuleName { get; }
    public string TypeName { get; }
    public string ConstructorName { get; }
    public object?[] Fields { get; }

    public KappaErasedDataValue(string moduleName, string typeName, string constructorName, object?[] fields)
    {
        ModuleName = moduleName;
        TypeName = typeName;
        ConstructorName = constructorName;
        Fields = fields;
    }
}

public sealed class KappaEffectOperationMetadata
{
    public string OperationId { get; }
    public string Name { get; }
    public int ParameterArity { get; }
    public bool AllowsMultipleResumptions { get; }

    public KappaEffectOperationMetadata(string operationId, string name, int parameterArity, bool allowsMultipleResumptions)
    {
        OperationId = operationId;
        Name = name;
        ParameterArity = parameterArity;
        AllowsMultipleResumptions = allowsMultipleResumptions;
    }
}

public sealed class KappaEffectLabel
{
    public string Name { get; }
    public string InterfaceId { get; }
    public string LabelId { get; }
    public IReadOnlyDictionary<string, KappaEffectOperationMetadata> Operations { get; }

    public KappaEffectLabel(string name, string interfaceId, string labelId, IReadOnlyDictionary<string, KappaEffectOperationMetadata> operations)
    {
        Name = name;
        InterfaceId = interfaceId;
        LabelId = labelId;
        Operations = operations;
    }
}

public sealed class KappaEffectOperationValue : IKappaCallable
{
    public KappaEffectLabel Label { get; }
    public KappaEffectOperationMetadata Operation { get; }
    public object?[] AppliedArguments { get; }

    public int Arity => Operation.ParameterArity;

    public KappaEffectOperationValue(KappaEffectLabel label, KappaEffectOperationMetadata operation)
        : this(label, operation, Array.Empty<object?>())
    {
    }

    private KappaEffectOperationValue(KappaEffectLabel label, KappaEffectOperationMetadata operation, object?[] appliedArguments)
    {
        Label = label;
        Operation = operation;
        AppliedArguments = appliedArguments;
    }

    public object? Apply(object?[] arguments)
    {
        var combined = KappaRuntime.CombineArguments(AppliedArguments, arguments);

        if (combined.Length < Operation.ParameterArity)
        {
            return new KappaEffectOperationValue(Label, Operation, combined);
        }

        var invokedArguments = KappaRuntime.CopyPrefix(combined, Operation.ParameterArity);
        object? value =
            new KappaIoAction(
                () => new KappaEffectRequest(Label, Operation, invokedArguments, KappaReturnContinuation.Instance));

        if (combined.Length == Operation.ParameterArity)
        {
            return value;
        }

        return KappaRuntime.Apply(value, KappaRuntime.CopySuffix(combined, Operation.ParameterArity));
    }
}

public abstract class KappaActionResult
{
}

public sealed class KappaActionReturn : KappaActionResult
{
    public object? Value { get; }

    public KappaActionReturn(object? value)
    {
        Value = value;
    }
}

public sealed class KappaEffectRequest : KappaActionResult
{
    public KappaEffectLabel Label { get; }
    public KappaEffectOperationMetadata Operation { get; }
    public object?[] Arguments { get; }
    public KappaContinuation Continuation { get; }

    public KappaEffectRequest(KappaEffectLabel label, KappaEffectOperationMetadata operation, object?[] arguments, KappaContinuation continuation)
    {
        Label = label;
        Operation = operation;
        Arguments = arguments;
        Continuation = continuation;
    }
}

public abstract class KappaContinuation
{
    public abstract KappaActionResult Resume(object? value);
}

public sealed class KappaReturnContinuation : KappaContinuation
{
    public static KappaReturnContinuation Instance { get; } = new();

    private KappaReturnContinuation()
    {
    }

    public override KappaActionResult Resume(object? value)
    {
        return new KappaActionReturn(value);
    }
}

public sealed class KappaBindContinuation : KappaContinuation
{
    private readonly KappaContinuation _continuation;
    private readonly IKappaCallable _binder;

    public KappaBindContinuation(KappaContinuation continuation, IKappaCallable binder)
    {
        _continuation = continuation;
        _binder = binder;
    }

    public override KappaActionResult Resume(object? value)
    {
        return KappaRuntime.BindActionResult(_continuation.Resume(value), _binder);
    }
}

public sealed class KappaHandleContinuation : KappaContinuation
{
    private readonly bool _isDeep;
    private readonly KappaEffectLabel _label;
    private readonly IKappaCallable _returnClause;
    private readonly IKappaCallable _dispatcher;
    private readonly KappaContinuation _continuation;

    public KappaHandleContinuation(bool isDeep, KappaEffectLabel label, IKappaCallable returnClause, IKappaCallable dispatcher, KappaContinuation continuation)
    {
        _isDeep = isDeep;
        _label = label;
        _returnClause = returnClause;
        _dispatcher = dispatcher;
        _continuation = continuation;
    }

    public override KappaActionResult Resume(object? value)
    {
        return KappaRuntime.HandleActionResult(_isDeep, _label, _returnClause, _dispatcher, _continuation.Resume(value));
    }
}

public sealed class KappaIoAction
{
    private readonly Func<KappaActionResult> _execute;

    public KappaIoAction(Func<KappaActionResult> execute)
    {
        _execute = execute;
    }

    public KappaActionResult Execute()
    {
        return _execute();
    }
}

public sealed class KappaResumption : IKappaCallable
{
    private readonly KappaContinuation _continuation;
    private readonly bool _allowsMultipleResumptions;
    private bool _consumed;

    public string DisplayName { get; }
    public int Arity => 1;

    public KappaResumption(string displayName, bool allowsMultipleResumptions, KappaContinuation continuation)
    {
        DisplayName = displayName;
        _allowsMultipleResumptions = allowsMultipleResumptions;
        _continuation = continuation;
    }

    public object? Apply(object?[] arguments)
    {
        if (arguments.Length != 1)
        {
            throw new KappaRuntimeException("Resumptions expect exactly one argument.");
        }

        if (!_allowsMultipleResumptions && _consumed)
        {
            throw new KappaRuntimeException($"Consumed one-shot resumption '{DisplayName}' cannot be resumed again.");
        }

        if (!_allowsMultipleResumptions)
        {
            _consumed = true;
        }

        var resumedValue = arguments[0];
        return new KappaIoAction(() => _continuation.Resume(resumedValue));
    }
}

public sealed class KappaRefCell
{
    public object? Value { get; set; }

    public KappaRefCell(object? value)
    {
        Value = value;
    }
}

public static class KappaRuntime
{
    private static readonly Dictionary<(string ModuleName, string Name), object?> Intrinsics =
        new()
        {
            { ("std.prelude", "True"), true },
            { ("std.prelude", "False"), false },
            { ("std.prelude", "pure"), new KappaBuiltin("pure", 1, arguments => Pure(arguments[0])) },
            { ("std.prelude", ">>="), new KappaBuiltin(">>=", 2, arguments => Bind(arguments[0], arguments[1])) },
            { ("std.prelude", ">>"), new KappaBuiltin(">>", 2, arguments => Then(arguments[0], arguments[1])) },
            { ("std.prelude", "runPure"), new KappaBuiltin("runPure", 1, arguments => RunPure(arguments[0])) },
            { ("std.prelude", "not"), new KappaBuiltin("not", 1, arguments => !ExpectBool(arguments[0])) },
            { ("std.prelude", "and"), new KappaBuiltin("and", 2, arguments => ExpectBool(arguments[0]) && ExpectBool(arguments[1])) },
            { ("std.prelude", "or"), new KappaBuiltin("or", 2, arguments => ExpectBool(arguments[0]) || ExpectBool(arguments[1])) },
            { ("std.prelude", "negate"), new KappaBuiltin("negate", 1, arguments => ApplyUnary("-", arguments[0])) },
            { ("std.prelude", "primitiveIntToString"), new KappaBuiltin("primitiveIntToString", 1, arguments => ExpectInt64(arguments[0]).ToString()) },
            { ("std.prelude", "print"), new KappaBuiltin("print", 1, arguments => Print(arguments[0], appendNewline: false)) },
            { ("std.prelude", "println"), new KappaBuiltin("println", 1, arguments => Print(arguments[0], appendNewline: true)) },
            { ("std.prelude", "unsafeConsume"), new KappaBuiltin("unsafeConsume", 1, _ => Unit()) },
            { ("std.prelude", "newRef"), new KappaBuiltin("newRef", 1, arguments => new KappaRefCell(arguments[0])) },
            { ("std.prelude", "readRef"), new KappaBuiltin("readRef", 1, arguments => ExpectRefCell(arguments[0]).Value) },
            { ("std.prelude", "writeRef"), new KappaBuiltin("writeRef", 2, arguments => WriteRef(arguments[0], arguments[1])) },
            { ("std.testing", "failNow"), new KappaBuiltin("failNow", 1, arguments => throw new KappaRuntimeException(ExpectString(arguments[0]))) },
        };

    public static object?[] CombineArguments(object?[] left, object?[] right)
    {
        if (left.Length == 0)
        {
            return right.Length == 0 ? Array.Empty<object?>() : CopyAll(right);
        }

        if (right.Length == 0)
        {
            return CopyAll(left);
        }

        var combined = new object?[left.Length + right.Length];
        Array.Copy(left, 0, combined, 0, left.Length);
        Array.Copy(right, 0, combined, left.Length, right.Length);
        return combined;
    }

    public static object?[] CopyAll(object?[] values)
    {
        if (values.Length == 0)
        {
            return Array.Empty<object?>();
        }

        var copied = new object?[values.Length];
        Array.Copy(values, copied, values.Length);
        return copied;
    }

    public static object?[] CopyPrefix(object?[] values, int count)
    {
        if (count == 0)
        {
            return Array.Empty<object?>();
        }

        var copied = new object?[count];
        Array.Copy(values, 0, copied, 0, count);
        return copied;
    }

    public static object?[] CopySuffix(object?[] values, int startIndex)
    {
        var count = values.Length - startIndex;

        if (count == 0)
        {
            return Array.Empty<object?>();
        }

        var copied = new object?[count];
        Array.Copy(values, startIndex, copied, 0, count);
        return copied;
    }

    public static object Unit()
    {
        return default(ValueTuple);
    }

    public static bool IsUnit(object? value)
    {
        return value is ValueTuple;
    }

    public static void ExpectUnit(object? value)
    {
        if (!IsUnit(value))
        {
            throw new KappaRuntimeException($"Expected Unit, but got {FormatValue(value)}.");
        }
    }

    public static bool ExpectBool(object? value)
    {
        if (value is bool booleanValue)
        {
            return booleanValue;
        }

        throw new KappaRuntimeException($"Expected Bool, but got {FormatValue(value)}.");
    }

    public static long ExpectInt64(object? value)
    {
        if (value is long integerValue)
        {
            return integerValue;
        }

        throw new KappaRuntimeException($"Expected Int, but got {FormatValue(value)}.");
    }

    public static double ExpectFloat64(object? value)
    {
        if (value is double floatValue)
        {
            return floatValue;
        }

        throw new KappaRuntimeException($"Expected Float, but got {FormatValue(value)}.");
    }

    public static string ExpectString(object? value)
    {
        if (value is string stringValue)
        {
            return stringValue;
        }

        throw new KappaRuntimeException($"Expected String, but got {FormatValue(value)}.");
    }

    public static object?[] ExpectObjectArray(object? value, string context)
    {
        if (value is object?[] values)
        {
            return values;
        }

        throw new KappaRuntimeException($"Expected an argument vector for {context}, but got {FormatValue(value)}.");
    }

    public static IKappaCallable ExpectCallable(object? value, string context)
    {
        if (value is IKappaCallable callable)
        {
            return callable;
        }

        throw new KappaRuntimeException($"Expected a callable {context}, but got {FormatValue(value)}.");
    }

    public static KappaIoAction ExpectIoAction(object? value)
    {
        if (value is KappaIoAction action)
        {
            return action;
        }

        throw new KappaRuntimeException($"Expected an IO action, but got {FormatValue(value)}.");
    }

    public static KappaEffectLabel ExpectEffectLabel(object? value)
    {
        if (value is KappaEffectLabel label)
        {
            return label;
        }

        throw new KappaRuntimeException($"Expected an effect label, but got {FormatValue(value)}.");
    }

    public static KappaRefCell ExpectRefCell(object? value)
    {
        if (value is KappaRefCell cell)
        {
            return cell;
        }

        throw new KappaRuntimeException($"Expected a Ref cell, but got {FormatValue(value)}.");
    }

    public static object? ResolveIntrinsic(string moduleName, string name)
    {
        if (!SupportsIntrinsic(moduleName, name))
        {
            throw new KappaRuntimeException($"Unsupported intrinsic '{moduleName}.{name}' in the effectful dotnet runtime.");
        }

        return Intrinsics[(moduleName, name)];
    }

    public static bool SupportsIntrinsic(string moduleName, string name)
    {
        return Intrinsics.ContainsKey((moduleName, name));
    }

    public static object? Apply(object? callee, object?[] arguments)
    {
        if (callee is IKappaCallable callable)
        {
            return callable.Apply(arguments);
        }

        throw new KappaRuntimeException($"Cannot apply {FormatValue(callee)} as a function.");
    }

    public static object? ApplyUnary(string operatorName, object? operand)
    {
        return operatorName switch
        {
            "-" when operand is long integerValue => -integerValue,
            "-" when operand is double floatValue => -floatValue,
            _ => throw new KappaRuntimeException($"Unary '{operatorName}' is not supported for {FormatValue(operand)}."),
        };
    }

    public static object? ApplyBinary(string operatorName, object? left, object? right)
    {
        return (operatorName, left, right) switch
        {
            ("+", long lhs, long rhs) => lhs + rhs,
            ("-", long lhs, long rhs) => lhs - rhs,
            ("*", long lhs, long rhs) => lhs * rhs,
            ("/", long lhs, long rhs) when rhs == 0L => throw new KappaRuntimeException("Division by zero."),
            ("/", long lhs, long rhs) => lhs / rhs,
            ("+", double lhs, double rhs) => lhs + rhs,
            ("-", double lhs, double rhs) => lhs - rhs,
            ("*", double lhs, double rhs) => lhs * rhs,
            ("/", double lhs, double rhs) => lhs / rhs,
            ("==", _, _) => ValuesEqual(left, right),
            ("!=", _, _) => !ValuesEqual(left, right),
            ("<", long lhs, long rhs) => lhs < rhs,
            ("<=", long lhs, long rhs) => lhs <= rhs,
            (">", long lhs, long rhs) => lhs > rhs,
            (">=", long lhs, long rhs) => lhs >= rhs,
            ("<", double lhs, double rhs) => lhs < rhs,
            ("<=", double lhs, double rhs) => lhs <= rhs,
            (">", double lhs, double rhs) => lhs > rhs,
            (">=", double lhs, double rhs) => lhs >= rhs,
            ("&&", bool lhs, bool rhs) => lhs && rhs,
            ("||", bool lhs, bool rhs) => lhs || rhs,
            _ => throw new KappaRuntimeException($"Operator '{operatorName}' is not supported for {FormatValue(left)} and {FormatValue(right)}."),
        };
    }

    public static bool ValuesEqual(object? left, object? right)
    {
        if (ReferenceEquals(left, right))
        {
            return true;
        }

        if (left is null || right is null)
        {
            return false;
        }

        return (left, right) switch
        {
            (long lhs, long rhs) => lhs == rhs,
            (double lhs, double rhs) => lhs.Equals(rhs),
            (bool lhs, bool rhs) => lhs == rhs,
            (string lhs, string rhs) => String.Equals(lhs, rhs, StringComparison.Ordinal),
            (ValueTuple, ValueTuple) => true,
            (KappaErasedDataValue lhs, KappaErasedDataValue rhs) =>
                String.Equals(lhs.ModuleName, rhs.ModuleName, StringComparison.Ordinal)
                && String.Equals(lhs.TypeName, rhs.TypeName, StringComparison.Ordinal)
                && String.Equals(lhs.ConstructorName, rhs.ConstructorName, StringComparison.Ordinal)
                && lhs.Fields.Length == rhs.Fields.Length
                && lhs.Fields.Zip(rhs.Fields, ValuesEqual).All(equal => equal),
            _ => Equals(left, right),
        };
    }

    private static int? TryGetOrderingRank(object? value)
    {
        if (value is null)
        {
            return null;
        }

        return value.GetType().Name switch
        {
            "LT" => -1,
            "EQ" => 0,
            "GT" => 1,
            _ => null,
        };
    }

    public static int CompareBuiltinSign(object? left, object? right)
    {
        return (left, right) switch
        {
            (long lhs, long rhs) => lhs.CompareTo(rhs),
            (double lhs, double rhs) => lhs.CompareTo(rhs),
            (bool lhs, bool rhs) => lhs.CompareTo(rhs),
            (string lhs, string rhs) => StringComparer.Ordinal.Compare(lhs, rhs),
            (char lhs, char rhs) => lhs.CompareTo(rhs),
            _ when TryGetOrderingRank(left) is int lhsRank && TryGetOrderingRank(right) is int rhsRank => lhsRank.CompareTo(rhsRank),
            _ => throw new KappaRuntimeException($"Intrinsic 'compare' is not supported for {FormatValue(left)} and {FormatValue(right)}."),
        };
    }

    public static string ShowBuiltin(object? value)
    {
        return value switch
        {
            null => "()",
            ValueTuple => "()",
            bool boolValue => boolValue ? "True" : "False",
            long integerValue => integerValue.ToString(System.Globalization.CultureInfo.InvariantCulture),
            double floatValue => floatValue.ToString("G17", System.Globalization.CultureInfo.InvariantCulture),
            string stringValue => stringValue,
            char charValue => charValue.ToString(),
            _ when TryGetOrderingRank(value) is int rank => rank switch
            {
                < 0 => "LT",
                > 0 => "GT",
                _ => "EQ",
            },
            _ => FormatValue(value),
        };
    }

    public static KappaEffectLabel CreateEffectLabel(string name, string interfaceId, string labelId, KappaEffectOperationMetadata[] operations)
    {
        var map = new Dictionary<string, KappaEffectOperationMetadata>(StringComparer.Ordinal);

        foreach (var operation in operations)
        {
            map[operation.Name] = operation;
        }

        return new KappaEffectLabel(name, interfaceId, labelId, map);
    }

    public static KappaEffectOperationValue CreateEffectOperation(object? labelValue, string operationId, string operationName)
    {
        var label = ExpectEffectLabel(labelValue);

        if (!label.Operations.TryGetValue(operationName, out var operation))
        {
            throw new KappaRuntimeException($"Effect label '{label.Name}' does not declare operation '{operationName}'.");
        }

        if (!String.Equals(operation.OperationId, operationId, StringComparison.Ordinal))
        {
            throw new KappaRuntimeException($"Effect label '{label.Name}' does not declare operation id '{operationId}' for '{operationName}'.");
        }

        return new KappaEffectOperationValue(label, operation);
    }

    public static object Pure(object? value)
    {
        return new KappaIoAction(() => new KappaActionReturn(value));
    }

    public static object Bind(object? actionValue, object? continuationValue)
    {
        var action = ExpectIoAction(actionValue);
        var continuation = ExpectCallable(continuationValue, "continuation");
        return new KappaIoAction(() => BindActionResult(action.Execute(), continuation));
    }

    public static object Then(object? leftActionValue, object? rightActionValue)
    {
        var rightAction = rightActionValue;
        var continuation =
            new KappaBuiltin(
                "__kappa_then",
                1,
                _ => rightAction);

        return Bind(leftActionValue, continuation);
    }

    public static object? RunPure(object? actionValue)
    {
        return ExecuteActionResult(ExpectIoAction(actionValue).Execute());
    }

    public static object? Execute(object? actionValue)
    {
        return ExecuteActionResult(ExpectIoAction(actionValue).Execute());
    }

    public static object Handle(bool isDeep, object? labelValue, object? bodyValue, object? returnClauseValue, object? dispatcherValue)
    {
        var label = ExpectEffectLabel(labelValue);
        var body = ExpectIoAction(bodyValue);
        var returnClause = ExpectCallable(returnClauseValue, "handler return clause");
        var dispatcher = ExpectCallable(dispatcherValue, "handler operation dispatcher");

        return new KappaIoAction(() => HandleActionResult(isDeep, label, returnClause, dispatcher, body.Execute()));
    }

    public static object? Print(object? value, bool appendNewline)
    {
        var rendered = value is string stringValue ? stringValue : FormatValue(value);

        if (appendNewline)
        {
            Console.WriteLine(rendered);
        }
        else
        {
            Console.Write(rendered);
        }

        return Unit();
    }

    public static object WriteRef(object? refCellValue, object? nextValue)
    {
        ExpectRefCell(refCellValue).Value = nextValue;
        return Unit();
    }

    public static string FormatValue(object? value)
    {
        return value switch
        {
            null => "()",
            ValueTuple => "()",
            bool boolValue => boolValue ? "True" : "False",
            long integerValue => integerValue.ToString(),
            double floatValue => floatValue.ToString(System.Globalization.CultureInfo.InvariantCulture),
            string stringValue => $"\"{stringValue.Replace("\\", "\\\\", StringComparison.Ordinal).Replace("\"", "\\\"", StringComparison.Ordinal)}\"",
            KappaEffectLabel label => $"<effect-label {label.Name}>",
            KappaEffectOperationValue operation => $"<effect-op {operation.Label.Name}.{operation.Operation.Name}/{operation.Operation.ParameterArity} [{operation.AppliedArguments.Length}]>",
            KappaClosure => "<function>",
            KappaBuiltin builtin => $"<builtin {builtin.Name}>",
            KappaConstructorCallable constructor => $"<constructor {constructor.Name}/{constructor.Arity} [{constructor.AppliedArguments.Length}]>",
            KappaErasedDataValue erased =>
                erased.Fields.Length == 0
                    ? erased.ConstructorName
                    : $"({erased.ConstructorName} {String.Join(" ", erased.Fields.Select(FormatValue))})",
            KappaIoAction => "<io>",
            KappaRefCell => "<ref>",
            _ => value.ToString() ?? "()",
        };
    }

    internal static KappaActionResult BindActionResult(KappaActionResult step, IKappaCallable continuation)
    {
        return step switch
        {
            KappaActionReturn result => ExpectIoAction(continuation.Apply(new object?[] { result.Value })).Execute(),
            KappaEffectRequest request =>
                new KappaEffectRequest(
                    request.Label,
                    request.Operation,
                    request.Arguments,
                    new KappaBindContinuation(request.Continuation, continuation)),
            _ => throw new KappaRuntimeException("Unknown IO action result."),
        };
    }

    private static object? ExecuteActionResult(KappaActionResult step)
    {
        return step switch
        {
            KappaActionReturn result => result.Value,
            KappaEffectRequest request =>
                throw new KappaRuntimeException($"Unhandled effect operation '{request.Label.Name}.{request.Operation.Name}'."),
            _ => throw new KappaRuntimeException("Unknown IO action result."),
        };
    }

    internal static KappaActionResult HandleActionResult(
        bool isDeep,
        KappaEffectLabel label,
        IKappaCallable returnClause,
        IKappaCallable dispatcher,
        KappaActionResult step)
    {
        switch (step)
        {
            case KappaActionReturn result:
            {
                var handled = returnClause.Apply(new object?[] { result.Value });
                return ExpectIoAction(handled).Execute();
            }
            case KappaEffectRequest request when String.Equals(request.Label.LabelId, label.LabelId, StringComparison.Ordinal):
            {
                KappaContinuation resumptionContinuation =
                    isDeep
                        ? new KappaHandleContinuation(isDeep, label, returnClause, dispatcher, request.Continuation)
                        : request.Continuation;

                var resumption = new KappaResumption(
                    $"{label.Name}.{request.Operation.Name}",
                    request.Operation.AllowsMultipleResumptions,
                    resumptionContinuation);

                var handled =
                    dispatcher.Apply(
                        new object?[]
                        {
                            request.Operation.Name,
                            request.Arguments,
                            resumption,
                        });

                return ExpectIoAction(handled).Execute();
            }
            case KappaEffectRequest request:
                return new KappaEffectRequest(
                    request.Label,
                    request.Operation,
                    request.Arguments,
                    new KappaHandleContinuation(isDeep, label, returnClause, dispatcher, request.Continuation));
            default:
                throw new KappaRuntimeException("Unknown IO action result.");
        }
    }
}
