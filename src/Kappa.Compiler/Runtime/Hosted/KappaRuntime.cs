using System;
using System.Collections.Generic;
using System.Linq;

namespace Kappa.Generated;

internal sealed class RuntimeError : Exception
{
    public RuntimeError(string message) : base(message)
    {
    }
}

internal enum LiteralKind
{
    Integer,
    Float,
    String,
    Character,
    Unit
}

internal sealed class LiteralValue
{
    public LiteralKind Kind { get; }
    public long IntegerValue { get; }
    public double FloatValue { get; }
    public string? StringValue { get; }
    public char CharacterValue { get; }

    private LiteralValue(LiteralKind kind, long integerValue = 0L, double floatValue = 0.0, string? stringValue = null, char characterValue = '\0')
    {
        Kind = kind;
        IntegerValue = integerValue;
        FloatValue = floatValue;
        StringValue = stringValue;
        CharacterValue = characterValue;
    }

    public static LiteralValue FromInteger(long value) => new(LiteralKind.Integer, integerValue: value);
    public static LiteralValue FromFloat(double value) => new(LiteralKind.Float, floatValue: value);
    public static LiteralValue FromString(string value) => new(LiteralKind.String, stringValue: value);
    public static LiteralValue FromCharacter(char value) => new(LiteralKind.Character, characterValue: value);
    public static LiteralValue Unit() => new(LiteralKind.Unit);
}

internal abstract class KExpr
{
}

internal sealed class LiteralExpression : KExpr
{
    public LiteralValue Value { get; }

    public LiteralExpression(LiteralValue value)
    {
        Value = value;
    }
}

internal sealed class NameExpression : KExpr
{
    public string[] Segments { get; }

    public NameExpression(string[] segments)
    {
        Segments = segments;
    }
}

internal sealed class ClosureExpression : KExpr
{
    public string[] Parameters { get; }
    public KExpr Body { get; }

    public ClosureExpression(string[] parameters, KExpr body)
    {
        Parameters = parameters;
        Body = body;
    }
}

internal sealed class IfExpression : KExpr
{
    public KExpr Condition { get; }
    public KExpr WhenTrue { get; }
    public KExpr WhenFalse { get; }

    public IfExpression(KExpr condition, KExpr whenTrue, KExpr whenFalse)
    {
        Condition = condition;
        WhenTrue = whenTrue;
        WhenFalse = whenFalse;
    }
}

internal sealed class MatchExpression : KExpr
{
    public KExpr Scrutinee { get; }
    public MatchCase[] Cases { get; }

    public MatchExpression(KExpr scrutinee, MatchCase[] cases)
    {
        Scrutinee = scrutinee;
        Cases = cases;
    }
}

internal sealed class ApplyExpression : KExpr
{
    public KExpr Callee { get; }
    public KExpr[] Arguments { get; }

    public ApplyExpression(KExpr callee, KExpr[] arguments)
    {
        Callee = callee;
        Arguments = arguments;
    }
}

internal sealed class UnaryExpression : KExpr
{
    public string OperatorName { get; }
    public KExpr Operand { get; }

    public UnaryExpression(string operatorName, KExpr operand)
    {
        OperatorName = operatorName;
        Operand = operand;
    }
}

internal sealed class BinaryExpression : KExpr
{
    public KExpr Left { get; }
    public string OperatorName { get; }
    public KExpr Right { get; }

    public BinaryExpression(KExpr left, string operatorName, KExpr right)
    {
        Left = left;
        OperatorName = operatorName;
        Right = right;
    }
}

internal sealed class PrefixedStringExpression : KExpr
{
    public string Prefix { get; }
    public StringPart[] Parts { get; }

    public PrefixedStringExpression(string prefix, StringPart[] parts)
    {
        Prefix = prefix;
        Parts = parts;
    }
}

internal sealed class StringPart
{
    public string? Text { get; }
    public KExpr? Interpolation { get; }

    private StringPart(string? text, KExpr? interpolation)
    {
        Text = text;
        Interpolation = interpolation;
    }

    public static StringPart FromText(string text) => new(text, null);
    public static StringPart FromInterpolation(KExpr expression) => new(null, expression);
}

internal enum PatternKind
{
    Wildcard,
    Name,
    Literal,
    Constructor
}

internal sealed class Pattern
{
    public PatternKind Kind { get; }
    public string? BoundName { get; }
    public string[] Segments { get; }
    public LiteralValue? Literal { get; }
    public Pattern[] Arguments { get; }

    private Pattern(PatternKind kind, string? boundName, string[] segments, LiteralValue? literal, Pattern[] arguments)
    {
        Kind = kind;
        BoundName = boundName;
        Segments = segments;
        Literal = literal;
        Arguments = arguments;
    }

    public static Pattern Wildcard() => new(PatternKind.Wildcard, null, Array.Empty<string>(), null, Array.Empty<Pattern>());
    public static Pattern FromName(string name) => new(PatternKind.Name, name, Array.Empty<string>(), null, Array.Empty<Pattern>());
    public static Pattern FromLiteral(LiteralValue literal) => new(PatternKind.Literal, null, Array.Empty<string>(), literal, Array.Empty<Pattern>());
    public static Pattern Constructor(string[] segments, Pattern[] arguments) => new(PatternKind.Constructor, null, segments, null, arguments);
}

internal sealed class MatchCase
{
    public Pattern Pattern { get; }
    public KExpr? Guard { get; }
    public KExpr Body { get; }

    public MatchCase(Pattern pattern, KExpr? guard, KExpr body)
    {
        Pattern = pattern;
        Guard = guard;
        Body = body;
    }
}

internal enum ImportNamespaceKind
{
    None,
    Term,
    Type,
    Trait,
    Constructor
}

internal sealed class ImportItem
{
    public ImportNamespaceKind Namespace { get; }
    public string Name { get; }
    public bool IncludeConstructors { get; }

    public ImportItem(ImportNamespaceKind @namespace, string name, bool includeConstructors)
    {
        Namespace = @namespace;
        Name = name;
        IncludeConstructors = includeConstructors;
    }
}

internal sealed class ExceptItem
{
    public ImportNamespaceKind Namespace { get; }
    public string Name { get; }

    public ExceptItem(ImportNamespaceKind @namespace, string name)
    {
        Namespace = @namespace;
        Name = name;
    }
}

internal enum ImportSelectionKind
{
    QualifiedOnly,
    Items,
    All,
    AllExcept
}

internal sealed class ImportSelection
{
    public ImportSelectionKind Kind { get; }
    public ImportItem[] Items { get; }
    public ExceptItem[] ExcludedItems { get; }

    private ImportSelection(ImportSelectionKind kind, ImportItem[] items, ExceptItem[] excludedItems)
    {
        Kind = kind;
        Items = items;
        ExcludedItems = excludedItems;
    }

    public static ImportSelection QualifiedOnly() => new(ImportSelectionKind.QualifiedOnly, Array.Empty<ImportItem>(), Array.Empty<ExceptItem>());
    public static ImportSelection FromItems(params ImportItem[] items) => new(ImportSelectionKind.Items, items, Array.Empty<ExceptItem>());
    public static ImportSelection All() => new(ImportSelectionKind.All, Array.Empty<ImportItem>(), Array.Empty<ExceptItem>());
    public static ImportSelection AllExcept(params ExceptItem[] excludedItems) => new(ImportSelectionKind.AllExcept, Array.Empty<ImportItem>(), excludedItems);
}

internal sealed class ModuleSpecifier
{
    public bool IsUrl { get; }
    public string[] Segments { get; }
    public string? Url { get; }

    private ModuleSpecifier(bool isUrl, string[] segments, string? url)
    {
        IsUrl = isUrl;
        Segments = segments;
        Url = url;
    }

    public static ModuleSpecifier Dotted(params string[] segments) => new(false, segments, null);
    public static ModuleSpecifier FromUrl(string url) => new(true, Array.Empty<string>(), url);
}

internal sealed class ImportSpec
{
    public ModuleSpecifier Source { get; }
    public string? Alias { get; }
    public ImportSelection Selection { get; }

    public ImportSpec(ModuleSpecifier source, string? alias, ImportSelection selection)
    {
        Source = source;
        Alias = alias;
        Selection = selection;
    }
}

internal sealed class BindingSpec
{
    public string Name { get; }
    public string[] Parameters { get; }
    public KExpr? Body { get; }
    public bool Intrinsic { get; }

    public BindingSpec(string name, string[] parameters, KExpr? body, bool intrinsic)
    {
        Name = name;
        Parameters = parameters;
        Body = body;
        Intrinsic = intrinsic;
    }
}

internal sealed class ConstructorSpec
{
    public string Name { get; }
    public int Arity { get; }
    public string TypeName { get; }

    public ConstructorSpec(string name, int arity, string typeName)
    {
        Name = name;
        Arity = arity;
        TypeName = typeName;
    }
}

internal sealed class RuntimeModuleSpec
{
    public string Name { get; }
    public ImportSpec[] Imports { get; }
    public string[] Exports { get; }
    public string[] IntrinsicTerms { get; }
    public ConstructorSpec[] Constructors { get; }
    public BindingSpec[] Bindings { get; }

    public RuntimeModuleSpec(string name, ImportSpec[] imports, string[] exports, string[] intrinsicTerms, ConstructorSpec[] constructors, BindingSpec[] bindings)
    {
        Name = name;
        Imports = imports;
        Exports = exports;
        IntrinsicTerms = intrinsicTerms;
        Constructors = constructors;
        Bindings = bindings;
    }
}

internal abstract class KValue
{
}

internal sealed class IntegerValue : KValue
{
    public long Value { get; }

    public IntegerValue(long value)
    {
        Value = value;
    }
}

internal sealed class FloatValue : KValue
{
    public double Value { get; }

    public FloatValue(double value)
    {
        Value = value;
    }
}

internal sealed class BooleanValue : KValue
{
    public bool Value { get; }

    public BooleanValue(bool value)
    {
        Value = value;
    }
}

internal sealed class StringValue : KValue
{
    public string Value { get; }

    public StringValue(string value)
    {
        Value = value;
    }
}

internal sealed class CharacterValue : KValue
{
    public char Value { get; }

    public CharacterValue(char value)
    {
        Value = value;
    }
}

internal sealed class UnitValue : KValue
{
    public static UnitValue Instance { get; } = new();

    private UnitValue()
    {
    }
}

internal sealed class FunctionValue : KValue
{
    public string[] Parameters { get; }
    public KExpr Body { get; }
    public RuntimeScope Scope { get; }

    public FunctionValue(string[] parameters, KExpr body, RuntimeScope scope)
    {
        Parameters = parameters;
        Body = body;
        Scope = scope;
    }
}

internal sealed class BuiltinValue : KValue
{
    public string Name { get; }
    public List<KValue> Arguments { get; }

    public BuiltinValue(string name, List<KValue>? arguments = null)
    {
        Name = name;
        Arguments = arguments ?? new List<KValue>();
    }
}

internal sealed class RuntimeConstructor
{
    public string Name { get; }
    public string QualifiedName { get; }
    public int Arity { get; }
    public string TypeName { get; }

    public RuntimeConstructor(string name, string qualifiedName, int arity, string typeName)
    {
        Name = name;
        QualifiedName = qualifiedName;
        Arity = arity;
        TypeName = typeName;
    }
}

internal sealed class ConstructorFunctionValue : KValue
{
    public RuntimeConstructor Constructor { get; }
    public List<KValue> Arguments { get; }

    public ConstructorFunctionValue(RuntimeConstructor constructor, List<KValue>? arguments = null)
    {
        Constructor = constructor;
        Arguments = arguments ?? new List<KValue>();
    }
}

internal sealed class ConstructedValue : KValue
{
    public RuntimeConstructor Constructor { get; }
    public List<KValue> Fields { get; }

    public ConstructedValue(RuntimeConstructor constructor, List<KValue> fields)
    {
        Constructor = constructor;
        Fields = fields;
    }
}

internal sealed class IOActionValue : KValue
{
    public Func<KValue> Action { get; }

    public IOActionValue(Func<KValue> action)
    {
        Action = action;
    }
}

internal sealed class RuntimeScope
{
    public Dictionary<string, KValue> Locals { get; }
    public string CurrentModule { get; }
    public RuntimeContext Context { get; }

    public RuntimeScope(Dictionary<string, KValue> locals, string currentModule, RuntimeContext context)
    {
        Locals = locals;
        CurrentModule = currentModule;
        Context = context;
    }
}

internal sealed class RuntimeModule
{
    public string Name { get; }
    public Dictionary<string, BindingSpec> Definitions { get; }
    public Dictionary<string, RuntimeConstructor> Constructors { get; }
    public HashSet<string> IntrinsicTerms { get; }
    public HashSet<string> Exports { get; }
    public List<ImportSpec> Imports { get; }
    public Dictionary<string, Lazy<KValue>> Values { get; }

    public RuntimeModule(string name, Dictionary<string, BindingSpec> definitions, Dictionary<string, RuntimeConstructor> constructors, HashSet<string> intrinsicTerms, HashSet<string> exports, List<ImportSpec> imports)
    {
        Name = name;
        Definitions = definitions;
        Constructors = constructors;
        IntrinsicTerms = intrinsicTerms;
        Exports = exports;
        Imports = imports;
        Values = new Dictionary<string, Lazy<KValue>>(StringComparer.Ordinal);
    }
}

internal sealed class RuntimeContext
{
    public Dictionary<string, RuntimeModule> Modules { get; }

    public RuntimeContext(Dictionary<string, RuntimeModule> modules)
    {
        Modules = modules;
    }
}

internal static class KappaRunner
{
    private static readonly HashSet<string> BuiltinBinaryNames = new(StringComparer.Ordinal)
    {
        "+", "-", "*", "/", "==", "!=", "<", "<=", ">", ">=", "&&", "||"
    };

    public static KValue Run(IReadOnlyDictionary<string, RuntimeModuleSpec> moduleSpecs, string entryPoint)
    {
        var context = BuildContext(moduleSpecs);
        var (moduleName, bindingName) = ResolveEntryPoint(context, entryPoint);
        return ExecuteIfIo(ForceBinding(context.Modules[moduleName], bindingName));
    }

    public static bool ShouldPrintResult(KValue value) => value is not UnitValue;

    public static string Format(KValue value) =>
        value switch
        {
            IntegerValue integerValue => integerValue.Value.ToString(),
            FloatValue floatValue => floatValue.Value.ToString(System.Globalization.CultureInfo.InvariantCulture),
            BooleanValue { Value: true } => "True",
            BooleanValue { Value: false } => "False",
            StringValue stringValue => $"\"{stringValue.Value}\"",
            CharacterValue characterValue => $"'{characterValue.Value}'",
            UnitValue => "()",
            ConstructorFunctionValue constructorValue => $"<constructor {constructorValue.Constructor.Name}/{constructorValue.Constructor.Arity} [{constructorValue.Arguments.Count}]>",
            ConstructedValue constructedValue when constructedValue.Fields.Count == 0 => constructedValue.Constructor.Name,
            ConstructedValue constructedValue when constructedValue.Constructor.Name == "::" && constructedValue.Fields.Count == 2 => $"{Format(constructedValue.Fields[0])} :: {Format(constructedValue.Fields[1])}",
            ConstructedValue constructedValue => $"{constructedValue.Constructor.Name} {string.Join(" ", constructedValue.Fields.Select(Format))}",
            IOActionValue => "<io>",
            FunctionValue => "<function>",
            BuiltinValue builtinValue => $"<builtin {builtinValue.Name}>",
            _ => throw new RuntimeError("Unknown runtime value.")
        };

    private static RuntimeContext BuildContext(IReadOnlyDictionary<string, RuntimeModuleSpec> moduleSpecs)
    {
        var modules =
            moduleSpecs.ToDictionary(
                pair => pair.Key,
                pair =>
                {
                    var spec = pair.Value;
                    var definitions =
                        spec.Bindings
                            .Where(binding => !binding.Intrinsic)
                            .ToDictionary(binding => binding.Name, binding => binding, StringComparer.Ordinal);

                    var constructors =
                        spec.Constructors
                            .ToDictionary(
                                constructor => constructor.Name,
                                constructor => new RuntimeConstructor(constructor.Name, $"{spec.Name}.{constructor.Name}", constructor.Arity, constructor.TypeName),
                                StringComparer.Ordinal);

                    return new RuntimeModule(
                        spec.Name,
                        definitions,
                        constructors,
                        new HashSet<string>(spec.IntrinsicTerms, StringComparer.Ordinal),
                        new HashSet<string>(spec.Exports, StringComparer.Ordinal),
                        spec.Imports.ToList());
                },
                StringComparer.Ordinal);

        var context = new RuntimeContext(modules);

        foreach (var module in modules.Values)
        {
            foreach (var definition in module.Definitions)
            {
                var bindingName = definition.Key;
                var binding = definition.Value;
                module.Values[bindingName] = new Lazy<KValue>(() => EvaluateDefinition(context, module.Name, binding));
            }

            foreach (var intrinsicName in module.IntrinsicTerms)
            {
                module.Values[intrinsicName] = new Lazy<KValue>(() => CreateIntrinsicTermValue(module.Name, intrinsicName));
            }
        }

        return context;
    }

    private static (string ModuleName, string BindingName) ResolveEntryPoint(RuntimeContext context, string entryPoint)
    {
        var segments =
            entryPoint.Split(new[] { '.' }, StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries);

        if (segments.Length == 0)
        {
            throw new RuntimeError("Expected a binding name to run.");
        }

        if (segments.Length == 1)
        {
            var bindingName = segments[0];
            var matches =
                context.Modules.Values
                    .Where(module => module.Definitions.ContainsKey(bindingName))
                    .Select(module => (module.Name, BindingName: bindingName))
                    .ToList();

            return matches.Count switch
            {
                0 => throw new RuntimeError($"No executable binding named '{bindingName}' was found."),
                1 => matches[0],
                _ => throw new RuntimeError($"Binding name '{bindingName}' is ambiguous. Use a fully qualified name.")
            };
        }

        var moduleName = string.Join(".", segments.Take(segments.Length - 1));
        var resolvedBindingName = segments[^1];

        if (!context.Modules.TryGetValue(moduleName, out var runtimeModule))
        {
            throw new RuntimeError($"Module '{moduleName}' was not found.");
        }

        if (!runtimeModule.Definitions.ContainsKey(resolvedBindingName))
        {
            throw new RuntimeError($"Module '{moduleName}' does not define '{resolvedBindingName}'.");
        }

        return (moduleName, resolvedBindingName);
    }

    private static KValue EvaluateDefinition(RuntimeContext context, string moduleName, BindingSpec definition)
    {
        if (definition.Body is null)
        {
            throw new RuntimeError($"Binding '{definition.Name}' does not belong to the executable backend subset.");
        }

        var baseScope = new RuntimeScope(new Dictionary<string, KValue>(StringComparer.Ordinal), moduleName, context);

        if (definition.Parameters.Length == 0)
        {
            return EvaluateExpression(baseScope, definition.Body);
        }

        return new FunctionValue(definition.Parameters, definition.Body, baseScope);
    }

    private static KValue EvaluateExpression(RuntimeScope scope, KExpr expression)
    {
        switch (expression)
        {
            case LiteralExpression literalExpression:
                return LiteralToValue(literalExpression.Value);
            case NameExpression nameExpression:
                return ResolveName(scope, nameExpression.Segments);
            case PrefixedStringExpression prefixedStringExpression:
                return EvaluatePrefixedString(scope, prefixedStringExpression.Prefix, prefixedStringExpression.Parts);
            case ClosureExpression closureExpression:
                return new FunctionValue(closureExpression.Parameters, closureExpression.Body, scope);
            case IfExpression ifExpression:
            {
                var conditionValue = EvaluateExpression(scope, ifExpression.Condition);

                return conditionValue switch
                {
                    BooleanValue { Value: true } => EvaluateExpression(scope, ifExpression.WhenTrue),
                    BooleanValue { Value: false } => EvaluateExpression(scope, ifExpression.WhenFalse),
                    _ => throw new RuntimeError($"Expected a Boolean in the if condition, but got {Format(conditionValue)}.")
                };
            }
            case MatchExpression matchExpression:
            {
                var scrutineeValue = EvaluateExpression(scope, matchExpression.Scrutinee);
                return EvaluateMatch(scope, scrutineeValue, matchExpression.Cases);
            }
            case ApplyExpression applyExpression:
            {
                var functionValue = EvaluateExpression(scope, applyExpression.Callee);
                var arguments = applyExpression.Arguments.Select(argument => EvaluateExpression(scope, argument)).ToList();
                return Apply(functionValue, arguments);
            }
            case UnaryExpression unaryExpression:
            {
                var operand = EvaluateExpression(scope, unaryExpression.Operand);

                if (HasExplicitUnqualifiedName(scope, unaryExpression.OperatorName))
                {
                    return Apply(ResolveName(scope, new[] { unaryExpression.OperatorName }), new List<KValue> { operand });
                }

                return ApplyBuiltinUnary(unaryExpression.OperatorName, operand);
            }
            case BinaryExpression binaryExpression when binaryExpression.OperatorName == "&&" && !HasExplicitUnqualifiedName(scope, "&&"):
            {
                var leftValue = EvaluateExpression(scope, binaryExpression.Left);

                return leftValue switch
                {
                    BooleanValue { Value: false } => new BooleanValue(false),
                    BooleanValue { Value: true } => EvaluateBooleanRightOperand(scope, binaryExpression.Right, "&&"),
                    _ => throw new RuntimeError($"Operator '&&' expects Boolean operands, but got {Format(leftValue)}.")
                };
            }
            case BinaryExpression binaryExpression when binaryExpression.OperatorName == "||" && !HasExplicitUnqualifiedName(scope, "||"):
            {
                var leftValue = EvaluateExpression(scope, binaryExpression.Left);

                return leftValue switch
                {
                    BooleanValue { Value: true } => new BooleanValue(true),
                    BooleanValue { Value: false } => EvaluateBooleanRightOperand(scope, binaryExpression.Right, "||"),
                    _ => throw new RuntimeError($"Operator '||' expects Boolean operands, but got {Format(leftValue)}.")
                };
            }
            case BinaryExpression binaryExpression:
            {
                var leftValue = EvaluateExpression(scope, binaryExpression.Left);
                var rightValue = EvaluateExpression(scope, binaryExpression.Right);

                if (HasExplicitUnqualifiedName(scope, binaryExpression.OperatorName))
                {
                    return Apply(
                        ResolveName(scope, new[] { binaryExpression.OperatorName }),
                        new List<KValue> { leftValue, rightValue });
                }

                return ApplyBuiltinBinary(binaryExpression.OperatorName, leftValue, rightValue);
            }
            default:
                throw new RuntimeError("Unknown backend expression.");
        }
    }

    private static KValue EvaluateMatch(RuntimeScope scope, KValue scrutineeValue, MatchCase[] cases)
    {
        foreach (var matchCase in cases)
        {
            var bindings = TryMatchPattern(scope, scrutineeValue, matchCase.Pattern);

            if (bindings is null)
            {
                continue;
            }

            var nextLocals = new Dictionary<string, KValue>(scope.Locals, StringComparer.Ordinal);

            foreach (var (name, value) in bindings)
            {
                nextLocals[name] = value;
            }

            var nextScope = new RuntimeScope(nextLocals, scope.CurrentModule, scope.Context);

            if (matchCase.Guard is not null)
            {
                var guardValue = EvaluateExpression(nextScope, matchCase.Guard);

                if (guardValue is not BooleanValue booleanValue)
                {
                    throw new RuntimeError($"Match guards must evaluate to Bool, but got {Format(guardValue)}.");
                }

                if (!booleanValue.Value)
                {
                    continue;
                }
            }

            return EvaluateExpression(nextScope, matchCase.Body);
        }

        throw new RuntimeError($"Non-exhaustive match for {Format(scrutineeValue)}.");
    }

    private static List<(string Name, KValue Value)>? TryMatchPattern(RuntimeScope scope, KValue value, Pattern pattern)
    {
        switch (pattern.Kind)
        {
            case PatternKind.Wildcard:
                return new List<(string Name, KValue Value)>();
            case PatternKind.Name:
                return new List<(string Name, KValue Value)> { (pattern.BoundName ?? string.Empty, value) };
            case PatternKind.Literal:
                return pattern.Literal is not null && ValuesEqual(value, LiteralToValue(pattern.Literal))
                    ? new List<(string Name, KValue Value)>()
                    : null;
            case PatternKind.Constructor:
            {
                var expectedConstructor = ResolvePatternConstructor(scope, pattern.Segments);

                if (value is BooleanValue booleanValue
                    && expectedConstructor.Arity == 0
                    && string.Equals(expectedConstructor.TypeName, "Bool", StringComparison.Ordinal)
                    && pattern.Arguments.Length == 0)
                {
                    var expectedValue = string.Equals(expectedConstructor.Name, "True", StringComparison.Ordinal);
                    return booleanValue.Value == expectedValue
                        ? new List<(string Name, KValue Value)>()
                        : null;
                }

                if (value is not ConstructedValue actualConstructedValue
                    || !string.Equals(expectedConstructor.QualifiedName, actualConstructedValue.Constructor.QualifiedName, StringComparison.Ordinal)
                    || pattern.Arguments.Length != actualConstructedValue.Fields.Count)
                {
                    return null;
                }

                var bindings = new List<(string Name, KValue Value)>();

                for (var index = 0; index < pattern.Arguments.Length; index++)
                {
                    var nestedBindings = TryMatchPattern(scope, actualConstructedValue.Fields[index], pattern.Arguments[index]);

                    if (nestedBindings is null)
                    {
                        return null;
                    }

                    bindings.AddRange(nestedBindings);
                }

                return bindings;
            }
            default:
                throw new RuntimeError("Unknown backend pattern.");
        }
    }

    private static KValue EvaluateBooleanRightOperand(RuntimeScope scope, KExpr expression, string operatorName)
    {
        var value = EvaluateExpression(scope, expression);

        return value switch
        {
            BooleanValue booleanValue => new BooleanValue(booleanValue.Value),
            _ => throw new RuntimeError($"Operator '{operatorName}' expects Boolean operands, but got {Format(value)}.")
        };
    }

    private static KValue EvaluatePrefixedString(RuntimeScope scope, string prefix, StringPart[] parts)
    {
        if (!string.Equals(prefix, "f", StringComparison.Ordinal))
        {
            throw new RuntimeError($"Prefixed string '{prefix}\"...\"' is not supported by the runtime yet.");
        }

        var segments = new List<string>();

        foreach (var part in parts)
        {
            if (part.Interpolation is null)
            {
                segments.Add(part.Text ?? string.Empty);
            }
            else
            {
                var value = EvaluateExpression(scope, part.Interpolation);
                segments.Add(RenderInterpolatedValue(value));
            }
        }

        return new StringValue(string.Concat(segments));
    }

    private static string RenderInterpolatedValue(KValue value) =>
        value switch
        {
            IntegerValue integerValue => integerValue.Value.ToString(),
            FloatValue floatValue => floatValue.Value.ToString(System.Globalization.CultureInfo.InvariantCulture),
            BooleanValue { Value: true } => "True",
            BooleanValue { Value: false } => "False",
            StringValue stringValue => stringValue.Value,
            CharacterValue characterValue => characterValue.Value.ToString(),
            UnitValue => "()",
            _ => throw new RuntimeError($"Cannot interpolate {Format(value)} into an f-string.")
        };

    private static KValue LiteralToValue(LiteralValue literal) =>
        literal.Kind switch
        {
            LiteralKind.Integer => new IntegerValue(literal.IntegerValue),
            LiteralKind.Float => new FloatValue(literal.FloatValue),
            LiteralKind.String => new StringValue(literal.StringValue ?? string.Empty),
            LiteralKind.Character => new CharacterValue(literal.CharacterValue),
            LiteralKind.Unit => UnitValue.Instance,
            _ => throw new RuntimeError("Unknown literal kind.")
        };

    private static KValue Apply(KValue functionValue, List<KValue> arguments) =>
        functionValue switch
        {
            FunctionValue closure => ApplyClosure(closure, arguments),
            ConstructorFunctionValue constructorValue => ApplyConstructor(constructorValue, arguments),
            BuiltinValue builtinValue => ApplyBuiltinFunction(builtinValue, arguments),
            _ => throw new RuntimeError($"Cannot apply {Format(functionValue)} as a function.")
        };

    private static KValue ApplyConstructor(ConstructorFunctionValue constructorValue, List<KValue> arguments)
    {
        var currentConstructor = new ConstructorFunctionValue(constructorValue.Constructor, constructorValue.Arguments.ToList());
        var remainingArguments = new Queue<KValue>(arguments);

        while (true)
        {
            if (remainingArguments.Count == 0)
            {
                return currentConstructor.Arguments.Count == currentConstructor.Constructor.Arity
                    ? new ConstructedValue(currentConstructor.Constructor, currentConstructor.Arguments.ToList())
                    : currentConstructor;
            }

            currentConstructor.Arguments.Add(remainingArguments.Dequeue());

            if (currentConstructor.Arguments.Count > currentConstructor.Constructor.Arity)
            {
                throw new RuntimeError($"Constructor '{currentConstructor.Constructor.Name}' received too many arguments.");
            }

            if (currentConstructor.Arguments.Count == currentConstructor.Constructor.Arity)
            {
                var value = new ConstructedValue(currentConstructor.Constructor, currentConstructor.Arguments.ToList());

                if (remainingArguments.Count == 0)
                {
                    return value;
                }

                return Apply(value, remainingArguments.ToList());
            }
        }
    }

    private static KValue ApplyClosure(FunctionValue closure, List<KValue> arguments)
    {
        var currentClosure = closure;
        var remainingArguments = new Queue<KValue>(arguments);

        while (true)
        {
            if (remainingArguments.Count == 0)
            {
                return currentClosure;
            }

            if (currentClosure.Parameters.Length == 0)
            {
                throw new RuntimeError("The function received too many arguments.");
            }

            var argument = remainingArguments.Dequeue();
            var parameter = currentClosure.Parameters[0];
            var nextLocals = new Dictionary<string, KValue>(currentClosure.Scope.Locals, StringComparer.Ordinal)
            {
                [parameter] = argument
            };

            if (currentClosure.Parameters.Length == 1)
            {
                var value = EvaluateExpression(new RuntimeScope(nextLocals, currentClosure.Scope.CurrentModule, currentClosure.Scope.Context), currentClosure.Body);

                if (remainingArguments.Count == 0)
                {
                    return value;
                }

                return Apply(value, remainingArguments.ToList());
            }

            currentClosure =
                new FunctionValue(
                    currentClosure.Parameters.Skip(1).ToArray(),
                    currentClosure.Body,
                    new RuntimeScope(nextLocals, currentClosure.Scope.CurrentModule, currentClosure.Scope.Context));
        }
    }

    private static KValue ApplyBuiltinFunction(BuiltinValue builtin, List<KValue> arguments)
    {
        var currentBuiltin = new BuiltinValue(builtin.Name, builtin.Arguments.ToList());
        var remainingArguments = new Queue<KValue>(arguments);

        while (true)
        {
            if (remainingArguments.Count == 0)
            {
                return currentBuiltin;
            }

            currentBuiltin.Arguments.Add(remainingArguments.Dequeue());
            var maybeValue = InvokeBuiltin(currentBuiltin);

            if (maybeValue is null)
            {
                continue;
            }

            if (remainingArguments.Count == 0)
            {
                return maybeValue;
            }

            return Apply(maybeValue, remainingArguments.ToList());
        }
    }

    private static KValue ExecuteIfIo(KValue value) =>
        value switch
        {
            IOActionValue ioActionValue => ioActionValue.Action(),
            _ => value
        };

    private static KValue? InvokeBuiltin(BuiltinValue builtin)
    {
        switch (builtin.Name)
        {
            case "not":
                return builtin.Arguments.Count switch
                {
                    0 => null,
                    1 when builtin.Arguments[0] is BooleanValue booleanValue => new BooleanValue(!booleanValue.Value),
                    1 => throw new RuntimeError($"Intrinsic 'not' expects a Bool, but got {Format(builtin.Arguments[0])}."),
                    _ => throw new RuntimeError("Intrinsic 'not' received too many arguments.")
                };
            case "negate":
                return builtin.Arguments.Count switch
                {
                    0 => null,
                    1 when builtin.Arguments[0] is IntegerValue integerValue => new IntegerValue(-integerValue.Value),
                    1 when builtin.Arguments[0] is FloatValue floatValue => new FloatValue(-floatValue.Value),
                    1 => throw new RuntimeError($"Intrinsic 'negate' expects a numeric value, but got {Format(builtin.Arguments[0])}."),
                    _ => throw new RuntimeError("Intrinsic 'negate' received too many arguments.")
                };
            case "and":
                return builtin.Arguments.Count switch
                {
                    < 2 => null,
                    2 when builtin.Arguments[0] is BooleanValue left && builtin.Arguments[1] is BooleanValue right => new BooleanValue(left.Value && right.Value),
                    2 => throw new RuntimeError($"Intrinsic 'and' expects Bool arguments, but got {Format(builtin.Arguments[0])} and {Format(builtin.Arguments[1])}."),
                    _ => throw new RuntimeError("Intrinsic 'and' received too many arguments.")
                };
            case "or":
                return builtin.Arguments.Count switch
                {
                    < 2 => null,
                    2 when builtin.Arguments[0] is BooleanValue left && builtin.Arguments[1] is BooleanValue right => new BooleanValue(left.Value || right.Value),
                    2 => throw new RuntimeError($"Intrinsic 'or' expects Bool arguments, but got {Format(builtin.Arguments[0])} and {Format(builtin.Arguments[1])}."),
                    _ => throw new RuntimeError("Intrinsic 'or' received too many arguments.")
                };
            case "pure":
                return builtin.Arguments.Count switch
                {
                    0 => null,
                    1 => new IOActionValue(() => builtin.Arguments[0]),
                    _ => throw new RuntimeError("Intrinsic 'pure' received too many arguments.")
                };
            case ">>=":
                return builtin.Arguments.Count switch
                {
                    < 2 => null,
                    2 => new IOActionValue(() =>
                    {
                        var result = ExecuteIfIo(builtin.Arguments[0]);
                        var next = Apply(builtin.Arguments[1], new List<KValue> { result });
                        return ExecuteIfIo(next);
                    }),
                    _ => throw new RuntimeError("Intrinsic '>>=' received too many arguments.")
                };
            case ">>":
                return builtin.Arguments.Count switch
                {
                    < 2 => null,
                    2 => new IOActionValue(() =>
                    {
                        _ = ExecuteIfIo(builtin.Arguments[0]);
                        return ExecuteIfIo(builtin.Arguments[1]);
                    }),
                    _ => throw new RuntimeError("Intrinsic '>>' received too many arguments.")
                };
            case "print":
                return builtin.Arguments.Count switch
                {
                    0 => null,
                    1 when builtin.Arguments[0] is StringValue stringValue => new IOActionValue(() =>
                    {
                        Console.Write(stringValue.Value);
                        return UnitValue.Instance;
                    }),
                    1 => throw new RuntimeError($"Intrinsic 'print' expects a String, but got {Format(builtin.Arguments[0])}."),
                    _ => throw new RuntimeError("Intrinsic 'print' received too many arguments.")
                };
            case "println":
                return builtin.Arguments.Count switch
                {
                    0 => null,
                    1 when builtin.Arguments[0] is StringValue stringValue => new IOActionValue(() =>
                    {
                        Console.WriteLine(stringValue.Value);
                        return UnitValue.Instance;
                    }),
                    1 => throw new RuntimeError($"Intrinsic 'println' expects a String, but got {Format(builtin.Arguments[0])}."),
                    _ => throw new RuntimeError("Intrinsic 'println' received too many arguments.")
                };
            case "printInt":
                return builtin.Arguments.Count switch
                {
                    0 => null,
                    1 when builtin.Arguments[0] is IntegerValue integerValue => new IOActionValue(() =>
                    {
                        Console.WriteLine(integerValue.Value);
                        return UnitValue.Instance;
                    }),
                    1 => throw new RuntimeError($"Intrinsic 'printInt' expects an Int, but got {Format(builtin.Arguments[0])}."),
                    _ => throw new RuntimeError("Intrinsic 'printInt' received too many arguments.")
                };
            case "unsafeConsume":
                return builtin.Arguments.Count switch
                {
                    0 => null,
                    1 => UnitValue.Instance,
                    _ => throw new RuntimeError("Intrinsic 'unsafeConsume' received too many arguments.")
                };
            default:
                if (BuiltinBinaryNames.Contains(builtin.Name))
                {
                    if (builtin.Arguments.Count < 2)
                    {
                        return null;
                    }

                    if (builtin.Arguments.Count > 2)
                    {
                        throw new RuntimeError($"Operator '{builtin.Name}' received too many arguments.");
                    }

                    return ApplyBuiltinBinary(builtin.Name, builtin.Arguments[0], builtin.Arguments[1]);
                }

                throw new RuntimeError($"Unknown builtin '{builtin.Name}'.");
        }
    }

    private static KValue ApplyBuiltinUnary(string operatorName, KValue operand) =>
        (operatorName, operand) switch
        {
            ("-", IntegerValue integerValue) => new IntegerValue(-integerValue.Value),
            ("-", FloatValue floatValue) => new FloatValue(-floatValue.Value),
            ("-", _) => throw new RuntimeError($"Unary '{operatorName}' expects a numeric value, but got {Format(operand)}."),
            _ => throw new RuntimeError($"Unary operator '{operatorName}' is not supported.")
        };

    private static KValue ApplyBuiltinBinary(string operatorName, KValue left, KValue right) =>
        (operatorName, left, right) switch
        {
            ("+", IntegerValue leftValue, IntegerValue rightValue) => new IntegerValue(leftValue.Value + rightValue.Value),
            ("-", IntegerValue leftValue, IntegerValue rightValue) => new IntegerValue(leftValue.Value - rightValue.Value),
            ("*", IntegerValue leftValue, IntegerValue rightValue) => new IntegerValue(leftValue.Value * rightValue.Value),
            ("/", IntegerValue leftValue, IntegerValue rightValue) when rightValue.Value == 0L => throw new RuntimeError("Division by zero."),
            ("/", IntegerValue leftValue, IntegerValue rightValue) => new IntegerValue(leftValue.Value / rightValue.Value),
            ("+", FloatValue leftValue, FloatValue rightValue) => new FloatValue(leftValue.Value + rightValue.Value),
            ("-", FloatValue leftValue, FloatValue rightValue) => new FloatValue(leftValue.Value - rightValue.Value),
            ("*", FloatValue leftValue, FloatValue rightValue) => new FloatValue(leftValue.Value * rightValue.Value),
            ("/", FloatValue leftValue, FloatValue rightValue) => new FloatValue(leftValue.Value / rightValue.Value),
            ("==", _, _) => new BooleanValue(ValuesEqual(left, right)),
            ("!=", _, _) => new BooleanValue(!ValuesEqual(left, right)),
            ("<", IntegerValue leftValue, IntegerValue rightValue) => new BooleanValue(leftValue.Value < rightValue.Value),
            ("<=", IntegerValue leftValue, IntegerValue rightValue) => new BooleanValue(leftValue.Value <= rightValue.Value),
            (">", IntegerValue leftValue, IntegerValue rightValue) => new BooleanValue(leftValue.Value > rightValue.Value),
            (">=", IntegerValue leftValue, IntegerValue rightValue) => new BooleanValue(leftValue.Value >= rightValue.Value),
            ("<", FloatValue leftValue, FloatValue rightValue) => new BooleanValue(leftValue.Value < rightValue.Value),
            ("<=", FloatValue leftValue, FloatValue rightValue) => new BooleanValue(leftValue.Value <= rightValue.Value),
            (">", FloatValue leftValue, FloatValue rightValue) => new BooleanValue(leftValue.Value > rightValue.Value),
            (">=", FloatValue leftValue, FloatValue rightValue) => new BooleanValue(leftValue.Value >= rightValue.Value),
            ("&&", BooleanValue leftValue, BooleanValue rightValue) => new BooleanValue(leftValue.Value && rightValue.Value),
            ("||", BooleanValue leftValue, BooleanValue rightValue) => new BooleanValue(leftValue.Value || rightValue.Value),
            _ => throw new RuntimeError($"Operator '{operatorName}' is not supported for {Format(left)} and {Format(right)}.")
        };

    private static bool ValuesEqual(KValue left, KValue right) =>
        (left, right) switch
        {
            (IntegerValue leftValue, IntegerValue rightValue) => leftValue.Value == rightValue.Value,
            (FloatValue leftValue, FloatValue rightValue) => leftValue.Value.Equals(rightValue.Value),
            (BooleanValue leftValue, BooleanValue rightValue) => leftValue.Value == rightValue.Value,
            (StringValue leftValue, StringValue rightValue) => string.Equals(leftValue.Value, rightValue.Value, StringComparison.Ordinal),
            (CharacterValue leftValue, CharacterValue rightValue) => leftValue.Value == rightValue.Value,
            (UnitValue, UnitValue) => true,
            (ConstructedValue leftValue, ConstructedValue rightValue)
                when string.Equals(leftValue.Constructor.QualifiedName, rightValue.Constructor.QualifiedName, StringComparison.Ordinal)
                     && leftValue.Fields.Count == rightValue.Fields.Count =>
                leftValue.Fields.Zip(rightValue.Fields).All(pair => ValuesEqual(pair.First, pair.Second)),
            _ => false
        };

    private static KValue ResolveName(RuntimeScope scope, string[] segments)
    {
        if (segments.Length == 0)
        {
            throw new RuntimeError("Encountered an empty name.");
        }

        if (segments.Length == 1)
        {
            return ResolveUnqualifiedName(scope, segments[0]);
        }

        var qualifierSegments = segments.Take(segments.Length - 1).ToArray();
        var bindingName = segments[^1];
        return ResolveQualifiedName(scope, qualifierSegments, bindingName);
    }

    private static bool HasExplicitUnqualifiedName(RuntimeScope scope, string name)
    {
        var currentModule = scope.Context.Modules[scope.CurrentModule];

        return scope.Locals.ContainsKey(name)
            || currentModule.Definitions.ContainsKey(name)
            || currentModule.Constructors.ContainsKey(name)
            || FindImportedModulesForName(scope, name).Count > 0;
    }

    private static KValue ResolveUnqualifiedName(RuntimeScope scope, string name)
    {
        if (scope.Locals.TryGetValue(name, out var localValue))
        {
            return localValue;
        }

        var currentModule = scope.Context.Modules[scope.CurrentModule];

        if (currentModule.Definitions.ContainsKey(name))
        {
            return ForceBinding(currentModule, name);
        }

        if (currentModule.Constructors.ContainsKey(name))
        {
            return ForceBinding(currentModule, name);
        }

        var matches = FindImportedModulesForName(scope, name);

        return matches.Count switch
        {
            0 => TryCreateBuiltinFunction(name) ?? throw new RuntimeError($"Name '{name}' is not in scope."),
            1 => ForceBinding(matches[0], name),
            _ => throw new RuntimeError($"Name '{name}' is ambiguous across imported modules.")
        };
    }

    private static bool ItemImportsTermName(ImportItem item)
    {
        return item.Namespace == ImportNamespaceKind.None
            || item.Namespace == ImportNamespaceKind.Term;
    }

    private static bool ItemImportsConstructorName(ImportItem item)
    {
        return item.Namespace == ImportNamespaceKind.Constructor;
    }

    private static bool ItemImportsConstructorsOfType(ImportItem item, string typeName)
    {
        return item.IncludeConstructors
            && item.Namespace == ImportNamespaceKind.Type
            && string.Equals(item.Name, typeName, StringComparison.Ordinal);
    }

    private static bool ExceptMatches(ExceptItem item, ImportNamespaceKind @namespace, string name)
    {
        return string.Equals(item.Name, name, StringComparison.Ordinal)
            && (item.Namespace == ImportNamespaceKind.None || item.Namespace == @namespace);
    }

    private static bool SelectionImportsTermName(RuntimeModule importedModule, ImportSelection selection, string name)
    {
        var exportsTerm =
            importedModule.Definitions.ContainsKey(name)
            || importedModule.IntrinsicTerms.Contains(name);

        return selection.Kind switch
        {
            ImportSelectionKind.QualifiedOnly => false,
            ImportSelectionKind.Items => selection.Items.Any(item => string.Equals(item.Name, name, StringComparison.Ordinal) && ItemImportsTermName(item)),
            ImportSelectionKind.All => exportsTerm && importedModule.Exports.Contains(name),
            ImportSelectionKind.AllExcept => !selection.ExcludedItems.Any(item => ExceptMatches(item, ImportNamespaceKind.Term, name)) && exportsTerm && importedModule.Exports.Contains(name),
            _ => false
        };
    }

    private static bool SelectionImportsConstructorName(RuntimeModule importedModule, ImportSelection selection, string name)
    {
        var exportsConstructor =
            importedModule.Constructors.ContainsKey(name)
            && importedModule.Exports.Contains(name);

        return selection.Kind switch
        {
            ImportSelectionKind.QualifiedOnly => false,
            ImportSelectionKind.Items => exportsConstructor && selection.Items.Any(item =>
                (string.Equals(item.Name, name, StringComparison.Ordinal) && ItemImportsConstructorName(item))
                || ItemImportsConstructorsOfType(item, importedModule.Constructors[name].TypeName)),
            ImportSelectionKind.All => false,
            ImportSelectionKind.AllExcept => false,
            _ => false
        };
    }

    private static List<RuntimeModule> FindImportedModulesForName(RuntimeScope scope, string name)
    {
        var currentModule = scope.Context.Modules[scope.CurrentModule];
        var matches = new Dictionary<string, RuntimeModule>(StringComparer.Ordinal);

        foreach (var spec in currentModule.Imports)
        {
            if (spec.Source.IsUrl)
            {
                continue;
            }

            var importedModuleName = string.Join(".", spec.Source.Segments);

            if (!scope.Context.Modules.TryGetValue(importedModuleName, out var importedModule))
            {
                continue;
            }

            var importsName =
                SelectionImportsTermName(importedModule, spec.Selection, name)
                || SelectionImportsConstructorName(importedModule, spec.Selection, name);

            if (importsName)
            {
                matches[importedModuleName] = importedModule;
            }
        }

        return matches.Values.ToList();
    }

    private static KValue ResolveQualifiedName(RuntimeScope scope, string[] qualifierSegments, string bindingName)
    {
        var qualifierText = string.Join(".", qualifierSegments);
        var currentModule = scope.Context.Modules[scope.CurrentModule];
        RuntimeModule? targetModule = null;

        if (string.Equals(qualifierText, scope.CurrentModule, StringComparison.Ordinal))
        {
            targetModule = currentModule;
        }
        else
        {
            foreach (var spec in currentModule.Imports)
            {
                if (spec.Source.IsUrl || spec.Selection.Kind != ImportSelectionKind.QualifiedOnly)
                {
                    continue;
                }

                if (spec.Alias is not null
                    && qualifierSegments.Length == 1
                    && string.Equals(spec.Alias, qualifierSegments[0], StringComparison.Ordinal)
                    && scope.Context.Modules.TryGetValue(string.Join(".", spec.Source.Segments), out var aliasedModule))
                {
                    targetModule = aliasedModule;
                    break;
                }

                if (qualifierSegments.SequenceEqual(spec.Source.Segments) && scope.Context.Modules.TryGetValue(string.Join(".", spec.Source.Segments), out var importedModule))
                {
                    targetModule = importedModule;
                    break;
                }
            }
        }

        if (targetModule is null)
        {
            throw new RuntimeError($"Module qualifier '{qualifierText}' is not in scope.");
        }

        if (targetModule.Name == scope.CurrentModule || targetModule.Exports.Contains(bindingName))
        {
            return ForceBinding(targetModule, bindingName);
        }

        throw new RuntimeError($"'{bindingName}' is not exported from module '{targetModule.Name}'.");
    }

    private static RuntimeConstructor ResolvePatternConstructor(RuntimeScope scope, string[] segments)
    {
        if (segments.Length == 0)
        {
            throw new RuntimeError("Encountered an empty constructor pattern.");
        }

        var currentModule = scope.Context.Modules[scope.CurrentModule];

        if (segments.Length == 1)
        {
            var name = segments[0];

            if (currentModule.Constructors.TryGetValue(name, out var localConstructor))
            {
                return localConstructor;
            }

            var matches = new Dictionary<string, RuntimeModule>(StringComparer.Ordinal);

            foreach (var spec in currentModule.Imports)
            {
                if (spec.Source.IsUrl)
                {
                    continue;
                }

                var importedModuleName = string.Join(".", spec.Source.Segments);

                if (!scope.Context.Modules.TryGetValue(importedModuleName, out var importedModule))
                {
                    continue;
                }

                if (SelectionImportsConstructorName(importedModule, spec.Selection, name))
                {
                    matches[importedModuleName] = importedModule;
                }
            }

            return matches.Count switch
            {
                0 => throw new RuntimeError($"Pattern '{name}' is not in scope as a constructor."),
                1 => matches.Values.Single().Constructors[name],
                _ => throw new RuntimeError($"Pattern constructor '{name}' is ambiguous across imported modules.")
            };
        }

        var qualifierSegments = segments.Take(segments.Length - 1).ToArray();
        var constructorName = segments[^1];
        var qualifierText = string.Join(".", qualifierSegments);
        RuntimeModule? targetModule = null;

        if (string.Equals(qualifierText, scope.CurrentModule, StringComparison.Ordinal))
        {
            targetModule = currentModule;
        }
        else
        {
            foreach (var spec in currentModule.Imports)
            {
                if (spec.Source.IsUrl || spec.Selection.Kind != ImportSelectionKind.QualifiedOnly)
                {
                    continue;
                }

                if (spec.Alias is not null
                    && qualifierSegments.Length == 1
                    && string.Equals(spec.Alias, qualifierSegments[0], StringComparison.Ordinal)
                    && scope.Context.Modules.TryGetValue(string.Join(".", spec.Source.Segments), out var aliasedModule))
                {
                    targetModule = aliasedModule;
                    break;
                }

                if (qualifierSegments.SequenceEqual(spec.Source.Segments)
                    && scope.Context.Modules.TryGetValue(string.Join(".", spec.Source.Segments), out var importedModule))
                {
                    targetModule = importedModule;
                    break;
                }
            }
        }

        if (targetModule is null)
        {
            throw new RuntimeError($"Module qualifier '{qualifierText}' is not in scope.");
        }

        if (targetModule.Name != scope.CurrentModule && !targetModule.Exports.Contains(constructorName))
        {
            throw new RuntimeError($"'{constructorName}' is not exported from module '{targetModule.Name}'.");
        }

        if (targetModule.Constructors.TryGetValue(constructorName, out var constructor))
        {
            return constructor;
        }

        throw new RuntimeError($"Pattern '{string.Join(".", segments)}' does not resolve to a constructor.");
    }

    private static KValue ForceBinding(RuntimeModule runtimeModule, string bindingName)
    {
        if (runtimeModule.Values.TryGetValue(bindingName, out var lazyValue))
        {
            try
            {
                return lazyValue.Value;
            }
            catch (InvalidOperationException)
            {
                throw new RuntimeError($"Recursive evaluation of '{runtimeModule.Name}.{bindingName}' is not supported for non-function values.");
            }
        }

        if (runtimeModule.Constructors.TryGetValue(bindingName, out var constructor))
        {
            return constructor.Arity == 0
                ? new ConstructedValue(constructor, new List<KValue>())
                : new ConstructorFunctionValue(constructor);
        }

        throw new RuntimeError($"Binding '{bindingName}' was not found in module '{runtimeModule.Name}'.");
    }

    private static KValue? TryCreateBuiltinFunction(string name) =>
        BuiltinBinaryNames.Contains(name)
            ? new BuiltinValue(name)
            : null;

    private static KValue CreateIntrinsicTermValue(string moduleName, string name)
    {
        if (!string.Equals(moduleName, "std.prelude", StringComparison.Ordinal))
        {
            throw new RuntimeError($"Intrinsic term '{name}' is not implemented for module '{moduleName}'.");
        }

        return name switch
        {
            "True" => new BooleanValue(true),
            "False" => new BooleanValue(false),
            "pure" => new BuiltinValue("pure"),
            ">>=" => new BuiltinValue(">>="),
            ">>" => new BuiltinValue(">>"),
            "not" => new BuiltinValue("not"),
            "and" => new BuiltinValue("and"),
            "or" => new BuiltinValue("or"),
            "negate" => new BuiltinValue("negate"),
            "println" => new BuiltinValue("println"),
            "print" => new BuiltinValue("print"),
            "printInt" => new BuiltinValue("printInt"),
            "unsafeConsume" => new BuiltinValue("unsafeConsume"),
            _ => throw new RuntimeError($"Intrinsic term '{name}' is not implemented for module '{moduleName}'.")
        };
    }
}
