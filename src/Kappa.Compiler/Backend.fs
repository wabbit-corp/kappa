namespace Kappa.Compiler

open System
open System.Globalization
open System.IO
open System.Text

type DotNetDeployment =
    | Managed
    | NativeAot

type DotNetArtifact =
    { ProjectDirectory: string
      ProjectFilePath: string
      ProgramFilePath: string
      RuntimeFilePath: string
      GeneratedFilePath: string
      EntryPoint: string
      Deployment: DotNetDeployment }

module Backend =
    let private csharpString (value: string) =
        let builder = StringBuilder()
        builder.Append('"') |> ignore

        for ch in value do
            match ch with
            | '\\' -> builder.Append("\\\\") |> ignore
            | '"' -> builder.Append("\\\"") |> ignore
            | '\r' -> builder.Append("\\r") |> ignore
            | '\n' -> builder.Append("\\n") |> ignore
            | '\t' -> builder.Append("\\t") |> ignore
            | _ when Char.IsControl(ch) ->
                builder.Append($"\\u{int ch:x4}") |> ignore
            | _ ->
                builder.Append(ch) |> ignore

        builder.Append('"') |> ignore
        builder.ToString()

    let private csharpChar (value: char) =
        match value with
        | '\\' -> "'\\\\'"
        | '\'' -> "'\\''"
        | '\r' -> "'\\r'"
        | '\n' -> "'\\n'"
        | '\t' -> "'\\t'"
        | _ when Char.IsControl(value) -> $"'\\u{int value:x4}'"
        | _ -> $"'{value}'"

    let private emitStringArray (values: string list) =
        match values with
        | [] -> "System.Array.Empty<string>()"
        | _ ->
            values
            |> List.map csharpString
            |> String.concat ", "
            |> fun body -> $"new[] {{ {body} }}"

    let private emitImportNamespace namespaceOption =
        match namespaceOption with
        | None -> "ImportNamespaceKind.None"
        | Some ImportNamespace.Term -> "ImportNamespaceKind.Term"
        | Some ImportNamespace.Type -> "ImportNamespaceKind.Type"
        | Some ImportNamespace.Trait -> "ImportNamespaceKind.Trait"
        | Some ImportNamespace.Constructor -> "ImportNamespaceKind.Constructor"

    let private emitImportItem (item: ImportItem) =
        $"new ImportItem({emitImportNamespace item.Namespace}, {csharpString item.Name})"

    let private emitImportSelection selection =
        match selection with
        | QualifiedOnly ->
            "ImportSelection.QualifiedOnly()"
        | Items items ->
            match items with
            | [] ->
                "ImportSelection.FromItems(System.Array.Empty<ImportItem>())"
            | _ ->
                items
                |> List.map emitImportItem
                |> String.concat ", "
                |> fun body -> $"ImportSelection.FromItems({body})"
        | All ->
            "ImportSelection.All()"
        | AllExcept names ->
            match names with
            | [] ->
                "ImportSelection.AllExcept(System.Array.Empty<string>())"
            | _ ->
                names
                |> emitStringArray
                |> fun body -> $"ImportSelection.AllExcept({body})"

    let private emitModuleSpecifier specifier =
        match specifier with
        | Dotted segments ->
            $"ModuleSpecifier.Dotted({emitStringArray segments})"
        | Url url ->
            $"ModuleSpecifier.FromUrl({csharpString url})"

    let private emitImportSpec (spec: ImportSpec) =
        let aliasCode =
            spec.Alias
            |> Option.map csharpString
            |> Option.defaultValue "null"

        $"new ImportSpec({emitModuleSpecifier spec.Source}, {aliasCode}, {emitImportSelection spec.Selection})"

    let private emitLiteralValue literal =
        match literal with
        | LiteralValue.Integer value ->
            $"LiteralValue.FromInteger({value}L)"
        | LiteralValue.Float value ->
            let text = value.ToString("R", CultureInfo.InvariantCulture)
            $"LiteralValue.FromFloat({text})"
        | LiteralValue.String value ->
            $"LiteralValue.FromString({csharpString value})"
        | LiteralValue.Character value ->
            $"LiteralValue.FromCharacter({csharpChar value})"
        | LiteralValue.Unit ->
            "LiteralValue.Unit()"

    let rec private emitBackendExpression expression =
        match expression with
        | KBackendLiteral literal ->
            $"new LiteralExpression({emitLiteralValue literal})"
        | KBackendName segments ->
            $"new NameExpression({emitStringArray segments})"
        | KBackendClosure(parameters, body) ->
            $"new ClosureExpression({emitStringArray parameters}, {emitBackendExpression body})"
        | KBackendIfThenElse(condition, whenTrue, whenFalse) ->
            $"new IfExpression({emitBackendExpression condition}, {emitBackendExpression whenTrue}, {emitBackendExpression whenFalse})"
        | KBackendApply(callee, arguments) ->
            let argumentsCode =
                match arguments with
                | [] -> "System.Array.Empty<KExpr>()"
                | _ ->
                    arguments
                    |> List.map emitBackendExpression
                    |> String.concat ", "
                    |> fun body -> $"new KExpr[] {{ {body} }}"

            $"new ApplyExpression({emitBackendExpression callee}, {argumentsCode})"
        | KBackendUnary(operatorName, operand) ->
            $"new UnaryExpression({csharpString operatorName}, {emitBackendExpression operand})"
        | KBackendBinary(left, operatorName, right) ->
            $"new BinaryExpression({emitBackendExpression left}, {csharpString operatorName}, {emitBackendExpression right})"
        | KBackendPrefixedString(prefix, parts) ->
            let partsCode =
                match parts with
                | [] -> "System.Array.Empty<StringPart>()"
                | _ ->
                    parts
                    |> List.map (function
                        | KBackendStringText text -> $"StringPart.FromText({csharpString text})"
                        | KBackendStringInterpolation inner -> $"StringPart.FromInterpolation({emitBackendExpression inner})")
                    |> String.concat ", "
                    |> fun body -> $"new StringPart[] {{ {body} }}"

            $"new PrefixedStringExpression({csharpString prefix}, {partsCode})"

    let private runtimeSource =
        """
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

    public ImportItem(ImportNamespaceKind @namespace, string name)
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
    public string[] ExcludedNames { get; }

    private ImportSelection(ImportSelectionKind kind, ImportItem[] items, string[] excludedNames)
    {
        Kind = kind;
        Items = items;
        ExcludedNames = excludedNames;
    }

    public static ImportSelection QualifiedOnly() => new(ImportSelectionKind.QualifiedOnly, Array.Empty<ImportItem>(), Array.Empty<string>());
    public static ImportSelection FromItems(params ImportItem[] items) => new(ImportSelectionKind.Items, items, Array.Empty<string>());
    public static ImportSelection All() => new(ImportSelectionKind.All, Array.Empty<ImportItem>(), Array.Empty<string>());
    public static ImportSelection AllExcept(params string[] excludedNames) => new(ImportSelectionKind.AllExcept, Array.Empty<ImportItem>(), excludedNames);
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

internal sealed class RuntimeModuleSpec
{
    public string Name { get; }
    public ImportSpec[] Imports { get; }
    public string[] Exports { get; }
    public string[] IntrinsicTerms { get; }
    public BindingSpec[] Bindings { get; }

    public RuntimeModuleSpec(string name, ImportSpec[] imports, string[] exports, string[] intrinsicTerms, BindingSpec[] bindings)
    {
        Name = name;
        Imports = imports;
        Exports = exports;
        IntrinsicTerms = intrinsicTerms;
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
    public HashSet<string> IntrinsicTerms { get; }
    public HashSet<string> Exports { get; }
    public List<ImportSpec> Imports { get; }
    public Dictionary<string, Lazy<KValue>> Values { get; }

    public RuntimeModule(string name, Dictionary<string, BindingSpec> definitions, HashSet<string> intrinsicTerms, HashSet<string> exports, List<ImportSpec> imports)
    {
        Name = name;
        Definitions = definitions;
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
        return ForceBinding(context.Modules[moduleName], bindingName);
    }

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

                    return new RuntimeModule(
                        spec.Name,
                        definitions,
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
            BuiltinValue builtinValue => ApplyBuiltinFunction(builtinValue, arguments),
            _ => throw new RuntimeError($"Cannot apply {Format(functionValue)} as a function.")
        };

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
                    1 => throw new RuntimeError("Intrinsic 'pure' is declared in std.prelude but is not executable in the runtime yet."),
                    _ => throw new RuntimeError("Intrinsic 'pure' received too many arguments.")
                };
            case ">>=":
                return builtin.Arguments.Count switch
                {
                    < 2 => null,
                    2 => throw new RuntimeError("Intrinsic '>>=' is declared in std.prelude but is not executable in the runtime yet."),
                    _ => throw new RuntimeError("Intrinsic '>>=' received too many arguments.")
                };
            case ">>":
                return builtin.Arguments.Count switch
                {
                    < 2 => null,
                    2 => throw new RuntimeError("Intrinsic '>>' is declared in std.prelude but is not executable in the runtime yet."),
                    _ => throw new RuntimeError("Intrinsic '>>' received too many arguments.")
                };
            case "print":
                return builtin.Arguments.Count switch
                {
                    0 => null,
                    1 => throw new RuntimeError("Intrinsic 'print' is declared in std.prelude but is not executable in the runtime yet."),
                    _ => throw new RuntimeError("Intrinsic 'print' received too many arguments.")
                };
            case "println":
                return builtin.Arguments.Count switch
                {
                    0 => null,
                    1 => throw new RuntimeError("Intrinsic 'println' is declared in std.prelude but is not executable in the runtime yet."),
                    _ => throw new RuntimeError("Intrinsic 'println' received too many arguments.")
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

        var matches = FindImportedModulesForName(scope, name);

        return matches.Count switch
        {
            0 => TryCreateBuiltinFunction(name) ?? throw new RuntimeError($"Name '{name}' is not in scope."),
            1 => ForceBinding(matches[0], name),
            _ => throw new RuntimeError($"Name '{name}' is ambiguous across imported modules.")
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
                spec.Selection.Kind switch
                {
                    ImportSelectionKind.QualifiedOnly => false,
                    ImportSelectionKind.Items => spec.Selection.Items.Any(item => string.Equals(item.Name, name, StringComparison.Ordinal) && (item.Namespace == ImportNamespaceKind.None || item.Namespace == ImportNamespaceKind.Term)),
                    ImportSelectionKind.All => importedModule.Exports.Contains(name),
                    ImportSelectionKind.AllExcept => !spec.Selection.ExcludedNames.Contains(name, StringComparer.Ordinal) && importedModule.Exports.Contains(name),
                    _ => false
                };

            if (importsName && importedModule.Exports.Contains(name))
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

    private static KValue ForceBinding(RuntimeModule runtimeModule, string bindingName)
    {
        if (!runtimeModule.Values.TryGetValue(bindingName, out var lazyValue))
        {
            throw new RuntimeError($"Binding '{bindingName}' was not found in module '{runtimeModule.Name}'.");
        }

        try
        {
            return lazyValue.Value;
        }
        catch (InvalidOperationException)
        {
            throw new RuntimeError($"Recursive evaluation of '{runtimeModule.Name}.{bindingName}' is not supported for non-function values.");
        }
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
            _ => throw new RuntimeError($"Intrinsic term '{name}' is not implemented for module '{moduleName}'.")
        };
    }
}
"""

    let private projectSource deployment =
        let publishAotProperty =
            match deployment with
            | Managed -> ""
            | NativeAot -> "    <PublishAot>true</PublishAot>\n"

        [
            "<Project Sdk=\"Microsoft.NET.Sdk\">"
            "  <PropertyGroup>"
            "    <OutputType>Exe</OutputType>"
            "    <TargetFramework>net10.0</TargetFramework>"
            "    <ImplicitUsings>enable</ImplicitUsings>"
            "    <Nullable>enable</Nullable>"
            publishAotProperty.TrimEnd('\n')
            "  </PropertyGroup>"
            "</Project>"
        ]
        |> List.filter (String.IsNullOrWhiteSpace >> not)
        |> String.concat Environment.NewLine

    let private programSource entryPoint =
        [
            "using Kappa.Generated;"
            ""
            "try"
            "{"
            $"    var value = KappaRunner.Run(GeneratedProgram.CreateModules(), {csharpString entryPoint});"
            "    Console.WriteLine(KappaRunner.Format(value));"
            "    return 0;"
            "}"
            "catch (RuntimeError error)"
            "{"
            "    Console.Error.WriteLine($\"runtime error: {error.Message}\");"
            "    return 1;"
            "}"
        ]
        |> String.concat Environment.NewLine

    let private appendArray (builder: StringBuilder) (itemType: string) (items: string list) =
        match items with
        | [] ->
            builder.Append($"System.Array.Empty<{itemType}>()") |> ignore
        | _ ->
            builder.Append($"new {itemType}[] {{ ") |> ignore
            items |> List.iteri (fun index item ->
                if index > 0 then
                    builder.Append(", ") |> ignore

                builder.Append(item) |> ignore)
            builder.Append(" }") |> ignore

    let private appendBinding (builder: StringBuilder) (binding: KBackendBinding) =
        builder.Append("new BindingSpec(") |> ignore
        builder.Append(csharpString binding.Name).Append(", ") |> ignore
        binding.Parameters |> List.map csharpString |> appendArray builder "string"
        builder.Append(", ") |> ignore

        match binding.Body with
        | Some body ->
            builder.Append(emitBackendExpression body) |> ignore
        | None ->
            builder.Append("null") |> ignore

        builder.Append(", ").Append(if binding.Intrinsic then "true" else "false").Append(")") |> ignore

    let private appendModule (builder: StringBuilder) (moduleDump: KBackendModule) =
        builder.Append("            [") |> ignore
        builder.Append(csharpString moduleDump.Name).Append("] = new RuntimeModuleSpec(") |> ignore
        builder.Append(csharpString moduleDump.Name).Append(", ") |> ignore

        moduleDump.Imports
        |> List.map emitImportSpec
        |> appendArray builder "ImportSpec"

        builder.Append(", ") |> ignore
        moduleDump.Exports |> List.map csharpString |> appendArray builder "string"
        builder.Append(", ") |> ignore
        moduleDump.IntrinsicTerms |> List.map csharpString |> appendArray builder "string"
        builder.Append(", ") |> ignore

        builder.Append("new BindingSpec[] { ") |> ignore
        moduleDump.Bindings
        |> List.iteri (fun index binding ->
            if index > 0 then
                builder.Append(", ") |> ignore

            appendBinding builder binding)

        builder.Append(" })") |> ignore

    let private generatedSource (workspace: WorkspaceCompilation) =
        let builder = StringBuilder()
        builder.AppendLine("using System;") |> ignore
        builder.AppendLine("using System.Collections.Generic;") |> ignore
        builder.AppendLine() |> ignore
        builder.AppendLine("namespace Kappa.Generated;") |> ignore
        builder.AppendLine() |> ignore
        builder.AppendLine("internal static class GeneratedProgram") |> ignore
        builder.AppendLine("{") |> ignore
        builder.AppendLine("    public static IReadOnlyDictionary<string, RuntimeModuleSpec> CreateModules() =>") |> ignore
        builder.AppendLine("        new Dictionary<string, RuntimeModuleSpec>(StringComparer.Ordinal)") |> ignore
        builder.AppendLine("        {") |> ignore

        workspace.KBackendIR
        |> List.sortBy (fun moduleDump -> moduleDump.Name)
        |> List.iteri (fun index moduleDump ->
            if index > 0 then
                builder.AppendLine(",") |> ignore

            appendModule builder moduleDump)

        builder.AppendLine() |> ignore
        builder.AppendLine("        };") |> ignore
        builder.AppendLine("}") |> ignore
        builder.ToString()

    let private resolveEntryPoint (workspace: WorkspaceCompilation) (entryPoint: string) =
        let segments =
            entryPoint.Split('.', StringSplitOptions.RemoveEmptyEntries)
            |> Array.toList

        match segments with
        | [] ->
            Result.Error "Expected a binding name to run."
        | [ bindingName ] ->
            let matches =
                workspace.KBackendIR
                |> List.choose (fun moduleDump ->
                    if moduleDump.Bindings |> List.exists (fun binding -> String.Equals(binding.Name, bindingName, StringComparison.Ordinal) && not binding.Intrinsic) then
                        Some(moduleDump.Name, bindingName)
                    else
                        None)

            match matches with
            | [] ->
                Result.Error $"No executable binding named '{bindingName}' was found."
            | [ moduleName, resolvedBindingName ] ->
                Result.Ok(moduleName, resolvedBindingName)
            | _ ->
                Result.Error $"Binding name '{bindingName}' is ambiguous. Use a fully qualified name."
        | _ ->
            let moduleName = segments |> List.take (segments.Length - 1) |> String.concat "."
            let bindingName = List.last segments

            match workspace.KBackendIR |> List.tryFind (fun moduleDump -> String.Equals(moduleDump.Name, moduleName, StringComparison.Ordinal)) with
            | Some moduleDump when moduleDump.Bindings |> List.exists (fun binding -> String.Equals(binding.Name, bindingName, StringComparison.Ordinal) && not binding.Intrinsic) ->
                Result.Ok(moduleName, bindingName)
            | Some _ ->
                Result.Error $"Module '{moduleName}' does not define '{bindingName}'."
            | None ->
                Result.Error $"Module '{moduleName}' was not found."

    let private aggregateDiagnostics diagnostics =
        diagnostics
        |> List.map (fun diagnostic -> diagnostic.Message)
        |> String.concat Environment.NewLine

    let emitDotNetArtifact (workspace: WorkspaceCompilation) (entryPoint: string) (outputDirectory: string) (deployment: DotNetDeployment) =
        if workspace.HasErrors then
            Result.Error $"Cannot emit a runnable backend artifact for a workspace with diagnostics:{Environment.NewLine}{aggregateDiagnostics workspace.Diagnostics}"
        else
            let verificationDiagnostics = Compilation.verifyCheckpoint workspace "KBackendIR"

            if not (List.isEmpty verificationDiagnostics) then
                Result.Error $"Cannot emit malformed KBackendIR:{Environment.NewLine}{aggregateDiagnostics verificationDiagnostics}"
            else
                match resolveEntryPoint workspace entryPoint with
                | Result.Error message ->
                    Result.Error message
                | Result.Ok _ ->
                    let projectDirectory = Path.GetFullPath(outputDirectory)
                    Directory.CreateDirectory(projectDirectory) |> ignore

                    let projectName = "Kappa.Generated.Runner"
                    let projectFilePath = Path.Combine(projectDirectory, $"{projectName}.csproj")
                    let programFilePath = Path.Combine(projectDirectory, "Program.cs")
                    let runtimeFilePath = Path.Combine(projectDirectory, "KappaRuntime.cs")
                    let generatedFilePath = Path.Combine(projectDirectory, "GeneratedProgram.cs")

                    File.WriteAllText(projectFilePath, projectSource deployment)
                    File.WriteAllText(programFilePath, programSource entryPoint)
                    File.WriteAllText(runtimeFilePath, runtimeSource)
                    File.WriteAllText(generatedFilePath, generatedSource workspace)

                    Result.Ok
                        { ProjectDirectory = projectDirectory
                          ProjectFilePath = projectFilePath
                          ProgramFilePath = programFilePath
                          RuntimeFilePath = runtimeFilePath
                          GeneratedFilePath = generatedFilePath
                          EntryPoint = entryPoint
                          Deployment = deployment }
