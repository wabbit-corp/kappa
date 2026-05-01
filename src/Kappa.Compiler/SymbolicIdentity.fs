namespace Kappa.Compiler

[<RequireQualifiedAccess>]
module SymbolicIdentity =
    let moduleIdentityOfSpecifier specifier =
        match specifier with
        | Dotted segments -> Some(ModuleIdentity.ofSegments segments)
        | Url _ -> None
