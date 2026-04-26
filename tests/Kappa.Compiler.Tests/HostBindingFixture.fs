namespace Kappa.Compiler.TestHost

type Sample(value: string) =
    member _.Echo() = value
    member _.Text = value
    static member Create(value: string) = Sample(value)
