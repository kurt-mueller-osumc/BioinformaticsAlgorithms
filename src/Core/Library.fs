namespace Core

module Say =
    let hello name =
        printfn "Hello %s" name

type Nucleotide =
    | A
    | C
    | G
    | T

    /// Create a nucleotide from a character
    static member Create (input: char) =
        match input with
        | 'A' | 'a' -> Ok A
        | 'C' | 'c' -> Ok C
        | 'G' | 'g' -> Ok G
        | 'T' | 't' -> Ok T
        | _ -> Error "Invalid nucleotide: {input}"

    member this.Char =
        match this with
        | A -> 'A'
        | C -> 'C'
        | G -> 'G'
        | T -> 'T'

type Nucleotides = Nucleotides of Nucleotide list

module ReplicationOrigin =
    let foo = "bar"