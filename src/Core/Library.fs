namespace Core

type Nucleotide =
    | Adenine
    | Cytosine
    | Guanine
    | Thymine

    /// Create a nucleotide from a character
    static member Create (input: char) =
        match input with
        | 'A' | 'a' -> Ok Adenine
        | 'C' | 'c' -> Ok Cytosine
        | 'G' | 'g' -> Ok Guanine
        | 'T' | 't' -> Ok Thymine
        | _ -> Error "Invalid nucleotide: {input}"

    member this.Char =
        match this with
        | Adenine  -> 'A'
        | Cytosine -> 'C'
        | Guanine  -> 'G'
        | Thymine  -> 'T'

    member this.Complement =
        match this with
        | Adenine -> Thymine
        | Cytosine -> Guanine
        | Guanine -> Cytosine
        | Thymine -> Adenine

type Nucleotides = Nucleotides of Nucleotide list

module ReplicationOrigin =
    let foo = "bar"