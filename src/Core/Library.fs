namespace BioinformaticsAlgorithms.Core

type Nucleotide =
    | Adenine
    | Cytosine
    | Guanine
    | Thymine

    /// Create a nucleotide from a character
    static member Create (code: char) =
        match code with
        | 'A' | 'a' -> Ok Adenine
        | 'C' | 'c' -> Ok Cytosine
        | 'G' | 'g' -> Ok Guanine
        | 'T' | 't' -> Ok Thymine
        | _ -> Error "Invalid nucleotide: {code}"

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

open Utilities

type Nucleotides =
    | Nucleotides of Nucleotide list

    /// Create nucleotides from a list of characters
    static member Create (codes: char list) =
        codes
        |> List.map Nucleotide.Create
        |> Results.combine
        |> Result.map Nucleotides

module ReplicationOrigin =
    let foo = "bar"