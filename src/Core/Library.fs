namespace BioinformaticsAlgorithms.Core

type Nucleotide =
    | Adenine
    | Cytosine
    | Guanine
    | Thymine

    /// Try to create a nucleotide from a character
    static member TryCreate (code: char) : Result<Nucleotide, string> =
        match code with
        | 'A' | 'a' -> Ok Adenine
        | 'C' | 'c' -> Ok Cytosine
        | 'G' | 'g' -> Ok Guanine
        | 'T' | 't' -> Ok Thymine
        | _ -> Error $"Invalid nucleotide: {code}"

    member this.Char : char =
        match this with
        | Adenine  -> 'A'
        | Cytosine -> 'C'
        | Guanine  -> 'G'
        | Thymine  -> 'T'

    member this.Complement : Nucleotide =
        match this with
        | Adenine -> Thymine
        | Cytosine -> Guanine
        | Guanine -> Cytosine
        | Thymine -> Adenine

open Utilities

type Nucleotides =
    | Nucleotides of Nucleotide list

    /// Create nucleotides from a list of characters
    static member TryCreate (codes: char list) : Result<Nucleotides, string list> =
        codes
        |> List.map Nucleotide.TryCreate
        |> Results.combine
        |> Result.map Nucleotides

    member this.Codes : Nucleotide list =
        this |> (fun (Nucleotides codes) -> codes)

    member this.Length : int =
        this.Codes.Length

    member this.Complement : Nucleotides =
        this.Codes
        |> List.map (fun code -> code.Complement)
        |> Nucleotides

    member this.Chars : char list =
        this.Codes
        |> List.map (fun code -> code.Char)

    member this.Count(kmer: Nucleotides) : int =
        this.Codes
        |> List.windowed kmer.Length
        |> List.filter (fun window -> window = kmer.Codes)
        |> List.length