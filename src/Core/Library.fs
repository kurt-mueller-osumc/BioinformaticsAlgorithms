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
        | _ -> Error $"Invalid nucleotide: {code}"

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

    member this.Codes = this |> (fun (Nucleotides codes) -> codes)

    member this.Length = this.Codes.Length

    member this.Complement =
        this.Codes
        |> List.map (fun code -> code.Complement)
        |> Nucleotides

    member this.Chars =
        this.Codes
        |> List.map (fun code -> code.Char)

    member this.Count(kmer: Nucleotides) =
        this.Codes
        |> List.windowed kmer.Length
        |> List.filter (fun window -> window = kmer.Codes)
        |> List.length