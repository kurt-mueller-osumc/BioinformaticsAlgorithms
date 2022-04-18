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

    /// Create a nucleotide from a character, with an exception thrown if the character is invalid
    static member Create (code: char) : Nucleotide =
        match Nucleotide.TryCreate(code) with
        | Ok nucleotide -> nucleotide
        | Error err -> failwith err

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
    | Nucleotides of Nucleotide seq

    /// Try to create nucleotides from a list of characters
    static member TryCreate (codes: char seq) : Result<Nucleotides, string seq> =
        codes
        |> Seq.map Nucleotide.TryCreate
        |> Results.combine
        |> Result.map Nucleotides

    /// Create nucleotides from a list of a characters, throwing an error if an invalid character is encountered
    static member Create (codes: char list) : Nucleotides =
        match Nucleotides.TryCreate(codes) with
        | Ok nucleotides -> nucleotides
        | Error errors -> failwith (errors.ToString())

    static member Create (codes: string) : Nucleotides =
        codes.ToCharArray()
        |> Array.toList
        |> Nucleotides.Create

    member this.Codes : Nucleotide seq =
        this |> (fun (Nucleotides codes) -> codes)

    member this.Length : int =
        Seq.length this.Codes

    member this.Complement : Nucleotides =
        this.Codes
        |> Seq.map (fun code -> code.Complement)
        |> Nucleotides

    member this.Chars : char seq =
        this.Codes
        |> Seq.map (fun code -> code.Char)

    // Count the # of times that `kmer` appears in the string of nucleotides
    member this.Count(kmer: Nucleotides) : int =
        this.Codes
        |> Seq.windowed kmer.Length
        |> Seq.filter (fun window -> (Array.toSeq window) = kmer.Codes)
        |> Seq.length

    member this.KmerFrequencies(length: uint) : (Nucleotides * int) seq =
        this.Codes
        |> Seq.windowed (int length)
        |> Seq.groupBy id
        |> Seq.map(fun (nucleotides, vals) -> ((Nucleotides (Array.toSeq nucleotides)), Seq.length vals))

    member this.MostFrequentKmer(length: uint) : (Nucleotides * int) =
        this.KmerFrequencies(length)
        |> Seq.maxBy(fun (_, count) -> count)
