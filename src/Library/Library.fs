namespace Library

module Domain =
    type Nucleotide =
        | Adenine
        | Thymine
        | Cytosine
        | Guanine

    module Nucleotide =
        let complement nucleotide =
            match nucleotide with
            | Adenine -> Thymine
            | Thymine -> Adenine
            | Cytosine -> Guanine
            | Guanine -> Cytosine

    type StrandDirection = 
        | ``Five' -> Three'``
        | ``Three' -> Five'``

    type DnaStrand = {
        Direction: StrandDirection
        FirstNucleotide: Nucleotide // ensure at least one nucleotide exists in strand
        RestNucleotides: Nucleotide list
    }

    module DnaStrand =
        let complement strand =
            let reversedNucleotides = strand.FirstNucleotide :: strand.RestNucleotides |> List.rev
            let (firstNucleotide :: restNucleotides) = reversedNucleotides

            match strand.Direction with
            | ``Five' -> Three'`` -> { Direction = ``Three' -> Five'``; FirstNucleotide = firstNucleotide; RestNucleotides = restNucleotides } 
            | ``Three' -> Five'`` -> {  Direction = ``Five' -> Three'``; FirstNucleotide = firstNucleotide; RestNucleotides = restNucleotides } 
    
module Input =
    open Domain

    module Nucleotide =
        let fromCode code =
            match code with
            | "A" -> Ok Adenine
            | "T" -> Ok Thymine
            | "C" -> Ok Cytosine
            | "G" -> Ok Guanine
            | _ -> Error $"Not a valid nucleotide: {code}"