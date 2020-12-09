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
        Directionality: StrandDirection
        Nucleotides: Nucleotide list
    }

    module DnaStrand =
        let complement strand =
            match strand.Directionality with
            | ``Five' -> Three'`` -> { Directionality = ``Three' -> Five'``; Nucleotides = List.rev strand.Nucleotides } 
            | ``Three' -> Five'`` -> {  Directionality = ``Five' -> Three'`` ; Nucleotides = List.rev strand.Nucleotides } 
    
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