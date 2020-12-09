module Generators

open FsCheck

open Library.Domain

let randomNucleotide = 
    [ Adenine; Thymine; Cytosine; Guanine ]
        |> Seq.map Gen.constant
        |> Gen.oneof

let randomStrandDirection = 
    [ ``Five' -> Three'``; ``Three' -> Five'`` ]
        |> Seq.map Gen.constant
        |> Gen.oneof

let randomDnaStrand = 
    gen {
        let! nucleotides = Gen.listOf randomNucleotide
        let! strandDirection = randomStrandDirection

        return {
            Direction = strandDirection
            Nucleotides = nucleotides
        }
    }