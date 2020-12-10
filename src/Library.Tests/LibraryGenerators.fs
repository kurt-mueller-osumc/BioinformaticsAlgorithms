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
        let! firstNucleotide = randomNucleotide
        let! nucleotides = Gen.listOf randomNucleotide
        let! strandDirection = randomStrandDirection

        return {
            Direction = strandDirection
            FirstNucleotide = firstNucleotide
            RestNucleotides = nucleotides
        }
    }

type DnaStrandGenerator =
    static member DnaStrand() = Arb.fromGen randomDnaStrand

Arb.register<DnaStrandGenerator>()