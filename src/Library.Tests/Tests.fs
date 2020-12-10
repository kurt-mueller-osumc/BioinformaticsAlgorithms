module Tests

open System
open Xunit
open FsCheck.Xunit

open Generators
open Library.Domain


[<Fact>]
let ``My test`` () =
    Assert.True(true)

[<Property>]
let ``DnaStrand complements`` (strand:DnaStrand) =
    let complement = DnaStrand.complement strand

    strand = DnaStrand.complement complement