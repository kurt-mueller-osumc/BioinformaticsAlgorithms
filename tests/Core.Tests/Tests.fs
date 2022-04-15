module Tests

open System
open Xunit

open BioinformaticsAlgorithms.Core

[<Fact>]
let ``My test`` () =
    Assert.True(true)

[<Fact>]
let ``Most Frequent Words`` () =
    let nucleotides = Nucleotides.Create "actgactcccacccc"

    let mfk = nucleotides.MostFrequentKmer(3u)

    Assert.True(true)