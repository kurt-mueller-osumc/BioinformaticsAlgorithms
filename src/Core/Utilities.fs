namespace BioinformaticsAlgorithms.Core

module Utilities =
    [<RequireQualifiedAccess>]
    module Results =
        /// Combine a list of results into a single result that contains either all the valid elements or a list of errors
        let combine (results: Result<'ok, 'error> seq) : Result<'ok seq, 'error seq> =
            let initial : Result<'ok seq, 'error seq> = Ok Seq.empty

            results
            |> Seq.fold (fun aggregate result ->
                match result with
                | Ok ok -> aggregate |> Result.map (fun oks -> Seq.append oks (Seq.singleton ok))
                | Error err -> aggregate |> Result.mapError (fun errs -> Seq.append errs (Seq.singleton err))
            ) initial

        /// Partition a list of results into a tuple of oks and errors
        let partition (results: Result<'ok, 'error> list) : 'ok list * 'error list =
            let initial : 'ok list * 'error list = ([], [])

            results
            |> List.fold (fun (oks, errors) result ->
                match result with
                | Ok ok -> (oks @ [ok], errors)
                | Error err -> (oks, errors @ [err])
            ) initial