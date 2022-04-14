namespace BioinformaticsAlgorithms.Core

module Utilities =
    [<RequireQualifiedAccess>]
    module Results =
        /// Combine a list of results into a single result that contains either all the valid elements or a list of errors
        let combine (results: Result<'ok, 'error> list) : Result<'ok list, 'error list> =
            let initial : Result<'ok list, 'error list> = Ok List.empty

            results
            |> List.fold (fun aggregate result ->
                match result with
                | Ok ok -> aggregate |> Result.map (fun oks -> ok :: oks)
                | Error err -> aggregate |> Result.mapError (fun errs -> err :: errs)
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