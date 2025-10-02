let merge (t :int list * int list) : int list =
    match t with
    | xs,xy ->
        let rec aux (xs :int list) (ys :int list) =
            match xs,ys with
            | ([], _) -> ys
            | (_, []) -> xs
            | (x::xr, y::yr) ->
                if x < y then x :: aux xr ys
                else y :: aux xs yr
        aux xs xy