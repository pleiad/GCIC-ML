let ( let* ) = Result.bind
let map_snd f (x, y) = x, f y

let rec map_results (f : 'a -> ('b, 'e) result) (xs : 'a list) : ('b list, 'e) result =
  match xs with
  | [] -> Ok []
  | x :: xs ->
    let* y = f x in
    let* ys = map_results f xs in
    Ok (y :: ys)

module List = struct
  include List

  let rec drop n xs =
    match n, xs with
    | 0, xs -> xs
    | n, _ :: xs -> drop (n - 1) xs
    | _ -> xs
end
