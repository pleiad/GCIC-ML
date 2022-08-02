let ( let* ) = Result.bind
let map_snd f (x, y) = x, f y

let rec map_results (f : 'a -> ('b, 'e) result) (xs : 'a list) : ('b list, 'e) result =
  match xs with
  | [] -> Ok []
  | x :: xs ->
    let* y = f x in
    let* ys = map_results f xs in
    Ok (y :: ys)

let rec fold_results
    (f : 'a -> 'b -> ('a, 'e) result)
    (z : ('a, 'e) result)
    (xs : 'b list)
    : ('a, 'e) result
  =
  match xs with
  | [] -> z
  | x :: xs ->
    let* acc = z in
    fold_results f (f acc x) xs

module List = struct
  include List

  let rec drop n xs =
    match n, xs with
    | 0, xs -> xs
    | n, _ :: xs -> drop (n - 1) xs
    | _ -> xs

  let split_at n xs =
    let rec go acc n xs =
      match n, xs with
      | 0, xs -> List.rev acc, xs
      | n, x :: xs -> go (x :: acc) (n - 1) xs
      | _ -> List.rev acc, []
    in
    go [] n xs

  let find_index_opt x xs =
    let rec go acc xs =
      match xs with
      | [] -> None
      | y :: ys -> if x = y then Some acc else go (acc + 1) ys
    in
    go 0 xs
end
