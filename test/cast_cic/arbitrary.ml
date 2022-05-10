open Cast_cic.Ast

let term =
  let name_of_int n = string_of_int n |> Name.of_string in
  let var n = Var (name_of_int n) in
  let universe n = Universe n in
  let app t1 t2 = App (t1, t2) in
  let lambda i dom body = Lambda { id = name_of_int i; dom; body } in
  let prod i dom body = Prod { id = name_of_int i; dom; body } in
  let unknown t = Unknown t in
  let err t = Err t in
  let cast source target term = Cast { source; target; term } in

  (* TODO: Check weights and distribution in subterms *)
  let term_gen =
    QCheck.Gen.(
      sized
      @@ fix (fun self n ->
            match n with
            | 0 -> map universe nat
            | n ->
                frequency
                  [
                    (1, map var nat);
                    (1, map universe nat);
                    (2, map2 app (self (n / 2)) (self (n / 2)));
                    (2, map3 lambda nat (self (n / 2)) (self (n / 2)));
                    (2, map3 prod nat (self (n / 2)) (self (n / 2)));
                    (2, map unknown (self n));
                    (2, map err (self n));
                    (2, map3 cast (self (n / 3)) (self (n / 3)) (self (n / 3)));
                  ])) in

(* TODO: Add shrinker *)
 QCheck.make term_gen ~print:to_string

 let context arbitrary_key arbitrary_value =
  let open Cast_cic.Context in
  let context_gen =
    QCheck.Gen.(
      sized
      @@ fix (fun self n ->
            match n with
            | 0 -> return empty
            | n ->
              let* key = QCheck.gen arbitrary_key in
              let* value = QCheck.gen arbitrary_value in
              map (add ~key ~value) (self (n - 1)))) in

  let print_context =
    let (let*) = Option.bind in
    let* print_key = QCheck.get_print arbitrary_key in
    let* print_value = QCheck.get_print arbitrary_value in
    Some (to_string print_key print_value) in

(* TODO: Add shrinker *) 
 QCheck.make context_gen ?print:print_context