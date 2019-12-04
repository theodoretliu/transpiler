let rec f l =
  let s = 0 in
  let s =
    let rec helper l s =
      match l with [] -> s | x :: tl -> (helper tl) (s + x + 1)
    in
    (helper l) s
  in
  s
