type ('key, 'value) t = ('key * 'value) list

let empty = []

let rec lookup k = function
  | [] -> None
  | (k', v) :: dict -> if k = k' then Some v else lookup k dict

let update k v dict =
  (k, v) :: dict

let of_list x = x

let iter f = List.iter (fun (x,y) -> f x y)

let keys x = List.map (fun (x, y) -> x) x
