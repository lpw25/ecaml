type ('key, 'value) t

val empty : ('key, 'value) t

val lookup : 'key -> ('key, 'value) t -> 'value option

val update : 'key -> 'value -> ('key, 'value) t -> ('key, 'value) t

val of_list : ('key * 'value) list -> ('key, 'value) t

val iter : ('key -> 'value -> unit) -> ('key, 'value) t -> unit

val keys : ('key, 'value) t -> 'key list
