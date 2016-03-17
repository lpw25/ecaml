type ('key, 'value) t

val lookup : 'key -> ('key, 'value) t -> 'value option

val update : 'key -> 'value -> ('key, 'value) t -> ('key, 'value) t
