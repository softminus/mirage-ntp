type 'a history = History of int * int * 'a list
type point = Now | Ago of point * int | Fixed of int
type range = Range of point * point
type status = Ready of int | NotReady of int | NeverReady

val at : 'a history -> point -> 'a
val hcons : 'a -> 'a history -> 'a history
val resize : 'a history -> int -> 'a history
val min_by : ('a -> 'b) -> 'a history -> 'a
