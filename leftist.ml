(* Autor : Michał Borowski *)
(* Reviewer: Artur Matyjasek *)

(**********************)
(* SPECYFIKACJA TYPÓW *)
(**********************)

(* Kolejka reprezentowana przez Node - mający lewego *)
(* syna, priorytet elementu w węźle, długość skrajnie *)
(* prawej ścieżki i prawego syna *)
(* LUB *)
(* Null - pusta kolejka *)
type 'a queue = 
        | Node of 'a queue * 'a * int * 'a queue
        | Null

(**********************)
(*       STAŁE        *)
(**********************)

let empty = Null

(**********************)
(*      WYJĄTKI       *)
(**********************)

exception Empty

(**********************)
(*    KONSTRUKTORY    *)
(**********************)

(* Tworzy jednoelementową kolejkę mając daną wartość. *)
let element v = Node(Null, v, 0, Null)

(**********************)
(*     SELEKTORY      *)
(**********************)

let is_empty q =
        match q with
        | Null -> true
        | _ -> false

(* Długość skrajnie prawej ścieżki kolejki. *)
(* Dla pustej kolejki zwraca -1. *)
let height q =
        match q with 
        | Null -> -1
        | (Node(_, _, h, _)) -> h


(**********************)
(* FUNKCJE POMOCNICZE *)
(**********************)

(* Naprawia lewicowość w węźle oraz oblicza wysokość jego poddrzewa. *)
let repair q =
        match q with
        | Null -> Null
        | Node(l, v, _, r) ->
                let hl = height l in
                let hr = height r in
                if hl > hr then
                        Node(l, v, hr + 1, r)
                else
                        Node(r, v, hl + 1, l)

(**********************)
(*      OPERACJE      *)
(**********************)

let rec join q1 q2 =
        match (q1, q2) with
        | (Null, _) -> q2
        | (_, Null) -> q1
        | (Node(l1, v1, _, r1), Node(_, v2, _, _)) ->
                if v1 > v2 then join q2 q1
                else 
                        let (l, v) = (l1, v1) in
                        let r = join r1 q2 in
                        repair (Node(l, v, 0, r))


let add x q1 = join q1 (element x)

let delete_min q =
        match q with
        | Null -> raise(Empty)
        | Node(left, x, _, right) -> (x, (join left right))

