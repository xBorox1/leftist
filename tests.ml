#load "leftist.cmo";;
open Leftist
open List;;

let create_queue lst =
        fold_left (fun acc h -> add h acc) empty lst;;  

let destroy_queue q =
        let rec helper q acc =
                if is_empty q then acc
                else
                        let (v, q2) = delete_min q in
                        helper q2 (v::acc)
        in
                rev (helper q []);;

let random_list cnt = 
        let a = Random.self_init() in
        let rec helper cnt acc =
                if cnt = 0 then acc
                else
                        let cnt2 = cnt - 1 in
                        let el = Random.int 1000000000 in
                        helper cnt2 (el::acc)
        in
                helper cnt [];;

let compare x y =
        if x > y then 1
        else if x = y then 0
        else -1;;

let rec testuj cnt size =
        if cnt = 0 then true
        else 
                let l1 = random_list size in
                let l2 = random_list size in
                let sorted_l1 = sort compare l1 in
                let sorted_l2 = sort compare l2 in
                let l3 = append l1 l2 in
                let sorted_l3 = sort compare l3 in
                let q = create_queue l1 in
                let p = create_queue l2 in
                let r = join p q in
                let b1 = ((destroy_queue q) = sorted_l1) in
                let b2 = ((destroy_queue p) = sorted_l2) in
                let b3 = ((destroy_queue r) = sorted_l3) in
                b1 && b2 && b3 && (testuj (cnt - 1) size) 


let q = empty;;
assert(is_empty q);;
let q = add 4 q;;
assert(not (is_empty q));;
let q = add 2 q;;
assert(fst (delete_min q) = 2);;
let q = snd (delete_min q);;
assert(not (is_empty q));;
assert(fst (delete_min q) = 4);;
let q = snd (delete_min q);;
assert(is_empty q);;

let q = create_queue [8; 16; 2; 4; 20; 6];;
let p = create_queue [10; 5; 1; 11; 12; 3];;
assert((destroy_queue q) = [2; 4; 6; 8; 16; 20]);;
assert((destroy_queue p) = [1; 3; 5; 10; 11; 12]);;
let r = join p q;;
assert((destroy_queue r) = [1; 2; 3; 4; 5; 6; 8; 10; 11; 12; 16; 20]);;

let q = create_queue [true ; false ; true];;
let p = create_queue [false ; true ; true ; false];;
assert((destroy_queue q) = [false ; true ; true]);;
assert((destroy_queue p) = [false ; false ; true ; true]);;
let r = join p q;;
assert((destroy_queue r) = [false; false; false; true; true; true; true]);;

assert((testuj 10000 10) = true);;
assert((testuj 1000 100) = true);;
assert((testuj 100 1000) = true);;
assert((testuj 10 10000) = true);;
assert((testuj 1 100000) = true);;
