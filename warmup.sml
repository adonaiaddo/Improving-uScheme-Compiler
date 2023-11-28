(***** Problem 1 *****)
(* mynull: determines whether a list is empty *)
(* mynull: 'a list -> bool *)

fun   mynull [] = true
    | mynull _  = false

    (* Unit Tests *)
    val () =
        Unit.checkAssert "[] is null"
        (fn () => mynull [])

    val () =
        Unit.checkAssert "[1] is not null"
        (fn () => not (mynull [1]))

    val () =
        Unit.checkAssert "[1, 2, 3] is not null"
        (fn () => not (mynull [1, 2, 3]))
    
    val () =
        Unit.checkAssert "[[1], [2], [3]] is not null"
        (fn () => not (mynull [[1], [2], [3]]))



(***** Problem 2 *****)
(* a *)
(* reverse reverses a list *)
(* val reverse: 'a list -> 'a list *)

fun reverse zs = foldl op::[] zs

    (* (int list) string buffer for unit testing *)
    val int_list_toString = Unit.listString Unit.intString

    (* Unit Tests *)
    val () =
        Unit.checkExpectWith int_list_toString "reversing empty"
        (fn () => reverse [])
        []

    val () =
        Unit.checkExpectWith(Unit.listString Int.toString)
        "reversing non-empty"
        (fn () => reverse [1, 2, 3])
        [3, 2, 1]

(* b *)
(* returns the minimum/smallest element in a non-empty list of integers *)
(* val minlist: int list -> int *)

(* Match exception initialized *)
exception Match

fun minlist [] = raise Match
    | minlist (z::zs) = foldl Int.min z zs

    (* Unit Tests *)
    val () =
        Unit.checkExpectWith Unit.intString "smallest of singleton"
        (fn () => minlist [1])
        1

    val () =
        Unit.checkExpectWith Int.toString
        "smallest of non-empty list with positive integers"
        (fn () => minlist [1, 2, 3])
        1
    
    val () =
        Unit.checkExpectWith Int.toString
        "smallest of non-empty list with negative integers"
        (fn () => minlist [~1, ~2, ~3])
        ~3

    val () =
        Unit.checkExpectWith Int.toString
        "smallest of non-empty list with both positive and negative integers"
        (fn () => minlist [~3, ~2, ~1, 1, 2, 3])
        ~3

    val () =
        Unit.checkExpectWith Int.toString
        "smallest of non-empty list with zero, positive, and negative integers"
        (fn () => minlist [~3, ~2, ~1, 0, 1, 2, 3])
        ~3

    val () =
        Unit.checkExnWith Int.toString "empty list exception"
        (fn () => minlist [])



(***** Problem 3 *****)
(* zip takes a pair of lists of the same length and returns their list of
   pairs *)
(* zip: 'a list * 'b list -> ('a * 'b) list *)


(* Mismatch exception initialized*)
exception Mismatch

fun zip ([], [])                = []
    | zip ((y::ys), [])         = raise Mismatch
    | zip ([], (y::ys))         = raise Mismatch
    | zip ((x::xs), (y::ys))    = (x,y)::zip(xs,ys)

    (* Some new string builders *)
    val int_pair_toString = Unit.pairString Unit.intString Unit.intString
    val list_pair_toString = Unit.listString int_pair_toString

    (* Unit Tests *)
    val () =
    Unit.checkExpectWith list_pair_toString "zip on integer lists"
    (fn () => zip ([1, ~2, 4], [9, 12, 0]))
    [(1, 9), (~2, 12), (4, 0)]

    val () =
    Unit.checkExpectWith list_pair_toString "zip on empty lists"
    (fn () => zip ([], []))
    []

    val () =
    Unit.checkExnWith list_pair_toString "Mismatch exception"
    (fn () => zip ([1], [1, 2, 3]))

    val () =
    Unit.checkExnWith list_pair_toString "Mismatch exception"
    (fn () => zip ([1, 2, 3], [1]))

    val () =
    Unit.checkExnWith list_pair_toString "Mismatch exception"
    (fn () => zip ([], [1, 2, 3]))

    val () =
    Unit.checkExnWith list_pair_toString "Mismatch exception"
    (fn () => zip ([1], []))

    val () =
    Unit.checkExpectWith
    (Unit.listString (Unit.pairString Char.toString Int.toString))
    "zip on two lists of different datatypes"
    (fn () => zip ([#"a"], [1]))
    [(#"a", 1)]


(***** Problem 4 *****)
(* pairfoldrEq applies a function of 3 arguments to a pair of lists of the same
    length *)
(* pairfoldrEq : ('a * 'b * 'c -> 'c) -> 'c -> 'a list * 'b list -> 'c *)

fun pairfoldrEq f m ([], []) = m
    | pairfoldrEq f m (y::ys, z::zs) = f(y, z, pairfoldrEq f m (ys, zs))
    | pairfoldrEq f m _ = raise Mismatch

    (* Unit Tests *)
    val () =
        Unit.checkExnWith Int.toString "Mismatch exception"
        (fn () => pairfoldrEq (fn (x, y, z) => x * y * z) 0 ([1, 4, 9], [1, 8]))
    
    val () =
        Unit.checkExnWith Int.toString "Mismatch exception"
        (fn () => pairfoldrEq (fn (a, b, c) => a + b + c) 0 ([], [1, 8]))
    
    val () =
        Unit.checkExnWith Int.toString "Mismatch exception"
        (fn () => pairfoldrEq (fn (x, y, z) => x - y - z) 0 ([1, 4, 9], []))
    
    val () =
        Unit.checkExnWith Int.toString "Mismatch exception"
        (fn () => pairfoldrEq (fn (a, b, c) => a * b * c) 0 ([1], [7, 8]))

(* ziptoo takes a pair of lists of the same length and returns the their list of
    pairs (paired at each index) *)
(* val ziptoo: 'a list * 'b list -> ('a * 'b) list *)
fun ziptoo ([], []) = []
    | ziptoo (x::xs, y::ys) = pairfoldrEq (fn (a, b, c) => (a, b)::c) []
                                            (x::xs, y::ys)
    | ziptoo _ = raise Mismatch

    (* String Builders *)
    val int_pair_toString = Unit.pairString Unit.intString Unit.intString
    val bool_pair_toString = Unit.pairString Unit.boolString Unit.boolString
    val list_pair_toString = Unit.listString int_pair_toString
    val list_boolpair_toString = Unit.listString bool_pair_toString

    (* Unit Tests *)
    val () =
        Unit.checkExpectWith list_pair_toString "[] lists"
        (fn () => ziptoo ([], []))
        []

    val () =
        Unit.checkExpectWith list_pair_toString "zip on integer lists"
        (fn () => ziptoo ([0, 10, 1], [1, 1, 1]))
        [(0, 1), (10, 1), (1, 1)]
 
    val () =
        Unit.checkExnWith list_pair_toString "Mismatch -> exception"
        (fn () => ziptoo ([1, 2, 3], []))

    val () =
        Unit.checkExnWith list_pair_toString "Mismatch -> exception"
        (fn () => ziptoo ([], [1, 2, 3]))
 
    val () =
        Unit.checkExpectWith
        (Unit.listString (Unit.pairString Int.toString Bool.toString))
        "perform zip on lists with varying data types"
        (fn () => ziptoo ([1, 0], [true, false]))
        [(1, true), (0, false)]

(***** Problem 5 *****)
(* concat takes a list of lists of any lengths, and returns one list with all
    the elements of the prior two lists in their right order *)
(* val concat: 'a list list -> 'a list *)

fun concat zs = foldr (op @) [] zs

    (* Unit Tests *)
    val () =
        Unit.checkExpectWith (Unit.listString Int.toString) "[] lists"
        (fn () => concat [])
        []

    val () =
        Unit.checkExpectWith (Unit.listString Int.toString)
        "cat non-empty list with empty list"
        (fn () => concat [[1, 2], []])
        [1, 2]

    val () =
        Unit.checkExpectWith (Unit.listString Int.toString)
        "cat empty list with non-empty list"
        (fn () => concat [[], [1, 2]])
        [1, 2]
    
    val () =
        Unit.checkExpectWith (Unit.listString Int.toString) "list of lists"
        (fn () => concat [[0], [~1], [10, 9, 7, 8], [1,2], [~5, 6]])
        [0, ~1, 10, 9, 7, 8, 1, 2, ~5, 6]


(***** Problem 6 *****)

datatype ordsx 
  = BOOL of bool
    | NUM  of int
    | SYM  of string
    | SXS  of ordsx list

fun numbersSx [] = SXS[]
    | numbersSx ys = SXS(map NUM ys)

fun sxString (SYM s)   = s
    | sxString (NUM n)   = Unit.intString n
    | sxString (BOOL b)  = if b then "true" else "false"
    | sxString (SXS sxs) = "(" ^ String.concatWith " " (map sxString sxs) ^ ")"


        (* Unit Tests *)
        val () =
            Unit.checkExpectWith  sxString "list of neg and pos ints"
            (fn () => numbersSx [10, 2, ~3])
            (SXS [NUM 10, NUM 2, NUM ~3])

(* extracts just the symbols from an ordinary S-expression *)
(* flattenSyms : ordsx -> string list *)
fun flattenSyms (SYM ordsx)  = [ordsx]
    | flattenSyms (NUM _)      = []
    | flattenSyms (BOOL _)     = []
    | flattenSyms (SXS ordsxs) = List.concat (List.map flattenSyms ordsxs)

        (* Unit Tests *)
        val () =
            Unit.checkExpectWith (Unit.listString Unit.stringString)
            "flatten called on list with only symbols"
            (fn () => flattenSyms (SXS []))
            []

        val () =
            Unit.checkExpectWith (Unit.listString Unit.stringString)
            "flatten called on list with only symbols"
            (fn () => flattenSyms (SXS [SYM "Thanks", SYM "Dear", SYM "World"]))
            ["Thanks", "Dear", "World"]
 
       val () =
            Unit.checkExpectWith (Unit.listString Unit.stringString)
            "flatten not just symbols"
            (fn () => flattenSyms (SXS [SYM "hello", BOOL false,  SYM "hi", 
                                    SXS []]))
                ["hello", "hi"]


val () = Unit.reportWhenFailures ()