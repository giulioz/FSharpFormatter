module FSharpCodeFormatter.Test

let prg1 = """
let f x =
if x > 0 then 1
else
let y = x + 2
                        let z = x + 3
    let f x =
        if x > 0 then
let a = 1
            let b = 3
            in a + b
                elif x > 0 then 3
elif x > 0 then
                            let a = 1
            let b = 3
in a + b
else 3
f y, f z

let fib n =            
            let rec R n =
if n < 2 then 1
else R (n - 1) + R (n - 2)
R n
"""

let prg2 = """
let f x =
match a with
| 1 -> 
match b with
| 2 -> 2
| 3 ->
    fun x ->
    match c with
    | 4 -> fun x -> x
    | 5 -> 5
"""


let prg3 = """
let rec F y =
let f x = x + 1
let y = f 4
match f 3 with
| 2 ->
match y with
    | 3 -> true
| 4 ->
if p 3 then 
let z = 8
in z < y
    else false
"""

let prg4 = """
let test s =
let temp = tok s
match temp with
| [] -> 0
| x :: xs -> match x with
| 7 -> 0 + test xs
| 14 -> 1 + test xs
"""

let flatten = List.fold (+) ""

let basic = [prg1; prg2; prg3; prg4] |> flatten

let all = [prg1; prg2; prg3; prg4] |> flatten


