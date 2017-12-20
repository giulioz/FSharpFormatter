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
        else
            R (n - 1) + R (n - 2)
    R n
"""

let prg2 = """
let c =
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
    f 1
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
let a =
    if a = a then a
    else
        let b =
            b * 2
        let c =
            if a = b then c
            else d
        let f =
            fun culo ->
            match g with
            | test1 -> ok
            | test2 -> no
        g b c 2
"""

let prg5 = """
let it go =
    let delle palle =
        match x with
        | dio ->
            if a = b then 3
            else 4
        | gesu -> no
        | giulio ->
            idem con patate
    delle palle

let f x =
let res =
if x < 5 then
let a = 8
let b =
if x < 3 then 8
else 3
in x+a*b
else
let c =
match x with
| 6 -> 7
| _ -> 78
let d = 1
in x * c + d
in res
"""

let prgdio = """
let test s =
let temp a =
match a with
| [] -> 0
| x :: xs ->
match x with
| 7 -> 0 + test xs
| 14 -> 1 + test xs
in temp a b
"""

let prgdemonio = """
let f a =
    let b a =
        match a with
        | 0 -> 1
        | 1 -> 0
        | 2 ->
            match x with
            | a -> b
            | c -> ∂d
    b (5 :: a)

let f n =
    let b n =
        if n = 0 then 0
        else
            let b =
                let c = 5 + 2
                2 + c n
            if b n = 5 then
                a + b
            else
                let c =
                    4 + 3
                c + 2
    b (n + 1)
"""

let grzprg = """
let a b =
    let d f =
        match b with
        | 42 -> 0
        | 0 ->
            if a = b then
                b + 2
            else
                42 / 0
        | 1 ->
            0
    d f

let a =
    match stocazzo with
    | x ->
        y
    | x -> y
"""

let gilbertoinfame = """
let f1 a1 =
            let f2 a2 =
                let f3 a3 =
                    let f4 a5 =
                        if a5 < 0 then 0
                        else
                            match a5 with
                            | 0 -> 0
                            | 3 ->
                            if 5 > 3 then 4 / 2
                                    else 0
                            | 4 ->
                                match a5 with
                                | 0 -> 4
                                | _ -> 6
                    f4 a3
                f3 a2
            f2 a1
"""

let dario = """
let f x =
match x with
| 0 -> 1
| 1 -> 42
| 2 ->
let x c =
match with
| 54 -> 545
| u74 -> trt
| d ->
let d f =
match er with
| 4 -> 454
| 6 -> 434
in dd
| 54 -> 54
in
fd
| 3 -> 0
"""

let single = """
let test x y = if y =
0 then 0 else x / y

let test x = match x with | 0 -> 1 | 1 -> 0
"""

let flatten = List.fold (+) ""

let basic = [prg1; prg2; prg3] |> flatten

let all = [prg1; prg2; prg3; prg4; prg5; prgdio; prgdemonio; grzprg; gilbertoinfame; dario] |> flatten

let custom = basic//[single] |> flatten

