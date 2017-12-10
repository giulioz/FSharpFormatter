module FSharpCodeFormatter.Test2

module Basic =
    let prg1 = """
    let f x =
    if x > 0 then 1
    else
    let y = x + 2
    let f x =
    if x > 0 then
    let a = 1
    in a + b
    elif x > 0 then 3
    else
    let a = 1
    in a
    in f y + f z
    """

    let prg2 = """
    let f x =
    match a with
    | 1 -> 
    match b with
    | 2 -> 2
    | 3 ->
    match c with
    | 4 -> 4
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
    let f x =
    let res = 
    match x with
    | 0 -> 1
    | 1 -> 18
    | 2 ->
    match x + 45 with
    | 47 -> x * 2
    | _ -> 7
    in res + 8
    """

    let prg5 = """
    let f x =
    let res =
    if x < 5 then
    let a = 8
    let b =
    if x < 3 then 8
    else 3
    in x + a * b
    else
    let c =
    match x with
    | 6 -> 7
    | _ -> 78    
    let d = 1
    in x * c + d
    in res
    """

    let my_prg = """
    let f x =
    let res =
    match x with
    | 0 -> 1
    | 1 -> 18
    | 2 ->
    match x + 45 with
    | 47 -> x * 2
    | _ -> 7
    res + 8

    let f x =
    let res =
    if x < 5 then
    let a = 8
    let b =
    if x < 3 then 8
    else 3
    x + a * b
    else
    let c =
    match x with
    | 6 -> 
    let a = 3
    | _ -> 78
    let d = 1
    x * c + d
    res + 1

    let aaa bbb =
    match l with
    | [] -> []
    | x :: xs ->
    if x = 0 then 3 :: xs
    else l

    let f1 a1 =
            let f2 a2 =
                let f3 a3 =
                    let f4 a5 =
                        if a5 > 0 then 0
                        else a5 * 3
                    f4 a3
                f3 a2
            f2 a1

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
    f y

    let fib n =            
    let rec R n =
    if n < 2 then 1
    else R (n - 1) + R (n - 2)
    R n

    let rec aaa bbbb =
        match aaa with
        | 0 -> 0
        | 1 -> 1
        | 5 -> 3
        | 6 ->
            let ccc ddd =
                match ddd with
                | 4 -> 6
                | 7 ->
                    let hhh mmm =
                        match hhh with
                        | 6 -> 8
                        | 8 -> m
                    hhh 999
            ccc 4 5
        | 7 ->
            let fff ggg =
                if 7 > 6 then true
                elif 8 > 9 then false
                else 5
            fff 7
        | 10 ->
            if 1 > 0 then
                if 5 > 7 then
                    if 8 > 4 then 5
                    else 6
                elif 7 > 8 then
                    if 8 > 4 then 5
                    else 6
                else
                    let hhh mmm =
                        match hhh with
                        | 6 -> 8
                        | 8 -> m
                    hhh 999
           elif 1 > 0 then
               if 5 > 7 then
                   if 8 > 4 then 5
                   else 6
               elif 7 > 8 then
                   if 8 > 4 then 5
                   else 6
               else
                   let hhh mmm =
                       match hhh with
                       | 6 -> 8
                       | 8 -> m
                   hhh 999
          else
              if 5 > 7 then
                  if 8 > 4 then 5
                  else 6
              elif 7 > 8 then
                  if 8 > 4 then 5
                  else 6
              else
                  let hhh mmm =
                      match hhh with
                      | 6 -> 8
                      | 8 -> m
                  hhh 999

    let prova a =
        let x =
            fun x ->
                match x with
                | 0 -> 0
                | _ ->
                    if x > 0 then -2 * x
                    else 2 * x
        let y =
            fun y -> y - 1
        x a + y a

        let test =
            let ab =
                in
                    3
            in
                ab + 2
    """

module Advanced =
    let prg1 = """
    let funzione x = if x > 0 then 3 else funzione 2
    """

    let prg2 = """
    let rec foldl f z l = match l with | [] -> if x > 0 then 1 else 3 | x :: xs -> foldl f (f z x) xs
    """

    let prg3 = """
    let rec map f l = let k = 1 let rec R c = match c with | [] -> [] | x :: xs -> match f x with | [] -> 3 | x :: xs -> match a with | [] -> [] | x :: xs -> 3 in R [] 
    """

    let my_prg = """
    let f1 a1 = let f2 a2 = let f3 a3 = let f4 a5 = if a5 < 0 then 0 else match a5 with | 0 -> 0 | 3 -> if 5 > 3 then 4 / 2 else 0 | 4 -> match a5 with | 0 -> 4 | _ -> 6 in f4 a3 in f3 a2 in f2 a1
    """

module A = Advanced
module B = Basic

let flatten = List.fold (+) ""

let basic = [B.prg1; B.prg2; B.prg3; B.prg4; B.prg5; B.my_prg] |> flatten 

let advanced = [A.prg1; A.prg2; A.prg3; A.my_prg] |> flatten 

let all = basic