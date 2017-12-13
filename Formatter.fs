// -------------------------------------------------------------------------------
//    _____ ____  _                      _____                          _   _
//   |  ___/ ___|| |__   __ _ _ __ _ __ |  ___|__  _ __ _ __ ___   __ _| |_| |_ ___  _ __
//   | |_  \___ \| '_ \ / _` | '__| '_ \| |_ / _ \| '__| '_ ` _ \ / _` | __| __/ _ \| '__|
//   |  _|  ___) | | | | (_| | |  | |_) |  _| (_) | |  | | | | | | (_| | |_| ||  __/| |   
//   |_|   |____/|_| |_|\__,_|_|  | .__/|_|  \___/|_|  |_| |_| |_|\__,_|\__|\__\___||_|  
//                                |_|                                               
//   (aka MagicFSharpCodeFormatter)
//
// Giulio Zausa, Marco Perrone
// thanks to Alessio Marotta for the stack idea
// and to Gilberto Vergerio for the impossible tests
// -------------------------------------------
// !! PLEASE DO NOT STEAL, OR WE WILL BOTH HAVE A NEGATIVE MARK !!
// -------------------------------------------

module FSharpCodeFormatter.Formatter

open Lib
open System


// -------------------------------------------
//  Stack Functions
// -------------------------------------------

/// Pushes an element into the stack, returns the element added and the stack
let private push stack element =
    match stack with
    | (a, b) -> (element, element :: b)

/// Pops an element from the stack, returns the element removed and the stack
let private pop stack =
    match stack with
    | (a, x :: y :: xs) -> (x, y :: xs)
    | (a, x :: []) -> (x, [])
    | (a, []) -> raise (ArgumentException("Stack empty"))

/// Peeks an element from the stack, returns the first element and the stack
let private peek stack =
    match stack with
    | (a, x :: xs) -> x
    | (a, []) -> raise (ArgumentException("Stack empty"))

/// Returns the number of elements on the stack
let rec private count stack =
    match stack with
    | (a, x :: xs) -> 1 + count (a, xs)
    | (a, []) -> 0

/// Increments by 1 the first element of the stack
let private push_increment stack =
    match stack with
    | (a, x :: xs) -> (0, (x + 1) :: xs)
    | (a, []) -> (0, [])


// ------------------------------------------- //
//  String Functions                           //
// ------------------------------------------- //

/// Returns true if the first token of a string is tok
let private starts_with (str : string) tok =
    match tokenize_line str with
    | x :: xs when x = tok -> true
    | _ -> false

/// Returns true if the last token of a string is tok
let private ends_with (str : string) tok =
    let rec aux = function
    | x :: [] when x = tok -> true
    | x :: xs -> aux xs
    | _ -> false
    in aux (tokenize_line str)

/// Returns true if the string contains tok
let private contains (str : string) tok =
    let rec aux = function
    | x :: xs when x = tok -> true
    | x :: xs -> aux xs
    | _ -> false
    in aux (tokenize_line str)

let private clean_string (str : string) =
    str.Replace("\t", " ").Replace("\r", " ").Replace("\n", " ")


// -------------------------------------------
//  PUBLIC FUNCTIONS
// -------------------------------------------

/// Indents splitted lines of code
let rec indent (lines : string list) =
    let rec aux stack last = function
    | [] -> []
    | str :: after ->

        // HACK: pushes an element to the stack if empty to prevent an exception
        //       to fix where there the whitespace between to lets is missing
        let stackn =
            if count stack = 0 then (0, [0])
            else stack
        try
            // HACK: reset everything on newline, should work without it but in some rare cases it doesn't
            if str = "" then
                (0, str) :: (aux (0, [0]) ((0, []), []) after)

            // Open Match Pattern
            elif starts_with str "|" && ends_with str "->" then
                (peek (peek last), str) :: (aux (push_increment (peek last)) last after)
            // Single-line Match Pattern
            elif starts_with str "|" then
                (peek (peek last), str) :: (aux (pop (peek last)) last after)
            // Match Begin
            elif starts_with str "match" then
                (peek stackn, str) :: (aux (pop stackn) (stackn |> push last) after)

            // Open Return Value
            elif str = "else" || ends_with str "->" || ends_with str "in" then
                let lastp = if count last > 1 then pop last else last
                (peek stackn, str) :: (aux (push_increment stackn) lastp after)

            // Open Intermediate
            elif ends_with str "then" || ends_with str "=" then
                (peek stackn, str) :: (aux ((peek stackn + 1) |> (push stackn)) last after)
            // Single-line Intermediate
            elif str = "" || starts_with str "let" || starts_with str "if" || starts_with str "elif" then
                (peek stackn, str) :: (aux stackn last after)

            // Single-line Return Value
            else
                let lastp = if count last > 1 then pop last else last
                (peek stackn, str) :: (aux (pop stackn) lastp after)
        with
        | :? ArgumentException -> failwithf "Invalid input in line: %s" str

    in aux (0, [0]) ((0, []), []) lines


/// Splits F# code given ideal width in characters
let split (w : int) (s : string) = split_lines s
    //let rec aux2 acc state current_width = function
    //| x :: xs ->
    //    if x = "let" then
    //        aux2 (acc + " " + x) "let" (current_width + x.Length) xs
    //    elif x = "if" || x = "elif" then
    //        aux2 (acc + " " + x) "if_case" (current_width + x.Length) xs
    //    elif x = "=" && state = "let" then
    //        (acc + " " + x) :: (aux2 "" "" 0 xs)
    //    elif x = "then" && state = "if_case" then
    //        (acc + " " + x) :: (aux2 "" "" 0 xs)
    //    else 
    //        aux2 (acc + " " + x) state (current_width + x.Length) xs
    //| [] -> [acc]
    //in aux2 "" "" 0 (s |> clean_string |> tokenize_line)

        (*if s.Length < w then
            x :: aux xs

        elif contains x "=" && not(ends_with x "=") && not(starts_with x "if") then
            let (a, b) = split_line_inc "=" x
            a :: aux (b :: xs)
          
        elif contains x "->" && not(ends_with x "->") then
            let (a, b) = split_line_inc "->" x
            a :: aux (b :: xs)

        elif contains x "then" && not(ends_with x "then") then
            let (a, b) = split_line_inc "then" x
            a :: aux (b :: xs)

        elif contains x "else" then
            let (a, b) = split_line_exc "else" x
            let (c, d) = split_line_exc "else" x
            a :: b :: aux xs

        elif contains x "|" then
            let (a, b) = split_line_exc "|" x
            a :: b :: aux xs

        else x :: aux xs
    
    in split_lines s |> aux*)