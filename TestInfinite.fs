﻿module TestInfinite

let infinite = """
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
let private pop = function
    | (a, x :: y :: xs) -> (x, y :: xs)
    | (a, x :: []) -> (x, [])
    | (a, []) -> raise (ArgumentException("Stack empty"))

/// Peeks an element from the stack, returns the first element and the stack
let private peek = function
    | (a, x :: xs) -> x
    | (a, []) -> raise (ArgumentException("Stack empty"))

/// Returns the number of elements on the stack
let rec private count = function
    | (a, x :: xs) -> 1 + count (a, xs)
    | (a, []) -> 0


// -------------------------------------------
//  List Functions                          
// -------------------------------------------

/// Create a new List with a function applied
let rec private map f = function
    | [] -> []
    | x :: xs -> (f x) :: (map f xs)

/// Create a new List without the elements that match a given predicate
let rec private filter f = function
    | [] -> []
    | x :: xs -> if f x then x :: (filter f xs) else filter f xs

/// Return if a given predicate is true at least with one item of a List
let rec private exists f = function
    | [] -> false
    | x :: xs -> (f x) || (exists f xs)

/// Create a new list that contains every item of a list
let rec private list_concat = function
    | [] -> []
    | x :: xs -> x @ list_concat xs


// -------------------------------------------
//  String Functions                          
// -------------------------------------------

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

/// Returns true if the first token of a string is a member of tok
let private starts_with_mul (str : string) tok =
    exists (starts_with str) tok

/// Returns true if the last token of a string is a member of tok
let private ends_with_mul (str : string) tok =
    exists (ends_with str) tok

/// Create a string from a list with a separator
let rec private string_concat separator = function
    | [] -> ""
    | x :: [] -> x
    | x :: xs -> x + " " + (string_concat separator xs)


// -------------------------------------------
//  PUBLIC FUNCTIONS
// -------------------------------------------

/// Indents splitted lines of code
let rec indent (lines : string list) =
    let rec aux stack last_stack match_opening_tabs i = function
    | [] ->
        // Check if every binding was closed
        if count stack = 1 then []
        else failwith "Invalid input: unclosed binding"
    | str :: after ->
            // Current line indendation
            let current_line =
                if starts_with str "|" then
                    (peek (peek last_stack), str) // Recover last stack state
                else (peek stack, str)
            // Current indendation stack
            let current_stack =
                if starts_with str "|" then
                    peek last_stack // Recover last stack state
                else stack

            // Stack value for the next line
            let next_stack =
                // Open Return Value
                if str = "else" || ends_with_mul str ["->"; "in"] then
                    (peek current_stack + 1) |> push (pop current_stack)
                // Open Intermediate
                elif ends_with_mul str ["then"; "="] then
                    (peek current_stack + 1) |> (push current_stack)
                // Single-line Intermediate
                elif str = "" || starts_with_mul str ["let"; "if"; "elif"] then
                    current_stack
                // Single-line Return Value
                else pop current_stack

            // Stack recovery (for pattern matching) value for the next line
            // Keep also tab level of match begin to detect match ending
            let (next_last_stack, next_match_opening_tabs) =
                // Match begin: push the state into the stack
                // Every pattern after a match begin must be at that tab level
                if starts_with str "match" then
                    (stack |> push last_stack, peek stack |> push match_opening_tabs)
                // Known intermediate token: do nothing
                elif starts_with_mul str ["|"; "let"; "if"; "elif"] || ends_with_mul str ["then"; "="] || str = "" then
                    (last_stack, match_opening_tabs)
                // Return value: check if indendation is behind match
                elif count match_opening_tabs > 0 && (peek match_opening_tabs) > peek stack then
                    (pop last_stack, pop match_opening_tabs)
                // Return value (after the match)
                else (last_stack, match_opening_tabs)

            // Append current line and continue recursion
            // (adding an empty line between top-level bindings)
            if peek current_stack = 0 then
                (0, "") :: current_line :: (aux next_stack next_last_stack next_match_opening_tabs (i + 1) after)
            else
                current_line :: (aux next_stack next_last_stack next_match_opening_tabs (i + 1) after)
    in aux (0, [0]) ((0, []), []) (0, []) 0 lines


/// Splits F# code given ideal width in characters
let split (w : int) (s : string) =

    /// Splits everytime it's possible
    let rec split_all acc if_case in_string : string list -> string list list = function
    | [] -> [acc]
    | x :: xs ->
        // Fix for keywords inside a string
        if x.StartsWith("\"") then
            split_all (acc @ [x]) if_case true xs
        elif x.EndsWith("\"") then
            split_all (acc @ [x]) if_case false xs

        // Fix for an = inside an if case
        elif not(in_string) && (x = "if" || x = "elif") then
            acc :: split_all [x] true in_string xs
        elif not(in_string) && (x = "then") then
            (acc @ [x]) :: split_all [] false in_string xs

        // ELSE and IN : split before and after
        elif not(in_string) && (x = "else" || x = "in") then
            acc :: [x] :: split_all [] if_case in_string xs

        // Split before this keyword
        elif not(in_string) && (x = "match" || x = "let" || x = "|") then
            acc :: split_all [x] if_case in_string xs
        // Split after this keyword
        elif not(in_string) && ((x = "=" && not(if_case)) || x = "->") then
            (acc @ [x]) :: split_all [] if_case in_string xs

        // Not a special keyword: add to accumulator
        else split_all (acc @ [x]) if_case in_string xs

    /// Joins lines if possible
    let rec split_collect : string list -> string list = function
    | [] -> []
    | last :: [] -> [last]
    | first :: second :: xs ->
        if second.Length < w && ends_with_mul first ["->"; "="; "then"; "else"; "in"] && not(starts_with_mul second ["fun"; "if"; "elif"; "in"; "match"; "let"; "else"; "|"]) then
               // Collect two lines
               (first + " " + second) :: split_collect xs
        else first :: split_collect (second :: xs)


    in split_lines s |> map (tokenize_line >> split_all [] false false >> map (string_concat " ")) |> list_concat |> filter (fun x -> x <> "") |> map trim_line |> split_collect
"""