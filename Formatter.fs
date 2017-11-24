// -------------------------------------------
//   FSharpCodeFormatter
//   (aka MagicFSharpCodeFormatter)
//
// Giulio Zausa, Marco Perrone
// thanks to Alessio Marotta for the stack idea
// and to Dario Lazzaro for finding an non-existent bug
// -------------------------------------------

module FSharpCodeFormatter.Formatter

open System


// -------------------------------------------
//  Stack Functions
// -------------------------------------------

/// Pushes an element into the stack, returns the element added and the stack
let private push element = function
    | (a, b) -> (element, element :: b)

/// Pops an element from the stack, returns the element removed and the stack
let private pop = function
    | (a, x :: xs) -> (x, xs)
    | (a, []) -> (a, [])

/// Peeks an element from the stack, returns the first element and the stack
let private peek = function
    | (a, x :: xs) -> x
    | (a, []) -> a


// -------------------------------------------
//  String Functions
// -------------------------------------------

/// Returns true if the first token of a string is tok
let private StartsWith (str : string) tok = str.StartsWith tok

/// Returns true if the last token of a string is tok
let private EndsWith (str : string) tok = str.EndsWith tok


// -------------------------------------------
//  Language Definitions
// -------------------------------------------

/// Returns true if the following line closes everything
let private IsClearToken = String.IsNullOrWhiteSpace

/// Returns true if the following line opens indendation
let private IsTabToken str = List.exists (EndsWith str)     <| ["="; "->"; "then"; "else"; "with"]

/// Returns true if the following line closes indendation
let private IsCloseToken str = List.exists (StartsWith str) <| ["in"; "else"]




// -------------------------------------------
//  PUBLIC FUNCTIONS
// -------------------------------------------

/// Indents splitted lines of code
let indent (lines : string list) =
    let rec aux stack = function
        | [] -> [] // EOF
        | s :: ss when IsClearToken s ->
            (0, s) :: (ss |> aux (0, []))
        | s :: ss when IsTabToken s ->
            (peek stack, s) :: (ss |> aux (push (peek stack + 1) stack))
        | s :: ss when IsCloseToken s ->
            (peek stack, s) :: (ss |> aux (pop stack))
        | s :: ss ->
            (peek stack, s) :: (ss |> aux stack)
    lines |> aux (0, [])


// se non vuoi realizzare la versione avanzata, non modificarla
let split (w : int) (s : string) = Lib.split_lines s
