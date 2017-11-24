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

/// Returns if the first token of a string is tok
//let StartsWith 


// -------------------------------------------
//  PUBLIC FUNCTIONS
// -------------------------------------------

/// Indents splitted lines of code
let indent (lines : string list) =
    let rec aux (lines : string list) stack =
        match lines with
        | [] -> []
        | s :: ss ->
            if String.IsNullOrWhiteSpace s then
                (0, s) :: (aux ss (0, []))

            elif s.EndsWith "="      || s.EndsWith "->"
              || s.EndsWith "then"   || s.EndsWith "else"
              || s.EndsWith "with" then
                (peek stack, s) :: (aux ss (push (peek stack + 1) stack))

            elif s.StartsWith "in"   || s.StartsWith "else" then
                (peek stack, s) :: (aux ss stack)

            else
                (peek stack, s) :: (aux ss (pop stack))
    aux lines (0, [])

// se non vuoi realizzare la versione avanzata, non modificarla
let split (w : int) (s : string) = Lib.split_lines s
