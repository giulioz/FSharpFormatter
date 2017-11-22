module FSharpCodeFormatter.Formatter

open Lib

// se non vuoi realizzare la versione avanzata, non modificarla
let split (w : int) (s : string) = split_lines s

let parseLine str =
    let rec aux = function
    | "=" :: [] -> 1
    | "->" :: [] -> 1
    | "then" :: [] -> 1
    | "else" :: [] -> 1
    | "with" :: [] -> 0
    | [] -> 0
    | x :: xs -> aux xs

    aux (tokenize_line str)

// questa è la funzione principale da implementare correttamente sia per versione avanzata che per quella normale
let rec indent (lines : string list) =
    let rec aux lines t acc =
        match lines with
        | [] -> []
        | s :: ss -> (t, s) :: aux ss (t + parseLine s)
    aux lines 0