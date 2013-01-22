namespace SExpress

module SExpressions =

  open System.Text

  //Modified from code on http://rosettacode.org/wiki/S-Expressions
(** This module is a very simple parsing library for S-expressions. *)
(* Copyright (C) 2009  Florent Monnier, released under MIT license. *)

  type sexpr = Atom of string | Expr of sexpr list

  type state =
    | Parse_root of sexpr list
    | Parse_content of sexpr list
    | Parse_word of StringBuilder * sexpr list
    | Parse_string of bool * StringBuilder * sexpr list
 
  let parse pop_char =
    let rec aux st =
      match pop_char() with
      | None ->
          match st with
          | Parse_root sl -> (List.rev sl)
          | _ ->
              failwith "Parsing error: content not closed by parenthesis"
      | Some c ->
          match c with
          | '(' ->
              match st with
              | Parse_root sl ->
                  let this = aux(Parse_content [])
                  aux(Parse_root((Expr this)::sl))
              | Parse_content sl ->
                  let this = aux(Parse_content [])
                  aux(Parse_content((Expr this)::sl))
              | Parse_word(w, sl) ->
                  let this = aux(Parse_content [])
                  aux(Parse_content((Expr this)::Atom(w.ToString())::sl))
              | Parse_string(_, s, sl) -> aux(Parse_string(false, s.Append(c), sl))
          | ')' ->
              match st with
              | Parse_root sl ->
                  failwith "Parsing error: closing parenthesis without openning"
              | Parse_content sl -> (List.rev sl)
              | Parse_word(w, sl) -> List.rev(Atom(w.ToString())::sl)
              | Parse_string(_, s, sl) -> aux(Parse_string(false, s.Append(c), sl))
          | ' ' | '\n' | '\r' | '\t' ->
              match st with
              | Parse_root sl -> aux(Parse_root sl)
              | Parse_content sl -> aux(Parse_content sl)
              | Parse_word(w, sl) -> aux(Parse_content(Atom(w.ToString())::sl))
              | Parse_string(_, s, sl) -> aux(Parse_string(false, s.Append(c), sl))
          | '"' ->
              (* '"' *)
              match st with
              | Parse_root _ -> failwith "Parse error: double quote at root level"
              | Parse_content sl ->
                  let s = new StringBuilder()
                  aux(Parse_string(false, s, sl))
              | Parse_word(w, sl) ->
                  let s = new StringBuilder()
                  aux(Parse_string(false, s, Atom(w.ToString())::sl))
              | Parse_string(true, s, sl) -> 
                  aux(Parse_string(false, s.Append(c), sl))
              | Parse_string(false, s, sl) ->
                  aux(Parse_content(Atom(s.ToString())::sl))
          | '\\' ->
              match st with
              | Parse_string(true, s, sl) ->
                  aux(Parse_string(false, s.Append(c), sl))
              | Parse_string(false, s, sl) ->
                  aux(Parse_string(true, s, sl))
              | _ ->
                  failwith "Parsing error: escape character wrong place"
          | _ ->
              match st with
              | Parse_root _ ->
                  failwith(Printf.sprintf "Parsing error: char '%c' at root level" c)
              | Parse_content sl ->
                  let w = new StringBuilder()
                  aux(Parse_word(w.Append(c), sl))
              | Parse_word(w, sl) ->
                  aux(Parse_word(w.Append(c), sl))
              | Parse_string(_, s, sl) ->
                  aux(Parse_string(false, s.Append(c), sl))
   
    aux (Parse_root [])
 
 
  let string_pop_char str =
    let len = String.length str
    let i = ref(-1)
    (function () -> incr i; if !i >= len then None else Some(str.[!i]))
 
 
  let parse_string str =
    parse (string_pop_char str)


  let ic_pop_char (ic:string) =
    let i = ref -1
    (function () ->
       try 
        incr i
        Some(ic.Chars(!i))
       with End_of_file -> (None))
 
 
  let parse_ic(ic:string) =
    parse (ic_pop_char ic)
 
  // 
  //let parse_file filename =
  //  let ic = open_in filename
  //  let res = parse_ic ic
  //  close_in ic;
  //  (res)
  // 
 
  let quote s = "\"" + s + "\""
 
  let needs_quote (s:string) =
    List.exists (s.Contains) [" "; "\n"; "\r"; "\t"; "("; ")"]
 
  let protect s =
    //let s = String(s)
    if needs_quote s then quote s else s
 
 
  let string_of_sexpr s =
    let rec aux acc = function
    | (Atom tag)::tl -> aux ((protect tag)::acc) tl
    | (Expr e)::tl ->
        let s =
          "(" + (String.concat " " (aux [] e)) + ")"
       
        aux (s::acc) tl
    | [] -> (List.rev acc)
   
    String.concat " " (aux [] s)

  let print_sexpr s = (string_of_sexpr s)
 
  let string_of_sexpr_indent s =
    let rec aux i acc = function
    | (Atom tag)::tl -> aux i ((protect tag)::acc) tl
    | (Expr e)::tl ->
        let s =
          (if i > 1 then "\n" + String.replicate i " " else "") + "(" +
          (String.concat " " (aux (i+1) [] e))
          + ")"
       
        aux i (s::acc) tl
    | [] -> (List.rev acc)
   
    String.concat "\n" (aux 1 [] s)
 
 
  let print_sexpr_indent s =
    (string_of_sexpr_indent s)

