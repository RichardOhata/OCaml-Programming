(* 
 * Sample data corresponding to Client data type with 2 variants:
 *   org(123,BCIT)
 *   org(345,hell holdings\,Ltd)
 *   org(456,ocaml \(LLC\))
 *   person(666,name(monty,burns))
 * Note that comma, brackets and newlines are escaped
 *)
open Angstrom

type name = Name of string * string  (* first and last names *)
type client = Org of int * string         (* ID, name *)
            | Person of int * name  

let name first last = Name (first, last)
let org id name = Org (id, name)
let person id name = Person (id, name)

let ws1 =
  take_while1 (function | ' ' | '\t' | '\r' | '\n' -> true | _ -> false)

(* handling escaped strings *)
let const x _ = x

let a_char =
  (const '(' <$> string "\\(") <|>
  (const ')' <$> string "\\)") <|>
  (const ',' <$> string "\\,") <|>
  (const '\n' <$> string "\\n") <|>
  satisfy (fun c -> c <> '(' && c <> ')' && c <> ',' && c <> '\n')

let a_string =
  many1 a_char >>| fun l -> String.of_seq @@ List.to_seq l

let a_number =
  take_while1 (function | '0'..'9' -> true | _ -> false) >>| int_of_string

let a_name =
  name <$> string "name(" *> a_string <* char ',' <*> a_string <* char ')'

let a_person =
  person <$> string "person(" *> a_number <* char ',' <*> a_name <* char ')'

let an_org =
  org <$> string "org(" *> a_number <* char ',' <*> a_string <* char ')'

let a_client = an_org <|> a_person

let parse file =
  let ic = open_in file in
  let content = really_input_string ic @@ in_channel_length ic in
  parse_string ~consume:All (many (a_client <* ws1)) content
