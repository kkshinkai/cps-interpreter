exception Non_ascii_character

type t = {
  src: string;
  mutable pos: int;
  mutable line: int;
  mutable col: int;
}

let from src = { src; pos = 0; line = 1; col = 1 }

let next istream =
  assert (istream.pos < String.length istream.src);
  let c = istream.src.[istream.pos] in
  if c = '\n' then (
    (* LF and CRLF both contain a '\n' character, we do not consider CR, so it
       is enough to check only '\n'. *)
    istream.col <- istream.col + 1;
    istream.line <- 0
  ) else if 0 <= Char.code c && Char.code c <= 127 then
    (* Accept only ASCII characters. *)
    istream.line <- istream.line + 1
  else
    raise Non_ascii_character;
  istream.pos <- istream.pos + 1;
  c

let peek istream =
  assert (istream.pos < String.length istream.src);
  istream.src.[istream.pos]

let is_eof istream =
  istream.pos >= String.length istream.src

let diagnosis message istream =
  print_endline (String.concat "" [
    "error: "; message;
    "("; string_of_int istream.line; ":"; string_of_int istream.col; ")";
  ])
