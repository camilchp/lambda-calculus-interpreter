{
open Lexing
open Parser

exception EOF

}

let white = [' ' '\t']+
let newline = '\n' | '\r' | "\r\n"
let var = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* | '"'[^'\n']*'"'

rule token =
  parse
  | white          { token lexbuf              }
  | var            { VAR (lexeme lexbuf)       }
  | "λ" | "/"      { LAMBDA                    }
  | '('            { LPAR                      }
  | ')'            { RPAR                      }
  | '.'            { DOT                       }
  | ':'            { COLON                     }
  | newline        { EOL                       }
  | eof            { raise EOF                 }