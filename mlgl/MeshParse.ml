type token =
  | MTLLIB
  | USEMTL
  | O
  | S
  | WORD of (string)
  | VECTOR
  | NORMAL
  | TEXTURE
  | POLY
  | SEP
  | FLOAT of (float)
  | INT of (int)
  | EOL
  | EOF

open Parsing;;
let yytransl_const = [|
  257 (* MTLLIB *);
  258 (* USEMTL *);
  259 (* O *);
  260 (* S *);
  262 (* VECTOR *);
  263 (* NORMAL *);
  264 (* TEXTURE *);
  265 (* POLY *);
  266 (* SEP *);
  269 (* EOL *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  261 (* WORD *);
  267 (* FLOAT *);
  268 (* INT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\004\000\005\000\006\000\008\000\
\009\000\011\000\010\000\007\000\012\000\012\000\013\000\013\000\
\013\000\013\000\013\000\000\000"

let yylen = "\002\000\
\002\000\001\000\002\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\004\000\004\000\004\000\002\000\
\002\000\002\000\002\000\002\000\001\000\002\000\005\000\004\000\
\004\000\003\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\004\000\028\000\000\000\000\000\005\000\006\000\
\007\000\008\000\009\000\010\000\011\000\012\000\016\000\017\000\
\019\000\018\000\000\000\000\000\000\000\000\000\020\000\000\000\
\001\000\003\000\000\000\000\000\000\000\000\000\022\000\013\000\
\014\000\015\000\000\000\000\000\025\000\000\000\023\000"

let yydgoto = "\002\000\
\012\000\013\000\014\000\015\000\016\000\017\000\018\000\019\000\
\020\000\021\000\022\000\031\000\032\000"

let yysindex = "\006\000\
\002\255\000\000\007\255\008\255\011\255\012\255\009\255\010\255\
\013\255\006\255\000\000\000\000\019\000\002\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\014\255\015\255\017\255\019\255\000\000\006\255\
\000\000\000\000\020\255\021\255\022\255\246\254\000\000\000\000\
\000\000\000\000\018\255\024\255\000\000\023\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\022\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\001\000\000\000\014\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\027\000\000\000\000\000\040\000\000\000"

let yygindex = "\000\000\
\000\000\009\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\004\000\000\000"

let yytablesize = 309
let yytable = "\043\000\
\027\000\044\000\003\000\004\000\005\000\006\000\001\000\007\000\
\008\000\009\000\010\000\023\000\024\000\021\000\011\000\025\000\
\026\000\030\000\033\000\027\000\028\000\002\000\034\000\029\000\
\035\000\036\000\026\000\037\000\038\000\045\000\040\000\041\000\
\042\000\046\000\047\000\039\000\000\000\000\000\000\000\024\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\027\000\027\000\027\000\027\000\000\000\027\000\027\000\
\027\000\027\000\000\000\000\000\027\000\027\000\021\000\021\000\
\021\000\021\000\000\000\021\000\021\000\021\000\021\000\000\000\
\000\000\000\000\021\000\026\000\026\000\026\000\026\000\000\000\
\026\000\026\000\026\000\026\000\000\000\000\000\000\000\026\000\
\024\000\024\000\024\000\024\000\000\000\024\000\024\000\024\000\
\024\000\000\000\000\000\000\000\024\000"

let yycheck = "\010\001\
\000\000\012\001\001\001\002\001\003\001\004\001\001\000\006\001\
\007\001\008\001\009\001\005\001\005\001\000\000\013\001\005\001\
\005\001\012\001\000\000\011\001\011\001\000\000\014\000\011\001\
\011\001\011\001\000\000\011\001\010\001\012\001\011\001\011\001\
\011\001\010\001\012\001\032\000\255\255\255\255\255\255\000\000\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\002\001\003\001\004\001\255\255\006\001\007\001\
\008\001\009\001\255\255\255\255\012\001\013\001\001\001\002\001\
\003\001\004\001\255\255\006\001\007\001\008\001\009\001\255\255\
\255\255\255\255\013\001\001\001\002\001\003\001\004\001\255\255\
\006\001\007\001\008\001\009\001\255\255\255\255\255\255\013\001\
\001\001\002\001\003\001\004\001\255\255\006\001\007\001\008\001\
\009\001\255\255\255\255\255\255\013\001"

let yynames_const = "\
  MTLLIB\000\
  USEMTL\000\
  O\000\
  S\000\
  VECTOR\000\
  NORMAL\000\
  TEXTURE\000\
  POLY\000\
  SEP\000\
  EOL\000\
  EOF\000\
  "

let yynames_block = "\
  WORD\000\
  FLOAT\000\
  INT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'directive_list) in
    Obj.repr(
# 21 "MeshParse.mly"
                                          ( _1 )
# 193 "MeshParse.ml"
               : Mesh.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'directive) in
    Obj.repr(
# 24 "MeshParse.mly"
                                       ( _1 Mesh.empty )
# 200 "MeshParse.ml"
               : 'directive_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'directive) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'directive_list) in
    Obj.repr(
# 25 "MeshParse.mly"
                                          ( _1 _2 )
# 208 "MeshParse.ml"
               : 'directive_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 28 "MeshParse.mly"
                                       ( fun e -> e )
# 214 "MeshParse.ml"
               : 'directive))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vector) in
    Obj.repr(
# 29 "MeshParse.mly"
                                          ( Mesh.add_vector _1 )
# 221 "MeshParse.ml"
               : 'directive))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'normal) in
    Obj.repr(
# 30 "MeshParse.mly"
                                          ( Mesh.add_normal _1 )
# 228 "MeshParse.ml"
               : 'directive))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'texture) in
    Obj.repr(
# 31 "MeshParse.mly"
                                          ( Mesh.add_texture _1 )
# 235 "MeshParse.ml"
               : 'directive))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'poly) in
    Obj.repr(
# 32 "MeshParse.mly"
                                          ( Mesh.add_polygon _1 )
# 242 "MeshParse.ml"
               : 'directive))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'mtllib) in
    Obj.repr(
# 33 "MeshParse.mly"
                                          ( fun e -> e )
# 249 "MeshParse.ml"
               : 'directive))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'usemtl) in
    Obj.repr(
# 34 "MeshParse.mly"
                                          ( fun e -> e )
# 256 "MeshParse.ml"
               : 'directive))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'o) in
    Obj.repr(
# 35 "MeshParse.mly"
                                          ( fun e -> e )
# 263 "MeshParse.ml"
               : 'directive))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 's) in
    Obj.repr(
# 36 "MeshParse.mly"
                                          ( fun e -> e )
# 270 "MeshParse.ml"
               : 'directive))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : float) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : float) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 39 "MeshParse.mly"
                                       ( Math.Vector.make _2 _3 _4 )
# 279 "MeshParse.ml"
               : 'vector))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : float) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : float) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 42 "MeshParse.mly"
                                       ( Math.Vector.make _2 _3 _4 )
# 288 "MeshParse.ml"
               : 'normal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : float) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : float) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 45 "MeshParse.mly"
                                       ( Math.Vector.make _2 _3 _4 )
# 297 "MeshParse.ml"
               : 'texture))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 48 "MeshParse.mly"
                                       ( _2 )
# 304 "MeshParse.ml"
               : 'mtllib))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 51 "MeshParse.mly"
                                       ( _2 )
# 311 "MeshParse.ml"
               : 'usemtl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 54 "MeshParse.mly"
                                       ( _2 )
# 318 "MeshParse.ml"
               : 's))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 57 "MeshParse.mly"
                                       ( _2 )
# 325 "MeshParse.ml"
               : 'o))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'poly_list) in
    Obj.repr(
# 60 "MeshParse.mly"
                                       ( _2 )
# 332 "MeshParse.ml"
               : 'poly))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'poly_e) in
    Obj.repr(
# 63 "MeshParse.mly"
                                       ( [_1] )
# 339 "MeshParse.ml"
               : 'poly_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'poly_e) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'poly_list) in
    Obj.repr(
# 64 "MeshParse.mly"
                                          ( _1::_2)
# 347 "MeshParse.ml"
               : 'poly_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 67 "MeshParse.mly"
                                       ( _1, Some _3, Some _5 )
# 356 "MeshParse.ml"
               : 'poly_e))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 68 "MeshParse.mly"
                                          ( _1, Some _3, None )
# 364 "MeshParse.ml"
               : 'poly_e))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 69 "MeshParse.mly"
                                          ( _1, None, Some _4 )
# 372 "MeshParse.ml"
               : 'poly_e))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    Obj.repr(
# 70 "MeshParse.mly"
                                          ( _1, None, None )
# 379 "MeshParse.ml"
               : 'poly_e))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 71 "MeshParse.mly"
                                          ( _1, None, None )
# 386 "MeshParse.ml"
               : 'poly_e))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Mesh.t)
