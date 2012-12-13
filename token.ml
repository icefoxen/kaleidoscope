type token =
 | Def
 | Extern
 | Ident of string
 | Number of float
 | Kwd of char
 | If | Then | Else | For | In

