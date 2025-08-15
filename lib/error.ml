exception Syntax of string * PyreAst.Parser.Error.t
exception Unimplemented of (string * PyreAst.Concrete.Location.t)

let syntax file e = raise (Syntax (file, e))

let unimplemented file l = raise (Unimplemented (file, l))