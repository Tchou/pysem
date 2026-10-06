module PC = PyreAst.Concrete
module PCI = PC.Identifier
module PCL = PC.Location

module MC = Mlsem.Common
module MS = Mlsem.System
module ML = Mlsem.Lang
module MT = Mlsem.Types
module MlVar = MC.Variable
module MlMVar = ML.MVariable
module MLAst = ML.Ast
module MSAst = MS.Ast
module MlGTy = MT.GTy

let fail msg = Format.kfprintf (fun _ -> assert false) Format.err_formatter msg
