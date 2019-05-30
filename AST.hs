module AST where


type AST = [Global]

data MType
  = 

data Global
  = Declare String MType
  | Define String Params Body
