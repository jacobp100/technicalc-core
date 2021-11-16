open AST

module Placeholder = {
  let attributes = list{("class", "placeholder"), ("mathvariant", "normal")}
  let element = "mi"
  let body = "&#x25a1;"
}

let stringOfOperator = x =>
  switch x {
  | Op_Add => "+"
  | Op_Sub => "-"
  | Op_Mul => "&#x00D7;"
  | Op_Div => "&#x00F7;"
  | Op_Dot => "&#xb7;"
  | Op_Rem => "mod"
  }

let stringOfFunction = x =>
  switch x {
  | Fn_Sin => "sin"
  | Fn_Asin => "arcsin"
  | Fn_Cosec => "cosec"
  | Fn_Sinh => "sinh"
  | Fn_Asinh => "arcsinh"
  | Fn_Cos => "cos"
  | Fn_Acos => "arccos"
  | Fn_Sec => "sec"
  | Fn_Cosh => "cosh"
  | Fn_Acosh => "arccosh"
  | Fn_Tan => "tan"
  | Fn_Atan => "arctan"
  | Fn_Cot => "cot"
  | Fn_Tanh => "tanh"
  | Fn_Atanh => "arctanh"
  | Fn_Deg => "deg"
  | Fn_Grad => "grad"
  | Fn_Rad => "rad"
  | Fn_Log => "log"
  | Fn_Re => "re"
  | Fn_Im => "im"
  | Fn_Gamma => "&#x0393;"
  }

let stringOfBase = base =>
  switch base {
  | Base_Bin => "0b"
  | Base_Oct => "0o"
  | Base_Hex => "0x"
  }
