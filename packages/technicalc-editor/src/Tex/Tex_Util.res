open AST

let stringOfOperator = x =>
  switch x {
  | Op_Add => "+"
  | Op_Sub => "-"
  | Op_Mul => "\\times"
  | Op_Div => "\\div"
  | Op_Dot => "\\cdot"
  | Op_Rem => "\\mod"
  }

let stringOfFunction = x =>
  switch x {
  | Fn_Sin => "\\sin"
  | Fn_Asin => "\\arcsin"
  | Fn_Cosec => "\\cosec"
  | Fn_Sinh => "\\sinh"
  | Fn_Asinh => "\\arcsinh"
  | Fn_Cos => "\\cos"
  | Fn_Acos => "\\arccos"
  | Fn_Sec => "\\sec"
  | Fn_Cosh => "\\cosh"
  | Fn_Acosh => "\\arccosh"
  | Fn_Tan => "\\tan"
  | Fn_Atan => "\\arctan"
  | Fn_Cot => "\\cot"
  | Fn_Tanh => "\\tanh"
  | Fn_Atanh => "\\arctanh"
  | Fn_Deg => "\\deg"
  | Fn_Grad => "\\grad"
  | Fn_Rad => "\\rm{rad}"
  | Fn_Log => "\\log"
  | Fn_Re => "\\rm{re}"
  | Fn_Im => "\\rm{im}"
  | Fn_Rref => "\\rm{rref}"
  | Fn_Trace => "\\rm{trace}"
  | Fn_Gamma => "\\Gamma"
  }

let stringOfBase = base =>
  switch base {
  | Base_Bin => "\\rm{0b}"
  | Base_Oct => "\\rm{0o}"
  | Base_Hex => "\\rm{0x}"
  }
