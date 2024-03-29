type base =
  | Base_Bin
  | Base_Oct
  | Base_Hex
type fn<'a> =
  | Fn_Acos
  | Fn_Acosh
  | Fn_Asin
  | Fn_Asinh
  | Fn_Atan
  | Fn_Atanh
  | Fn_Deg
  | Fn_Gamma
  | Fn_Grad
  | Fn_Im
  | Fn_Log
  | Fn_Rad
  | Fn_Re
  | Fn_Rref
  | Fn_Trace
  /* Superscripts supported */
  | Fn_Cos
  | Fn_Cosec
  | Fn_Cosh
  | Fn_Cot
  | Fn_Sec
  | Fn_Sin
  | Fn_Sinh
  | Fn_Tan
  | Fn_Tanh
  /* Function-like */
  | Fn_NLog({base: 'a})
  | Fn_Sum({from: 'a, to: 'a})
  | Fn_Product({from: 'a, to: 'a})
type angle =
  | Angle_Radian
  | Angle_Degree
  | Angle_ArcMinute
  | Angle_ArcSecond
  | Angle_Gradian
type cmp = Cmp_Eq | Cmp_Gt | Cmp_Gte | Cmp_Lt | Cmp_Lte
