type base =
  | @as(0) Base_Bin
  | @as(1) Base_Oct
  | @as(2) Base_Hex
type fn<'a> =
  | @as(0) Fn_Acos
  | @as(1) Fn_Acosh
  | @as(2) Fn_Asin
  | @as(3) Fn_Asinh
  | @as(4) Fn_Atan
  | @as(5) Fn_Atanh
  | @as(6) Fn_Deg
  | @as(7) Fn_Gamma
  | @as(8) Fn_Grad
  | @as(9) Fn_Im
  | @as(10) Fn_Log
  | @as(11) Fn_Rad
  | @as(12) Fn_Re
  | @as(13) Fn_Rref
  | @as(14) Fn_Trace
  /* Superscripts supported */
  | @as(15) Fn_Cos
  | @as(16) Fn_Cosec
  | @as(17) Fn_Cosh
  | @as(18) Fn_Cot
  | @as(19) Fn_Sec
  | @as(20) Fn_Sin
  | @as(21) Fn_Sinh
  | @as(22) Fn_Tan
  | @as(23) Fn_Tanh
  /* Function-like */
  | @as(24) Fn_NLog({base: 'a})
  | @as(25) Fn_Sum({from: 'a, to: 'a})
  | @as(26) Fn_Product({from: 'a, to: 'a})
type angle =
  | @as(0) Angle_Radian
  | @as(1) Angle_Degree
  | @as(2) Angle_ArcMinute
  | @as(3) Angle_ArcSecond
  | @as(4) Angle_Gradian
type cmp = Cmp_Eq | @as(0) Cmp_Gt | @as(1) Cmp_Gte | @as(2) Cmp_Lt | @as(3) Cmp_Lte
