type angleMode =
  | @as(0) Degree
  | @as(1) Radian
  | @as(2) Gradian

type config = {angleMode: angleMode}

let defaultConfig = {angleMode: Radian}

type rec t =
  | @as(0) NaN
  | @as(1) Zero
  | @as(2) One
  | @as(3) MinusOne
  | @as(4) I
  | @as(5) MinusI
  | @as(6) Pi
  | @as(7) E
  | @as(8) XUnit
  | @as(9) YUnit
  | @as(10) ZUnit
  | @as(11) OfInt(int)
  | @as(12) OfFloat(float)
  | @as(13) OfString(string)
  | @as(14) OfStringBase(int, string)
  | @as(15) OfEncoded(string)
  | @as(16) Vector(array<t>)
  | @as(17) Matrix({numRows: int, numColumns: int, elements: array<t>})
  | @as(18) Percent(t)
  | @as(19) Measure(t, array<unitsType>)
  | @as(20) Variable(string)
  | @as(21) Add(t, t)
  | @as(22) Sub(t, t)
  | @as(23) Mul(t, t)
  | @as(24) Div(t, t)
  | @as(25) Pow(t, t)
  | @as(26) Dot(t, t)
  | @as(27) Neg(t)
  | @as(28) Abs(t)
  | @as(29) Floor(t)
  | @as(30) Ceil(t)
  | @as(31) Round(t)
  | @as(32) Sqrt(t)
  | @as(33) Exp(t)
  | @as(34) Log(t)
  | @as(35) LogBase({base: t, body: t})
  | @as(36) Sin(t)
  | @as(37) Asin(t)
  | @as(38) Cosec(t)
  | @as(39) Sinh(t)
  | @as(40) Asinh(t)
  | @as(41) Cos(t)
  | @as(42) Acos(t)
  | @as(43) Sec(t)
  | @as(44) Cosh(t)
  | @as(45) Acosh(t)
  | @as(46) Tan(t)
  | @as(47) Atan(t)
  | @as(48) Cot(t)
  | @as(49) Tanh(t)
  | @as(50) Atanh(t)
  | @as(51) OfDeg(t)
  | @as(52) OfArcMin(t)
  | @as(53) OfArcSec(t)
  | @as(54) OfRad(t)
  | @as(55) OfGrad(t)
  | @as(56) ToDeg(t)
  | @as(57) ToRad(t)
  | @as(58) ToGrad(t)
  | @as(59) Re(t)
  | @as(60) Im(t)
  | @as(61) Conj(t)
  | @as(62) Gamma(t)
  | @as(63) Factorial(t)
  | @as(64) Rand
  | @as(65) RandInt(t, t)
  | @as(66) NPR(t, t)
  | @as(67) NCR(t, t)
  | @as(68) Rem(t, t)
  | @as(69) Min(t, t)
  | @as(70) Max(t, t)
  | @as(71) Gcd(t, t)
  | @as(72) Lcm(t, t)
  | @as(73) Rref(t)
  | @as(74) Trace(t)
  | @as(75) Transpose(t)
  | @as(76) X
  | @as(77) Y
  | @as(78) Z
  | @as(79) Differential({at: t, body: t})
  | @as(80) Integral({from: t, to: t, body: t})
  | @as(81) Sum({from: t, to: t, body: t})
  | @as(82) Product({from: t, to: t, body: t})
  | @as(83) Equation({body: t, arguments: array<t>})
  | @as(84) EquationArg(int)
and unitsType = {
  prefix: Units_Types.prefix,
  name: Units_Types.name,
  power: t,
}
