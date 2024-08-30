type angleMode =
  | Degree
  | Radian
  | Gradian

type config = {angleMode: angleMode}

let defaultConfig = {angleMode: Radian}

type rec t =
  | NaN
  | Zero
  | One
  | MinusOne
  | I
  | MinusI
  | Pi
  | E
  | XUnit
  | YUnit
  | ZUnit
  | OfInt(int)
  | OfFloat(float)
  | OfString(string)
  | OfStringBase(int, string)
  | OfEncoded(string)
  | Vector(array<t>)
  | Matrix({numRows: int, numColumns: int, elements: array<t>})
  | Percent(t)
  | Measure(t, array<unitsType>)
  | Variable(string)
  | Add(t, t)
  | Sub(t, t)
  | Mul(t, t)
  | Div(t, t)
  | Pow(t, t)
  | Dot(t, t)
  | Neg(t)
  | Abs(t)
  | Floor(t)
  | Ceil(t)
  | Round(t)
  | Sqrt(t)
  | Exp(t)
  | Log(t)
  | LogBase({base: t, body: t})
  | Sin(t)
  | Asin(t)
  | Cosec(t)
  | Sinh(t)
  | Asinh(t)
  | Cos(t)
  | Acos(t)
  | Sec(t)
  | Cosh(t)
  | Acosh(t)
  | Tan(t)
  | Atan(t)
  | Cot(t)
  | Tanh(t)
  | Atanh(t)
  | OfDeg(t)
  | OfArcMin(t)
  | OfArcSec(t)
  | OfRad(t)
  | OfGrad(t)
  | ToDeg(t)
  | ToRad(t)
  | ToGrad(t)
  | Re(t)
  | Im(t)
  | Conj(t)
  | Gamma(t)
  | Factorial(t)
  | Rand
  | RandInt(t, t)
  | NPR(t, t)
  | NCR(t, t)
  | Rem(t, t)
  | Min(t, t)
  | Max(t, t)
  | Gcd(t, t)
  | Lcm(t, t)
  | Rref(t)
  | Trace(t)
  | Transpose(t)
  | X
  | Y
  | Z
  | Differential({at: t, body: t})
  | Integral({from: t, to: t, body: t})
  | Sum({from: t, to: t, body: t})
  | Product({from: t, to: t, body: t})
  | Equation({body: t, arguments: array<t>})
  | EquationArg(int)
and unitsType = {
  prefix: Units_Types.prefix,
  name: Units_Types.name,
  power: t,
}
