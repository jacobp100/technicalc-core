type t =
  | NaN
  | Zero
  | One
  | MinusOne
  | I
  | MinusI
  | Pi
  | E
  | OfInt(int)
  | OfFloat(float)
  | OfString(string)
  | OfStringBase(int, string)
  | OfEncoded(string)
  | Vector(array(t))
  | Matrix({
      numRows: int,
      numColumns: int,
      elements: array(t),
    })
  | Percent(t)
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
  | Sin(t)
  | Asin(t)
  | Sinh(t)
  | Asinh(t)
  | Cos(t)
  | Acos(t)
  | Cosh(t)
  | Acosh(t)
  | Tan(t)
  | Atan(t)
  | Tanh(t)
  | Atanh(t)
  | Re(t)
  | Im(t)
  | Conj(t)
  | Gamma(t)
  | Factorial(t)
  | Rand
  | RandInt(t, t)
  | NPR(t, t)
  | NCR(t, t)
  | Min(t, t)
  | Max(t, t)
  | Gcd(t, t)
  | Lcm(t, t)
  | Differential({
      x: t,
      body: t,
    })
  | Integral({
      a: t,
      b: t,
      body: t,
    })
  | Sum({
      a: t,
      b: t,
      body: t,
    })
  | Product({
      a: t,
      b: t,
      body: t,
    })
  | Convert({
      a: t,
      toUnits: array(Unit_Types.unitPower),
      fromUnits: array(Unit_Types.unitPower),
    });
