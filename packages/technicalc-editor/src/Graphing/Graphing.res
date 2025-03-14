%%private(
  let decodeContext = context =>
    Belt.Array.reduce(context, TechniCalcCalculator.AST_Context.empty, (accum, (key, value)) => {
      TechniCalcCalculator.AST_Context.set(accum, key, value)
    })
)

%%private(
  let floatToMetalShader = (f: float) =>
    if TechniCalcCalculator.FloatUtil.isFinite(f) {
      let s = Belt.Float.toString(f)
      let s = StringUtil.includes(s, ".") ? `${s}f` : `${s}.0f`
      Some(s)
    } else {
      None
    }
)

%%private(
  let valueToMetalShader = (v: TechniCalcCalculator.Value_Types.t) =>
    switch v {
    | #Zero => Some("0.0f")
    | #Real(re) => TechniCalcCalculator.Real.toFloat(re)->floatToMetalShader
    | #Cmpx(_, _) => None
    | #Imag(_) => None
    | #Vect(_) => None
    | #Matx(_) => None
    | #Pcnt(_) => None
    | #Mesr(_) => None
    | #NaNN => None
    }
)

%%private(
  let rec astToMetalShader = (~context, v: TechniCalcCalculator.AST_Types.t): option<string> =>
    switch v {
    | Zero => Some("0.0f")
    | One => Some("1.0f")
    | MinusOne => Some("(-1.0f)")
    | Pi => Some("M_PI_F")
    | E => Some("M_E_F")
    | X => Some("x")
    | Y => Some("y")
    | OfInt(x) => Belt.Int.toFloat(x)->floatToMetalShader
    | OfFloat(x) => floatToMetalShader(x)
    | OfString(x) =>
      switch TechniCalcCalculator.Formatting.ofString(x) {
      | Some(x) => valueToMetalShader(x)
      | None => None
      }
    | OfStringBase(base, x) =>
      switch TechniCalcCalculator.Formatting.ofStringBase(base, x) {
      | Some(x) => valueToMetalShader(x)
      | None => None
      }
    | OfEncoded(encoded) =>
      switch TechniCalcCalculator.Encoding.decode(encoded) {
      | Some(decoded) => valueToMetalShader(decoded)
      | None => None
      }
    | Variable(ident) =>
      switch TechniCalcCalculator.AST_Context.get(context, ident) {
      | Some(v) => valueToMetalShader(v)
      | None => None
      }
    | Add(a, b) =>
      switch (astToMetalShader(~context, a), astToMetalShader(~context, b)) {
      | (Some(a), Some(b)) => Some(`(${a}) + (${b})`)
      | _ => None
      }
    | Sub(a, b) =>
      switch (astToMetalShader(~context, a), astToMetalShader(~context, b)) {
      | (Some(a), Some(b)) => Some(`(${a}) - (${b})`)
      | _ => None
      }
    | Mul(a, b)
    | Dot(a, b) =>
      switch (astToMetalShader(~context, a), astToMetalShader(~context, b)) {
      | (Some(a), Some(b)) => Some(`(${a}) * (${b})`)
      | _ => None
      }
    | Div(a, b) =>
      switch (astToMetalShader(~context, a), astToMetalShader(~context, b)) {
      | (Some(a), Some(b)) => Some(`(${a}) / (${b})`)
      | _ => None
      }
    | Neg(a) =>
      switch astToMetalShader(~context, a) {
      | Some(a) => Some(`(-(${a}))`)
      | None => None
      }
    | LogBase({base: b, body: a}) =>
      switch (astToMetalShader(~context, a), astToMetalShader(~context, b)) {
      | (Some(a), Some(b)) => Some(`(log(${a}) / log(${b}))`)
      | _ => None
      }
    | Pow(a, b) =>
      switch (astToMetalShader(~context, a), astToMetalShader(~context, b)) {
      | (Some(a), Some(b)) => Some(`pow(${a}, ${b})`)
      | _ => None
      }
    | Min(a, b) =>
      switch (astToMetalShader(~context, a), astToMetalShader(~context, b)) {
      | (Some(a), Some(b)) => Some(`min(${a}, ${b})`)
      | _ => None
      }
    | Max(a, b) =>
      switch (astToMetalShader(~context, a), astToMetalShader(~context, b)) {
      | (Some(a), Some(b)) => Some(`max(${a}, ${b})`)
      | _ => None
      }
    | Abs(x) => fn(~context, x, "abs")
    | Floor(x) => fn(~context, x, "floor")
    | Ceil(x) => fn(~context, x, "ceil")
    | Round(x) => fn(~context, x, "round")
    | Sqrt(x) => fn(~context, x, "sqrt")
    | Exp(x) => fn(~context, x, "exp")
    | Log(x) => fn(~context, x, "log")
    | Sin(x) => fn(~context, x, "sin")
    | Asin(x) => fn(~context, x, "asin")
    | Cosec(x) => fn(~context, x, "cosec")
    | Sinh(x) => fn(~context, x, "sinh")
    | Asinh(x) => fn(~context, x, "asinh")
    | Cos(x) => fn(~context, x, "cos")
    | Acos(x) => fn(~context, x, "acos")
    | Sec(x) => fn(~context, x, "sec")
    | Cosh(x) => fn(~context, x, "cosh")
    | Acosh(x) => fn(~context, x, "acosh")
    | Tan(x) => fn(~context, x, "tan")
    | Atan(x) => fn(~context, x, "atan")
    | Cot(x) => fn(~context, x, "cot")
    | Tanh(x) => fn(~context, x, "tanh")
    | Atanh(x) => fn(~context, x, "atanh")
    | OfDeg(_) => None // Legacy encodings
    | OfArcMin(_) => None // Legacy encodings
    | OfArcSec(_) => None // Legacy encodings
    | OfRad(_) => None // Legacy encodings
    | OfGrad(_) => None // Legacy encodings
    | ToDeg(_) => None // Legacy encodings
    | ToRad(_) => None // Legacy encodings
    | ToGrad(_) => None // Legacy encodings
    | Conj(_) => None
    | Differential(_) => None
    | Factorial(_) => None
    | Gamma(_) => None
    | Gcd(_, _) => None
    | I => None
    | Im(_) => None
    | Integral(_) => None
    | Lcm(_, _) => None
    | Matrix(_) => None
    | Measure(_, _) => None
    | MinusI => None
    | NaN => None
    | NCR(_, _) => None
    | NPR(_, _) => None
    | Percent(_) => None
    | Product(_) => None
    | Rand => None
    | RandInt(_, _) => None
    | Re(_) => None
    | Rem(_, _) => None
    | Rref(_) => None
    | Sum(_) => None
    | Trace(_) => None
    | Transpose(_) => None
    | Vector(_) => None
    | XUnit => None
    | YUnit => None
    | Z => None
    | ZUnit => None
    /* FIXME */
    | Equation(_) => None
    | EquationArg(_) => None
    }
  and fn = (~context, v: TechniCalcCalculator.AST_Types.t, name: string) =>
    switch astToMetalShader(~context, v) {
    | Some(x) => Some(`${name}(${x})`)
    | None => None
    }
)

%%private(
  let splitIndex = (v: array<AST.t>) => {
    let rec splitIndex = (~comparison: option<(AST_Categorization.cmp, int)>=None, index) =>
      switch Belt.Array.get(v, index) {
      | Some(
          Eq
          | Gt
          | Gte
          | Lt
          | Lte,
        ) if comparison != None =>
        Error(index)
      | Some(Eq) => splitIndex(~comparison=Some((Cmp_Eq, index)), index + 1)
      | Some(Gt) => splitIndex(~comparison=Some((Cmp_Gt, index)), index + 1)
      | Some(Gte) => splitIndex(~comparison=Some((Cmp_Gte, index)), index + 1)
      | Some(Lt) => splitIndex(~comparison=Some((Cmp_Lt, index)), index + 1)
      | Some(Lte) => splitIndex(~comparison=Some((Cmp_Lte, index)), index + 1)
      | Some(_) => splitIndex(~comparison, index + 1)
      | None => Ok(comparison)
      }
    splitIndex(~comparison=None, 0)
  }
)

let parseAsMetalShaderComponents = (v: array<AST.t>) => {
  switch splitIndex(v) {
  | Ok(Some((cmp, splitIndex))) =>
    let left = Belt.Array.slice(v, ~offset=0, ~len=splitIndex)->Value.parse
    let right = Belt.Array.sliceToEnd(v, splitIndex + 1)->Value.parse
    switch (left, right) {
    | (Ok(left), Ok(right)) => Ok((cmp, left, right))
    | (Error(i), _)
    | (_, Error(i)) =>
      Error(i)
    }
  | Ok(None) =>
    switch Value.parse(v) {
    | Ok(v) => Ok((Cmp_Eq, Y, v))
    | Error(i) => Error(i)
    }
  | Error(i) => Error(i)
  }
}

let parseAsMetalShader = (~context, v: array<AST.t>) => {
  let context = decodeContext(context)

  switch parseAsMetalShaderComponents(v) {
  | Ok((cmp, left, right)) =>
    switch (astToMetalShader(~context, left), astToMetalShader(~context, right)) {
    | (Some(left), Some(right)) =>
      let eq = switch cmp {
      | Cmp_Eq | Cmp_Gt | Cmp_Gte => `(${left}) - (${right})`
      | Cmp_Lt | Cmp_Lte => `(${right}) - (${left})`
      }
      Ok((eq, cmp))
    | _ => Error(None)
    }
  | Error(i) => Error(Some(i))
  }
}
