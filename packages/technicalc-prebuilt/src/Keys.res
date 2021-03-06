open TechniCalcEditor.AST_Types

type key =
  | One(t)
  | Many(array<t>)

let keys = {
  "0": One(N0_S),
  "1": One(N1_S),
  "2": One(N2_S),
  "3": One(N3_S),
  "4": One(N4_S),
  "5": One(N5_S),
  "6": One(N6_S),
  "7": One(N7_S),
  "8": One(N8_S),
  "9": One(N9_S),
  "A": One(NA_S),
  "B": One(NB_S),
  "C": One(NC_S),
  "D": One(ND_S),
  "E": One(NE_S),
  "F": One(NF_S),
  ".": One(DecimalSeparator),
  "+": One(Add),
  "-": One(Sub),
  "*": One(Mul),
  "/": One(Div),
  "%": One(Percent),
  "_": One(Frac2S),
  "^": One(Superscript1),
  "!": One(Factorial),
  "(": One(OpenBracket),
  ")": One(CloseBracketS),
  "base2": One(Bin),
  "base8": One(Oct),
  "base16": One(Hex),
  "mfrac": One(MFrac3S),
  "sqrt": One(Sqrt1S),
  "cuberoot": Many([NRoot2S, N3_S, Arg, Arg]),
  "nroot": One(NRoot2S),
  "abs": One(Abs1S),
  "floor": One(Floor1S),
  "ceil": One(Ceil1S),
  "round": One(Round1S),
  "log": One(Log),
  "log2": Many([NLog1, N2_S, Arg]),
  "log10": Many([NLog1, N1_S, N0_S, Arg]),
  "logn": One(NLog1),
  "sin": One(SinS),
  "asin": One(Asin),
  "cosec": One(CosecS),
  "sinh": One(SinhS),
  "asinh": One(Asinh),
  "cos": One(CosS),
  "acos": One(Acos),
  "sec": One(SecS),
  "cosh": One(CoshS),
  "acosh": One(Acosh),
  "tan": One(TanS),
  "atan": One(Atan),
  "cot": One(CotS),
  "tanh": One(TanhS),
  "atanh": One(Atanh),
  "i": One(ImaginaryUnitS),
  "x": One(IteratorXS),
  "pi": One(ConstPiS),
  "e": One(ConstES),
  "ans": One(VariableS({id: "Ans", name: "Ans"})),
  "re": One(Re),
  "im": One(Im),
  "conj": One(Conj),
  "gamma": One(Gamma),
  "rand": One(RandS),
  "randint": One(RandInt2S),
  "npr": One(NPR2),
  "ncr": One(NCR2),
  "min": One(Min2S),
  "max": One(Max2S),
  "gcd": One(Gcd2S),
  "lcm": One(Lcm2S),
  "differential": One(Differential2),
  "integral": One(Integral3),
  "sum": One(Sum2),
  "product": One(Product2),
  "dot": One(Dot),
  "magnitude": One(Magnitude1),
  "degrees": One(DegreeUnit),
  "arcminutes": One(ArcMinuteUnit),
  "arcseconds": One(ArcSecondUnit),
  "radians": One(RadianUnit),
  "gradians": One(GradianUnit),
  "deg": One(DegreeFunction),
  "rad": One(RadianFunction),
  "grad": One(GradianFunction),
  "vec2": One(Vector2S),
  "vec3": One(Vector3S),
  "mat2": One(Matrix4S),
  "mat3": One(Matrix9S),
}
