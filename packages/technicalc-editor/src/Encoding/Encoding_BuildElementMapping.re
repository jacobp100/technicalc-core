let%private makeSureThisIsTheLastIndex = 79;
let%private toInt = (element: AST.t) =>
  switch (element) {
  /* Most common (make var-int encoding more efficient) */
  | N0_S => 0
  | N1_S => 1
  | N2_S => 2
  | N3_S => 3
  | N4_S => 4
  | N5_S => 5
  | N6_S => 6
  | N7_S => 7
  | N8_S => 8
  | N9_S => 9
  | DecimalSeparator => 10
  | Add => 11
  | Mul => 12
  | Sub => 13
  | Div => 14
  | OpenBracket => 15
  | CloseBracketS => 16
  | Arg => 17
  | Magnitude1 => 18
  | Frac2S => 19
  | Sqrt1S => 20
  | Log => 21
  | SinS => 22
  | CosS => 23
  | TanS => 24
  | Asin => 25
  | Acos => 26
  | Atan => 27
  | ConstES => 28
  | ConstPiS => 29
  | Degree => 30
  | Percent => 31
  /* Any order */
  | Abs1S => 32
  | Acosh => 33
  | ArcMinute => 34
  | ArcSecond => 35
  | Asinh => 36
  | Atanh => 37
  | Bin => 38
  | Ceil1S => 39
  | Conj => 40
  | CoshS => 41
  | Differential2 => 42
  | Dot => 43
  | Factorial => 44
  | Floor1S => 45
  | Gamma => 46
  | Hex => 47
  | Im => 48
  | ImaginaryUnitS => 49
  | Integral3 => 50
  | Matrix4S => 51
  | NA_S => 52
  | NB_S => 53
  | NC_S => 54
  | NCR2 => 55
  | ND_S => 56
  | NE_S => 57
  | NF_S => 58
  | NLog1 => 59
  | NPR2 => 60
  | NRoot2S => 61
  | Oct => 62
  | Product2 => 63
  | RandInt2S => 64
  | RandS => 65
  | Re => 66
  | Round1S => 67
  | SinhS => 68
  | Sum2 => 69
  | Superscript1 => 70
  | TanhS => 71
  | Vector2S => 72
  | Vector3S => 73
  | Matrix9S => 74
  /* 2nd set Additions */
  | Min2S => 75
  | Max2S => 76
  | Gcd2S => 77
  | Lcm2S => 78
  /* 3rd set Additions */
  | Gradian => makeSureThisIsTheLastIndex
  /* Custom handling */
  | UnitConversion(_)
  | CustomAtomS(_)
  | LabelS(_)
  | VariableS(_) => assert(false)
  };

let mapping = Belt.Array.make(makeSureThisIsTheLastIndex + 1, 0);
let reverseMapping = Belt.Array.make(makeSureThisIsTheLastIndex + 1, AST.Arg);
for (i in 0 to makeSureThisIsTheLastIndex) {
  let element: AST.t = Obj.magic(i);
  let index = toInt(element);
  assert(Belt.Array.set(mapping, i, index));
  assert(Belt.Array.set(reverseMapping, index, element));
};
