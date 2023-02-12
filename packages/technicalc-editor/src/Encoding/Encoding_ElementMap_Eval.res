%%private(
  let toUint = (element: AST.t) =>
    switch element {
    /* Common elements */
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
    | Superscript1 => 21
    | Log => 22
    | SinS => 23
    | CosS => 24
    | TanS => 25
    | Asin => 26
    | Acos => 27
    | Atan => 28
    | ConstES => 29
    | ConstPiS => 30
    | DegreeUnit => 31
    /* Original elements */
    | Abs1S => 32
    | Acosh => 33
    | ArcMinuteUnit => 34
    | ArcSecondUnit => 35
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
    | GCD2S => 47
    | GradianUnit => 48
    | Hex => 49
    | Im => 50
    | ImaginaryUnitS => 51
    | Integral3 => 52
    | LCM2S => 53
    // REMOVED | Matrix4S => 54
    // REMOVED | Matrix9S => 55
    | Max2S => 56
    | Min2S => 57
    | NA_S => 58
    | NB_S => 59
    | NC_S => 60
    | NCR2 => 61
    | ND_S => 62
    | NE_S => 63
    | NF_S => 64
    | NLog1 => 65
    | NPR2 => 66
    | NRoot2S => 67
    | Oct => 68
    | Percent => 69
    | Product2 => 70
    | RandInt2S => 71
    | RandS => 72
    | Re => 73
    | Round1S => 74
    | SinhS => 75
    | Sum2 => 76
    | TanhS => 77
    // REMOVED | Vector2S => 78
    // REMOVED | Vector3S => 79
    /* 2nd set elements */
    | CosecS => 80
    | SecS => 81
    | CotS => 82
    | DegFunction => 83
    | GradFunction => 84
    /* 3rd set elements */
    // REMOVED | MFrac3S => 85
    /* 4th set elements */
    | CaptureGroupEndS => 86
    | RadFunction => 87
    | RadianUnit => 88
    | IterationXS => 89
    /* 5th set elements */
    // REMOVED | Rem => 90
    /* 6th set elements */
    | Rref => 91
    | Trace => 92
    | Transpose => 93
    /* 7th set elements */
    | XUnitS => 94
    | YUnitS => 95
    | ZUnitS => 96
    /* Custom handling */
    | CaptureGroupStart(_)
    | ConstantS(_)
    | UnitS(_)
    | VariableS(_)
    | TableNS(_) =>
      assert false
    }
)

%%private(let numIntElements = 90)
%%private(let maxUintValue = 96)
let mapping = Belt.Array.make(numIntElements + 1, 0)
let reverseMapping = Belt.Array.make(maxUintValue + 1, AST.Arg)
for i in 0 to numIntElements {
  let element: AST.t = Obj.magic(i)
  let index = toUint(element)
  assert Belt.Array.set(mapping, i, index)
  assert Belt.Array.set(reverseMapping, index, element)
}
