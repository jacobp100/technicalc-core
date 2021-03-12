open AST_Types

type superscript<'a> = {
  superscriptBody: 'a,
  index: int,
}

type base =
  | Bin
  | Oct
  | Hex
type func =
  | Acos
  | Acosh
  | Asin
  | Asinh
  | Atan
  | Atanh
  | Deg
  | Gamma
  | Grad
  | Im
  | Log
  | Rad
  | Re
  /* Superscripts supported */
  | Cos
  | Cosec
  | Cosh
  | Cot
  | Sec
  | Sin
  | Sinh
  | Tan
  | Tanh
type angle =
  | Radian
  | Degree
  | ArcMinute
  | ArcSecond
  | Gradian
type operator =
  | Add
  | Sub
  | Mul
  | Div
  | Dot

type foldState<'a> =
  | Abs({arg: 'a, superscript: option<superscript<'a>>})
  | Angle(angle)
  | Base(base)
  | CaptureGroupPlaceholder({placeholderMml: string, superscript: option<superscript<'a>>})
  | Ceil({arg: 'a, superscript: option<superscript<'a>>})
  | CloseBracket(option<superscript<'a>>)
  | ConstE(option<superscript<'a>>)
  | ConstPi(option<superscript<'a>>)
  | Conj
  | CustomAtom({mml: string, value: string, superscript: option<superscript<'a>>})
  | DecimalSeparator
  | Differential({at: 'a, body: 'a})
  | Digit({nucleus: string, superscript: option<superscript<'a>>})
  | Factorial
  | Floor({arg: 'a, superscript: option<superscript<'a>>})
  | Frac({num: 'a, den: 'a, superscript: option<superscript<'a>>})
  | Function({func: func, resultSuperscript: option<superscript<'a>>})
  | Gcd({a: 'a, b: 'a, superscript: option<superscript<'a>>})
  | ImaginaryUnit(option<superscript<'a>>)
  | Integral({from: 'a, to_: 'a, body: 'a})
  | IteratorX(option<superscript<'a>>)
  | Lcm({a: 'a, b: 'a, superscript: option<superscript<'a>>})
  | Magnitude({value: 'a})
  | MFrac({integer: 'a, num: 'a, den: 'a, superscript: option<superscript<'a>>})
  | Min({a: 'a, b: 'a, superscript: option<superscript<'a>>})
  | Max({a: 'a, b: 'a, superscript: option<superscript<'a>>})
  | NCR({n: 'a, r: 'a})
  | NLog({base: 'a})
  | NPR({n: 'a, r: 'a})
  | NRoot({degree: 'a, radicand: 'a, superscript: option<superscript<'a>>})
  | OpenBracket
  | Operator(operator)
  | Percent
  | Placeholder(option<superscript<'a>>)
  | Product({from: 'a, to_: 'a})
  | Rand(option<superscript<'a>>)
  | RandInt({a: 'a, b: 'a, superscript: option<superscript<'a>>})
  | Round({arg: 'a, superscript: option<superscript<'a>>})
  | Sqrt({radicand: 'a, superscript: option<superscript<'a>>})
  | Sum({from: 'a, to_: 'a})
  | Vector({elements: array<'a>, superscript: option<superscript<'a>>})
  | Table({
      elements: array<'a>,
      numRows: int,
      numColumns: int,
      superscript: option<superscript<'a>>,
    })
  | UnitConversion({
      fromUnits: array<TechniCalcCalculator.Unit_Types.unitPart>,
      toUnits: array<TechniCalcCalculator.Unit_Types.unitPart>,
    })
  | Variable({id: string, name: string, superscript: option<superscript<'a>>})

type range = (int, int)

let superscriptBody = superscript => superscript.superscriptBody

%%private(
  let digitNucleusExn = digit =>
    switch digit {
    | AST_Types.N0_S => "0"
    | N1_S => "1"
    | N2_S => "2"
    | N3_S => "3"
    | N4_S => "4"
    | N5_S => "5"
    | N6_S => "6"
    | N7_S => "7"
    | N8_S => "8"
    | N9_S => "9"
    | NA_S => "A"
    | NB_S => "B"
    | NC_S => "C"
    | ND_S => "D"
    | NE_S => "E"
    | NF_S => "F"
    | _ => assert false
    }
)

type readResult<'a> =
  | Node(foldState<'a>, int, int)
  | Empty

let reduceMap = (
  input: array<t>,
  ~reduce: ('accum, foldState<'a>, range) => 'accum,
  ~map: ('accum, range) => 'value,
  ~initial: 'accum,
): 'value => {
  let rec readNodeExn = (i): readResult<'a> =>
    switch Belt.Array.getExn(input, i) {
    | CaptureGroupStart({placeholderMml}) =>
      switch Belt.Array.get(input, i + 1) {
      | Some(CaptureGroupEndS) =>
        let i' = i + 2
        let (superscript, i') = readSuperscript(i')
        Node(
          CaptureGroupPlaceholder({placeholderMml: placeholderMml, superscript: superscript}),
          i,
          i',
        )
      | _ => Empty
      }
    | CaptureGroupEndS => Empty
    | Conj => Node(Conj, i, i + 1)
    | DecimalSeparator => Node(DecimalSeparator, i, i + 1)
    | Factorial => Node(Factorial, i, i + 1)
    | OpenBracket => Node(OpenBracket, i, i + 1)
    | Percent => Node(Percent, i, i + 1)
    | Bin => Node(Base(Bin), i, i + 1)
    | Oct => Node(Base(Oct), i, i + 1)
    | Hex => Node(Base(Hex), i, i + 1)
    | Add => Node(Operator(Add), i, i + 1)
    | Sub => Node(Operator(Sub), i, i + 1)
    | Mul => Node(Operator(Mul), i, i + 1)
    | Div => Node(Operator(Div), i, i + 1)
    | Dot => Node(Operator(Dot), i, i + 1)
    | Acos => func(i, Acos)
    | Acosh => func(i, Acosh)
    | Asin => func(i, Asin)
    | Asinh => func(i, Asinh)
    | Atan => func(i, Atan)
    | Atanh => func(i, Atanh)
    | DegreeFunction => func(i, Deg)
    | Gamma => func(i, Gamma)
    | GradianFunction => func(i, Grad)
    | Im => func(i, Im)
    | Log => func(i, Log)
    | RadianFunction => func(i, Rad)
    | Re => func(i, Re)
    | CoshS => funcS(i, Cosh)
    | CosS => funcS(i, Cos)
    | SinhS => funcS(i, Sinh)
    | SinS => funcS(i, Sin)
    | TanhS => funcS(i, Tanh)
    | TanS => funcS(i, Tan)
    | CosecS => funcS(i, Cosec)
    | SecS => funcS(i, Sec)
    | CotS => funcS(i, Cot)
    | RadianUnit => Node(Angle(Radian), i, i + 1)
    | DegreeUnit => Node(Angle(Degree), i, i + 1)
    | ArcMinuteUnit => Node(Angle(ArcMinute), i, i + 1)
    | ArcSecondUnit => Node(Angle(ArcSecond), i, i + 1)
    | GradianUnit => Node(Angle(Gradian), i, i + 1)
    | UnitConversion({fromUnits, toUnits}) =>
      Node(UnitConversion({fromUnits: fromUnits, toUnits: toUnits}), i, i + 1)
    | CloseBracketS =>
      let i' = i + 1
      let (superscript, i') = readSuperscript(i')
      Node(CloseBracket(superscript), i, i')
    | ConstPiS =>
      let i' = i + 1
      let (superscript, i') = readSuperscript(i')
      Node(ConstPi(superscript), i, i')
    | ConstES =>
      let i' = i + 1
      let (superscript, i') = readSuperscript(i')
      Node(ConstE(superscript), i, i')
    | CustomAtomS({mml, value}) =>
      let i' = i + 1
      let (superscript, i') = readSuperscript(i')
      Node(CustomAtom({mml: mml, value: value, superscript: superscript}), i, i')
    | (N0_S | N1_S | N2_S | N3_S | N4_S | N5_S | N6_S | N7_S | N8_S | N9_S) as digit
    | (NA_S | NB_S | NC_S | ND_S | NE_S | NF_S) as digit =>
      let nucleus = digitNucleusExn(digit)
      let i' = i + 1
      let (superscript, i') = readSuperscript(i')
      Node(Digit({nucleus: nucleus, superscript: superscript}), i, i')
    | ImaginaryUnitS =>
      let i' = i + 1
      let (superscript, i') = readSuperscript(i')
      Node(ImaginaryUnit(superscript), i, i')
    | IteratorXS =>
      let i' = i + 1
      let (superscript, i') = readSuperscript(i')
      Node(IteratorX(superscript), i, i')
    | RandS =>
      let i' = i + 1
      let (superscript, i') = readSuperscript(i')
      Node(Rand(superscript), i, i')
    | VariableS({id, name}) =>
      let i' = i + 1
      let (superscript, i') = readSuperscript(i')
      Node(Variable({id: id, name: name, superscript: superscript}), i, i')
    | Magnitude1 =>
      let (value, i') = readArg(i + 1)
      Node(Magnitude({value: value}), i, i')
    | Superscript1 =>
      let (superscriptBody, i') = readArg(i + 1)
      let superscript = Some({superscriptBody: superscriptBody, index: i + 1})
      Node(Placeholder(superscript), i, i')
    | NLog1 =>
      let (base, i') = readArg(i + 1)
      Node(NLog({base: base}), i, i')
    | Abs1S =>
      let (arg, i') = readArg(i + 1)
      let (superscript, i') = readSuperscript(i')
      Node(Abs({arg: arg, superscript: superscript}), i, i')
    | Ceil1S =>
      let (arg, i') = readArg(i + 1)
      let (superscript, i') = readSuperscript(i')
      Node(Ceil({arg: arg, superscript: superscript}), i, i')
    | Floor1S =>
      let (arg, i') = readArg(i + 1)
      let (superscript, i') = readSuperscript(i')
      Node(Floor({arg: arg, superscript: superscript}), i, i')
    | Round1S =>
      let (arg, i') = readArg(i + 1)
      let (superscript, i') = readSuperscript(i')
      Node(Round({arg: arg, superscript: superscript}), i, i')
    | Sqrt1S =>
      let (radicand, i') = readArg(i + 1)
      let (superscript, i') = readSuperscript(i')
      Node(Sqrt({radicand: radicand, superscript: superscript}), i, i')
    | Differential2 =>
      let (body, i') = readArg(i + 1)
      let (at, i') = readArg(i')
      Node(Differential({at: at, body: body}), i, i')
    | NCR2 =>
      let (n, i') = readArg(i + 1)
      let (r, i') = readArg(i')
      Node(NCR({n: n, r: r}), i, i')
    | NPR2 =>
      let (n, i') = readArg(i + 1)
      let (r, i') = readArg(i')
      Node(NPR({n: n, r: r}), i, i')
    | Product2 =>
      let (from, i') = readArg(i + 1)
      let (to_, i') = readArg(i')
      Node(Product({from: from, to_: to_}), i, i')
    | Sum2 =>
      let (from, i') = readArg(i + 1)
      let (to_, i') = readArg(i')
      Node(Sum({from: from, to_: to_}), i, i')
    | Frac2S =>
      let (num, i') = readArg(i + 1)
      let (den, i') = readArg(i')
      let (superscript, i') = readSuperscript(i')
      Node(Frac({num: num, den: den, superscript: superscript}), i, i')
    | MFrac3S =>
      let (integer, i') = readArg(i + 1)
      let (num, i') = readArg(i')
      let (den, i') = readArg(i')
      let (superscript, i') = readSuperscript(i')
      Node(MFrac({integer: integer, num: num, den: den, superscript: superscript}), i, i')
    | Min2S =>
      let (a, i') = readArg(i + 1)
      let (b, i') = readArg(i')
      let (superscript, i') = readSuperscript(i')
      Node(Min({a: a, b: b, superscript: superscript}), i, i')
    | Max2S =>
      let (a, i') = readArg(i + 1)
      let (b, i') = readArg(i')
      let (superscript, i') = readSuperscript(i')
      Node(Max({a: a, b: b, superscript: superscript}), i, i')
    | Gcd2S =>
      let (a, i') = readArg(i + 1)
      let (b, i') = readArg(i')
      let (superscript, i') = readSuperscript(i')
      Node(Gcd({a: a, b: b, superscript: superscript}), i, i')
    | Lcm2S =>
      let (a, i') = readArg(i + 1)
      let (b, i') = readArg(i')
      let (superscript, i') = readSuperscript(i')
      Node(Lcm({a: a, b: b, superscript: superscript}), i, i')
    | NRoot2S =>
      let (degree, i') = readArg(i + 1)
      let (radicand, i') = readArg(i')
      let (superscript, i') = readSuperscript(i')
      Node(NRoot({degree: degree, radicand: radicand, superscript: superscript}), i, i')
    | RandInt2S =>
      let (a, i') = readArg(i + 1)
      let (b, i') = readArg(i')
      let (superscript, i') = readSuperscript(i')
      Node(RandInt({a: a, b: b, superscript: superscript}), i, i')
    | Integral3 =>
      let (from, i') = readArg(i + 1)
      let (to_, i') = readArg(i')
      let (body, i') = readArg(i')
      Node(Integral({from: from, to_: to_, body: body}), i, i')
    | Vector2S => vectorS(i, ~numElements=2)
    | Vector3S => vectorS(i, ~numElements=3)
    | Matrix4S => tableS(i, ~numRows=2, ~numColumns=2)
    | Matrix9S => tableS(i, ~numRows=3, ~numColumns=3)
    | Arg => assert false
    }
  and readArg = (~accum=initial, ~start=?, i) => {
    let start = Belt.Option.getWithDefault(start, i)
    switch Belt.Array.get(input, i) {
    | None => assert false
    | Some(Arg) =>
      let i' = i
      (map(accum, (start, i')), i' + 1)
    | Some(_) =>
      switch readNodeExn(i) {
      | Node(node, i, i') => readArg(~accum=reduce(accum, node, (i, i')), i')
      | Empty => readArg(~accum, i + 1)
      }
    }
  }
  and readSuperscript = argEndIndex => {
    let rec iter = i =>
      switch Belt.Array.get(input, i) {
      | Some(Superscript1) =>
        let (superscriptBody, i') = readArg(i + 1)
        (Some({superscriptBody: superscriptBody, index: argEndIndex}), i')
      | Some(CaptureGroupEndS) =>
        // If a superscript occurs immediately after a capture group, apply the
        // superscript to the last element within the capture group
        iter(i + 1)
      | _ => (None, argEndIndex)
      }

    iter(argEndIndex)
  }
  and func = (i, func) => {
    let i' = i + 1
    Node(Function({func: func, resultSuperscript: None}), i, i')
  }
  and funcS = (i, func) => {
    let i' = i + 1
    let (resultSuperscript, i') = readSuperscript(i')
    Node(Function({func: func, resultSuperscript: resultSuperscript}), i, i')
  }
  and vectorS = (i, ~numElements) => {
    let i' = i + 1
    let (i', elements) = ArrayUtil.foldMakeU(numElements, i', (. s, _) => {
      let (element, index) = readArg(s)
      (index, element)
    })
    let (superscript, i') = readSuperscript(i')
    Node(Vector({elements: elements, superscript: superscript}), i, i')
  }
  and tableS = (i, ~numRows, ~numColumns) => {
    let i' = i + 1
    let (i', elements) = ArrayUtil.foldMakeU(numRows * numColumns, i', (. s, _) => {
      let (element, index) = readArg(s)
      (index, element)
    })
    let (superscript, i') = readSuperscript(i')
    Node(
      Table({
        elements: elements,
        numRows: numRows,
        numColumns: numColumns,
        superscript: superscript,
      }),
      i,
      i',
    )
  }

  let rec readUntilEnd = (~accum=initial, i) =>
    switch Belt.Array.get(input, i) {
    | None => map(accum, (0, i))
    | Some(Arg) => assert false
    | Some(_) =>
      switch readNodeExn(i) {
      | Node(node, i, i') => readUntilEnd(~accum=reduce(accum, node, (i, i')), i')
      | Empty => readUntilEnd(~accum, i + 1)
      }
    }

  readUntilEnd(0)
}
