open AST_Types
open AST_Categorization

type superscript<'a> = {
  superscriptBody: 'a,
  index: int,
}

type foldState<'a> =
  | Fold_Abs({arg: 'a, superscript: option<superscript<'a>>})
  | Fold_Add
  | Fold_Angle(angle)
  | Fold_Base(base)
  | Fold_Ceil({arg: 'a, superscript: option<superscript<'a>>})
  | Fold_CloseBracket(option<superscript<'a>>)
  | Fold_Conj
  | Fold_ConstE(option<superscript<'a>>)
  | Fold_ConstPi(option<superscript<'a>>)
  | Fold_Constant({symbol: Symbol.t, value: string, superscript: option<superscript<'a>>})
  | Fold_DecimalSeparator
  | Fold_Differential({at: 'a, body: 'a})
  | Fold_Digit({nucleus: string, superscript: option<superscript<'a>>})
  | Fold_Div
  | Fold_Dot
  | Fold_Factorial
  | Fold_Floor({arg: 'a, superscript: option<superscript<'a>>})
  | Fold_Frac({num: 'a, den: 'a, superscript: option<superscript<'a>>})
  | Fold_Function({fn: fn<'a>, resultSuperscript: option<superscript<'a>>})
  | Fold_Gcd({a: 'a, b: 'a, superscript: option<superscript<'a>>})
  | Fold_ImaginaryUnit(option<superscript<'a>>)
  | Fold_Integral({from: 'a, to: 'a, body: 'a})
  | Fold_Lcm({a: 'a, b: 'a, superscript: option<superscript<'a>>})
  | Fold_Magnitude({value: 'a})
  | Fold_Max({a: 'a, b: 'a, superscript: option<superscript<'a>>})
  | Fold_Min({a: 'a, b: 'a, superscript: option<superscript<'a>>})
  | Fold_Mul
  | Fold_NCR({n: 'a, r: 'a})
  | Fold_NPR({n: 'a, r: 'a})
  | Fold_NRoot({degree: 'a, radicand: 'a, superscript: option<superscript<'a>>})
  | Fold_OpenBracket
  | Fold_Percent
  | Fold_Placeholder({
      implicit: bool,
      placeholder: option<Symbol.t>,
      superscript: option<superscript<'a>>,
      captureGroupIndex: option<int>,
    })
  | Fold_Rand(option<superscript<'a>>)
  | Fold_RandInt({a: 'a, b: 'a, superscript: option<superscript<'a>>})
  | Fold_Round({arg: 'a, superscript: option<superscript<'a>>})
  | Fold_Sqrt({radicand: 'a, superscript: option<superscript<'a>>})
  | Fold_Sub
  | Fold_Table({
      elements: array<'a>,
      numRows: int,
      numColumns: int,
      superscript: option<superscript<'a>>,
    })
  | Fold_Transpose
  | Fold_Unit({
      prefix: TechniCalcCalculator.Units.prefix,
      name: TechniCalcCalculator.Units.name,
      superscript: option<superscript<'a>>,
    })
  | Fold_Variable({id: string, symbol: Symbol.t, superscript: option<superscript<'a>>})
  | Fold_X(option<superscript<'a>>)
  | Fold_XUnit(option<superscript<'a>>)
  | Fold_Y(option<superscript<'a>>)
  | Fold_YUnit(option<superscript<'a>>)
  | Fold_Z(option<superscript<'a>>)
  | Fold_ZUnit(option<superscript<'a>>)

type range = (int, int)

let superscriptBodyU = (. superscript) => superscript.superscriptBody

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

%%private(
  let defaultEmptyArg = Fold_Placeholder({
    implicit: true,
    placeholder: None,
    superscript: None,
    captureGroupIndex: None,
  })
)

type readResult<'a> =
  | Node(foldState<'a>, int, int)
  | Empty

let reduceMapU = (
  input: array<t>,
  ~reduce: (. 'accum, foldState<'a>, range) => 'accum,
  ~map: (. 'accum, bool) => 'value,
  ~initial: 'accum,
): 'value => {
  let rec readNodeExn = (i): readResult<'a> =>
    switch Belt.Array.getExn(input, i) {
    | CaptureGroupStart({placeholder}) =>
      switch Belt.Array.get(input, i + 1) {
      | Some(CaptureGroupEndS) =>
        let implicit = false
        let captureGroupIndex = Some(i + 1)
        let i' = i + 2
        let (superscript, i') = readSuperscript(i')
        Node(Fold_Placeholder({implicit, placeholder, superscript, captureGroupIndex}), i, i')
      | _ => Empty
      }
    | CaptureGroupEndS => Empty
    | Conj => Node(Fold_Conj, i, i + 1)
    | DecimalSeparator => Node(Fold_DecimalSeparator, i, i + 1)
    | Factorial => Node(Fold_Factorial, i, i + 1)
    | OpenBracket => Node(Fold_OpenBracket, i, i + 1)
    | Percent => Node(Fold_Percent, i, i + 1)
    | Transpose => Node(Fold_Transpose, i, i + 1)
    | Bin => Node(Fold_Base(Base_Bin), i, i + 1)
    | Oct => Node(Fold_Base(Base_Oct), i, i + 1)
    | Hex => Node(Fold_Base(Base_Hex), i, i + 1)
    | Add => Node(Fold_Add, i, i + 1)
    | Sub => Node(Fold_Sub, i, i + 1)
    | Mul => Node(Fold_Mul, i, i + 1)
    | Div => Node(Fold_Div, i, i + 1)
    | Dot => Node(Fold_Dot, i, i + 1)
    | Acos => fn(i, Fn_Acos)
    | Acosh => fn(i, Fn_Acosh)
    | Asin => fn(i, Fn_Asin)
    | Asinh => fn(i, Fn_Asinh)
    | Atan => fn(i, Fn_Atan)
    | Atanh => fn(i, Fn_Atanh)
    | Gamma => fn(i, Fn_Gamma)
    | Im => fn(i, Fn_Im)
    | Log => fn(i, Fn_Log)
    | Re => fn(i, Fn_Re)
    | Rref => fn(i, Fn_Rref)
    | Trace => fn(i, Fn_Trace)
    | CoshS => fnS(i, Fn_Cosh)
    | CosS => fnS(i, Fn_Cos)
    | SinhS => fnS(i, Fn_Sinh)
    | SinS => fnS(i, Fn_Sin)
    | TanhS => fnS(i, Fn_Tanh)
    | TanS => fnS(i, Fn_Tan)
    | CosecS => fnS(i, Fn_Cosec)
    | SecS => fnS(i, Fn_Sec)
    | CotS => fnS(i, Fn_Cot)
    | NLog1 =>
      let (base, i') = readArg(i + 1)
      Node(Fold_Function({fn: Fn_NLog({base: base}), resultSuperscript: None}), i, i')
    | Product2 =>
      let (from, i') = readArg(i + 1)
      let (to, i') = readArg(i')
      Node(Fold_Function({fn: Fn_Product({from, to}), resultSuperscript: None}), i, i')
    | Sum2 =>
      let (from, i') = readArg(i + 1)
      let (to, i') = readArg(i')
      Node(Fold_Function({fn: Fn_Sum({from, to}), resultSuperscript: None}), i, i')
    | RadianUnit => Node(Fold_Angle(Angle_Radian), i, i + 1)
    | DegreeUnit => Node(Fold_Angle(Angle_Degree), i, i + 1)
    | ArcMinuteUnit => Node(Fold_Angle(Angle_ArcMinute), i, i + 1)
    | ArcSecondUnit => Node(Fold_Angle(Angle_ArcSecond), i, i + 1)
    | GradianUnit => Node(Fold_Angle(Angle_Gradian), i, i + 1)
    | CloseBracketS =>
      let i' = i + 1
      let (superscript, i') = readSuperscript(i')
      Node(Fold_CloseBracket(superscript), i, i')
    | ConstPiS =>
      let i' = i + 1
      let (superscript, i') = readSuperscript(i')
      Node(Fold_ConstPi(superscript), i, i')
    | ConstES =>
      let i' = i + 1
      let (superscript, i') = readSuperscript(i')
      Node(Fold_ConstE(superscript), i, i')
    | ConstantS({symbol, value}) =>
      let i' = i + 1
      let (superscript, i') = readSuperscript(i')
      Node(Fold_Constant({symbol, value, superscript}), i, i')
    | (N0_S | N1_S | N2_S | N3_S | N4_S | N5_S | N6_S | N7_S | N8_S | N9_S) as digit
    | (NA_S | NB_S | NC_S | ND_S | NE_S | NF_S) as digit =>
      let nucleus = digitNucleusExn(digit)
      let i' = i + 1
      let (superscript, i') = readSuperscript(i')
      Node(Fold_Digit({nucleus, superscript}), i, i')
    | ImaginaryUnitS =>
      let i' = i + 1
      let (superscript, i') = readSuperscript(i')
      Node(Fold_ImaginaryUnit(superscript), i, i')
    | XS =>
      let i' = i + 1
      let (superscript, i') = readSuperscript(i')
      Node(Fold_X(superscript), i, i')
    | YS =>
      let i' = i + 1
      let (superscript, i') = readSuperscript(i')
      Node(Fold_Y(superscript), i, i')
    | ZS =>
      let i' = i + 1
      let (superscript, i') = readSuperscript(i')
      Node(Fold_Z(superscript), i, i')
    | RandS =>
      let i' = i + 1
      let (superscript, i') = readSuperscript(i')
      Node(Fold_Rand(superscript), i, i')
    | VariableS({id, symbol}) =>
      let i' = i + 1
      let (superscript, i') = readSuperscript(i')
      Node(Fold_Variable({id, symbol, superscript}), i, i')
    | XUnitS =>
      let i' = i + 1
      let (superscript, i') = readSuperscript(i')
      Node(Fold_XUnit(superscript), i, i')
    | YUnitS =>
      let i' = i + 1
      let (superscript, i') = readSuperscript(i')
      Node(Fold_YUnit(superscript), i, i')
    | ZUnitS =>
      let i' = i + 1
      let (superscript, i') = readSuperscript(i')
      Node(Fold_ZUnit(superscript), i, i')
    | Magnitude1 =>
      let (value, i') = readArg(i + 1)
      Node(Fold_Magnitude({value: value}), i, i')
    | Superscript1 =>
      let implicit = false
      let placeholder = None
      let captureGroupIndex = None
      let (superscriptBody, i') = readArg(i + 1)
      let superscript = Some({superscriptBody, index: i + 1})
      Node(Fold_Placeholder({implicit, placeholder, superscript, captureGroupIndex}), i, i')
    | Abs1S =>
      let (arg, i') = readArg(i + 1)
      let (superscript, i') = readSuperscript(i')
      Node(Fold_Abs({arg, superscript}), i, i')
    | Ceil1S =>
      let (arg, i') = readArg(i + 1)
      let (superscript, i') = readSuperscript(i')
      Node(Fold_Ceil({arg, superscript}), i, i')
    | Floor1S =>
      let (arg, i') = readArg(i + 1)
      let (superscript, i') = readSuperscript(i')
      Node(Fold_Floor({arg, superscript}), i, i')
    | Round1S =>
      let (arg, i') = readArg(i + 1)
      let (superscript, i') = readSuperscript(i')
      Node(Fold_Round({arg, superscript}), i, i')
    | Sqrt1S =>
      let (radicand, i') = readArg(i + 1)
      let (superscript, i') = readSuperscript(i')
      Node(Fold_Sqrt({radicand, superscript}), i, i')
    | Differential2 =>
      let (body, i') = readArg(i + 1)
      let (at, i') = readArg(i')
      Node(Fold_Differential({at, body}), i, i')
    | NCR2 =>
      let (n, i') = readArg(i + 1)
      let (r, i') = readArg(i')
      Node(Fold_NCR({n, r}), i, i')
    | NPR2 =>
      let (n, i') = readArg(i + 1)
      let (r, i') = readArg(i')
      Node(Fold_NPR({n, r}), i, i')
    | Frac2S =>
      let (num, i') = readArg(i + 1)
      let (den, i') = readArg(i')
      let (superscript, i') = readSuperscript(i')
      Node(Fold_Frac({num, den, superscript}), i, i')
    | Min2S =>
      let (a, i') = readArg(i + 1)
      let (b, i') = readArg(i')
      let (superscript, i') = readSuperscript(i')
      Node(Fold_Min({a, b, superscript}), i, i')
    | Max2S =>
      let (a, i') = readArg(i + 1)
      let (b, i') = readArg(i')
      let (superscript, i') = readSuperscript(i')
      Node(Fold_Max({a, b, superscript}), i, i')
    | GCD2S =>
      let (a, i') = readArg(i + 1)
      let (b, i') = readArg(i')
      let (superscript, i') = readSuperscript(i')
      Node(Fold_Gcd({a, b, superscript}), i, i')
    | LCM2S =>
      let (a, i') = readArg(i + 1)
      let (b, i') = readArg(i')
      let (superscript, i') = readSuperscript(i')
      Node(Fold_Lcm({a, b, superscript}), i, i')
    | NRoot2S =>
      let (degree, i') = readArg(i + 1)
      let (radicand, i') = readArg(i')
      let (superscript, i') = readSuperscript(i')
      Node(Fold_NRoot({degree, radicand, superscript}), i, i')
    | RandInt2S =>
      let (a, i') = readArg(i + 1)
      let (b, i') = readArg(i')
      let (superscript, i') = readSuperscript(i')
      Node(Fold_RandInt({a, b, superscript}), i, i')
    | Integral3 =>
      let (from, i') = readArg(i + 1)
      let (to, i') = readArg(i')
      let (body, i') = readArg(i')
      Node(Fold_Integral({from, to, body}), i, i')
    | TableNS({numRows, numColumns}) => tableS(i, ~numRows, ~numColumns)
    | UnitS({prefix, name}) =>
      let i' = i + 1
      let (superscript, i') = readSuperscript(i')
      Node(Fold_Unit({prefix, name, superscript}), i, i')
    | Arg => assert false
    }
  and readArg = (~emptyArg=defaultEmptyArg, i) => {
    let rec iter = (~accum: option<'accum>, i) => {
      switch Belt.Array.get(input, i) {
      | None => assert false
      | Some(Arg) =>
        let i' = i
        let isPlaceholder = accum == None
        let accum = switch accum {
        | Some(accum) => accum
        | None => reduce(. initial, emptyArg, (i, i'))
        }
        (map(. accum, isPlaceholder), i' + 1)
      | Some(_) =>
        switch readNodeExn(i) {
        | Node(node, i, i') =>
          let accum = Belt.Option.getWithDefault(accum, initial)
          iter(~accum=reduce(. accum, node, (i, i'))->Some, i')
        | Empty => iter(~accum, i + 1)
        }
      }
    }
    iter(~accum=None, i)
  }
  and readSuperscript = superscriptIndex => {
    let rec iter = i =>
      switch Belt.Array.get(input, i) {
      | Some(Superscript1) =>
        let (superscriptBody, i') = readArg(i + 1)
        (Some({superscriptBody, index: superscriptIndex}), i')
      | Some(CaptureGroupEndS) =>
        // If a superscript occurs immediately after a capture group, apply the
        // superscript to the last element within the capture group
        iter(i + 1)
      | _ => (None, superscriptIndex)
      }

    iter(superscriptIndex)
  }
  and fn = (i, fn) => {
    let i' = i + 1
    Node(Fold_Function({fn, resultSuperscript: None}), i, i')
  }
  and fnS = (i, fn) => {
    let i' = i + 1
    let (resultSuperscript, i') = readSuperscript(i')
    Node(Fold_Function({fn, resultSuperscript}), i, i')
  }
  and tableS = (i, ~numRows, ~numColumns) => {
    let emptyArg = Fold_Digit({nucleus: "0", superscript: None})
    let i' = i + 1
    let (i', elements) = ArrayUtil.foldMakeU(numRows * numColumns, i', (. s, _) => {
      let (element, index) = readArg(~emptyArg, s)
      (index, element)
    })
    let (superscript, i') = readSuperscript(i')
    Node(
      Fold_Table({
        elements,
        numRows,
        numColumns,
        superscript,
      }),
      i,
      i',
    )
  }

  let readUntilEnd = () => {
    let rec iter = (~accum, i) =>
      switch Belt.Array.get(input, i) {
      | None => map(. accum, false)
      | Some(Arg) => assert false
      | Some(_) =>
        switch readNodeExn(i) {
        | Node(node, i, i') => iter(~accum=reduce(. accum, node, (i, i')), i')
        | Empty => iter(~accum, i + 1)
        }
      }

    iter(~accum=initial, 0)
  }

  readUntilEnd()
}
