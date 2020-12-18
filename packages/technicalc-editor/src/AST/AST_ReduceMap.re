open AST_Types;

type superscript('a) = {
  superscriptBody: 'a,
  index: int,
};

type base =
  | Bin
  | Oct
  | Hex;
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
  | Tanh;
type angle =
  | Degree
  | ArcMinute
  | ArcSecond
  | Gradian;
type operator =
  | Add
  | Sub
  | Mul
  | Div
  | Dot;

type foldState('a) =
  | Abs({
      arg: 'a,
      superscript: option(superscript('a)),
    })
  | Angle(angle)
  | Base(base)
  | Ceil({
      arg: 'a,
      superscript: option(superscript('a)),
    })
  | CloseBracket(option(superscript('a)))
  | ConstE(option(superscript('a)))
  | ConstPi(option(superscript('a)))
  | Conj
  | CustomAtom({
      mml: string,
      value: string,
      superscript: option(superscript('a)),
    })
  | DecimalSeparator
  | Differential({
      at: 'a,
      body: 'a,
    })
  | Digit({
      nucleus: string,
      superscript: option(superscript('a)),
    })
  | Factorial
  | Floor({
      arg: 'a,
      superscript: option(superscript('a)),
    })
  | Frac({
      num: 'a,
      den: 'a,
      superscript: option(superscript('a)),
    })
  | Function({
      func,
      resultSuperscript: option(superscript('a)),
    })
  | Gcd({
      a: 'a,
      b: 'a,
      superscript: option(superscript('a)),
    })
  | ImaginaryUnit(option(superscript('a)))
  | Integral({
      from: 'a,
      to_: 'a,
      body: 'a,
    })
  | Label({
      mml: string,
      superscript: option(superscript('a)),
    })
  | Lcm({
      a: 'a,
      b: 'a,
      superscript: option(superscript('a)),
    })
  | Magnitude({value: 'a})
  | Min({
      a: 'a,
      b: 'a,
      superscript: option(superscript('a)),
    })
  | Max({
      a: 'a,
      b: 'a,
      superscript: option(superscript('a)),
    })
  | NCR({
      n: 'a,
      r: 'a,
    })
  | NLog({base: 'a})
  | NPR({
      n: 'a,
      r: 'a,
    })
  | NRoot({
      degree: 'a,
      radicand: 'a,
      superscript: option(superscript('a)),
    })
  | OpenBracket
  | Operator(operator)
  | Percent
  | Product({
      from: 'a,
      to_: 'a,
    })
  | Rand(option(superscript('a)))
  | RandInt({
      a: 'a,
      b: 'a,
      superscript: option(superscript('a)),
    })
  | Round({
      arg: 'a,
      superscript: option(superscript('a)),
    })
  | Sqrt({
      radicand: 'a,
      superscript: option(superscript('a)),
    })
  | Sum({
      from: 'a,
      to_: 'a,
    })
  | Superscript('a)
  | Vector({
      elements: array('a),
      superscript: option(superscript('a)),
    })
  | Table({
      elements: array('a),
      numRows: int,
      numColumns: int,
      superscript: option(superscript('a)),
    })
  | UnitConversion({
      fromUnits: array(TechniCalcCalculator.Unit_Types.unitPower),
      toUnits: array(TechniCalcCalculator.Unit_Types.unitPower),
    })
  | Variable({
      nucleus: string,
      superscript: option(superscript('a)),
    });

type range = (int, int);

let superscriptBody = superscript => superscript.superscriptBody;

let%private digitNucleus = digit =>
  switch (digit) {
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
  | _ => assert(false)
  };

let reduceMap =
    (
      input: array(t),
      ~reduce: ('accum, foldState('a), range) => 'accum,
      ~map: ('accum, range) => 'value,
      ~initial: 'accum,
    )
    : 'value => {
  let rec readNodeExn = (i): (foldState('a), int) =>
    switch (Belt.Array.getExn(input, i)) {
    | Conj => (Conj, i + 1)
    | DecimalSeparator => (DecimalSeparator, i + 1)
    | Factorial => (Factorial, i + 1)
    | OpenBracket => (OpenBracket, i + 1)
    | Percent => (Percent, i + 1)
    | Bin => (Base(Bin), i + 1)
    | Oct => (Base(Oct), i + 1)
    | Hex => (Base(Hex), i + 1)
    | Add => (Operator(Add), i + 1)
    | Sub => (Operator(Sub), i + 1)
    | Mul => (Operator(Mul), i + 1)
    | Div => (Operator(Div), i + 1)
    | Dot => (Operator(Dot), i + 1)
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
    | DegreeUnit => (Angle(Degree), i + 1)
    | ArcMinuteUnit => (Angle(ArcMinute), i + 1)
    | ArcSecondUnit => (Angle(ArcSecond), i + 1)
    | GradianUnit => (Angle(Gradian), i + 1)
    | UnitConversion({fromUnits, toUnits}) => (
        UnitConversion({fromUnits, toUnits}),
        i + 1,
      )
    | CloseBracketS =>
      let i' = i + 1;
      let (superscript, i') = readSuperscript(i');
      (CloseBracket(superscript), i');
    | ConstPiS =>
      let i' = i + 1;
      let (superscript, i') = readSuperscript(i');
      (ConstPi(superscript), i');
    | ConstES =>
      let i' = i + 1;
      let (superscript, i') = readSuperscript(i');
      (ConstE(superscript), i');
    | CustomAtomS({mml, value}) =>
      let i' = i + 1;
      let (superscript, i') = readSuperscript(i');
      (CustomAtom({mml, value, superscript}), i');
    | (N0_S | N1_S | N2_S | N3_S | N4_S | N5_S | N6_S | N7_S | N8_S | N9_S) as digit
    | (NA_S | NB_S | NC_S | ND_S | NE_S | NF_S) as digit =>
      let nucleus = digitNucleus(digit);
      let i' = i + 1;
      let (superscript, i') = readSuperscript(i');
      (Digit({nucleus, superscript}), i');
    | ImaginaryUnitS =>
      let i' = i + 1;
      let (superscript, i') = readSuperscript(i');
      (ImaginaryUnit(superscript), i');
    | RandS =>
      let i' = i + 1;
      let (superscript, i') = readSuperscript(i');
      (Rand(superscript), i');
    | VariableS(nucleus) =>
      let i' = i + 1;
      let (superscript, i') = readSuperscript(i');
      (Variable({nucleus, superscript}), i');
    | LabelS({mml}) =>
      let i' = i + 1;
      let (superscript, i') = readSuperscript(i');
      (Label({mml, superscript}), i');
    | Magnitude1 =>
      let (value, i') = readArg(i + 1);
      (Magnitude({value: value}), i');
    | Superscript1 =>
      let (superscript, i') = readArg(i + 1);
      (Superscript(superscript), i');
    | NLog1 =>
      let (base, i') = readArg(i + 1);
      (NLog({base: base}), i');
    | Abs1S =>
      let (arg, i') = readArg(i + 1);
      let (superscript, i') = readSuperscript(i');
      (Abs({arg, superscript}), i');
    | Ceil1S =>
      let (arg, i') = readArg(i + 1);
      let (superscript, i') = readSuperscript(i');
      (Ceil({arg, superscript}), i');
    | Floor1S =>
      let (arg, i') = readArg(i + 1);
      let (superscript, i') = readSuperscript(i');
      (Floor({arg, superscript}), i');
    | Round1S =>
      let (arg, i') = readArg(i + 1);
      let (superscript, i') = readSuperscript(i');
      (Round({arg, superscript}), i');
    | Sqrt1S =>
      let (radicand, i') = readArg(i + 1);
      let (superscript, i') = readSuperscript(i');
      (Sqrt({radicand, superscript}), i');
    | Differential2 =>
      let (body, i') = readArg(i + 1);
      let (at, i') = readArg(i');
      (Differential({at, body}), i');
    | NCR2 =>
      let (n, i') = readArg(i + 1);
      let (r, i') = readArg(i');
      (NCR({n, r}), i');
    | NPR2 =>
      let (n, i') = readArg(i + 1);
      let (r, i') = readArg(i');
      (NPR({n, r}), i');
    | Product2 =>
      let (from, i') = readArg(i + 1);
      let (to_, i') = readArg(i');
      (Product({from, to_}), i');
    | Sum2 =>
      let (from, i') = readArg(i + 1);
      let (to_, i') = readArg(i');
      (Sum({from, to_}), i');
    | Frac2S =>
      let (num, i') = readArg(i + 1);
      let (den, i') = readArg(i');
      let (superscript, i') = readSuperscript(i');
      (Frac({num, den, superscript}), i');
    | Min2S =>
      let (a, i') = readArg(i + 1);
      let (b, i') = readArg(i');
      let (superscript, i') = readSuperscript(i');
      (Min({a, b, superscript}), i');
    | Max2S =>
      let (a, i') = readArg(i + 1);
      let (b, i') = readArg(i');
      let (superscript, i') = readSuperscript(i');
      (Max({a, b, superscript}), i');
    | Gcd2S =>
      let (a, i') = readArg(i + 1);
      let (b, i') = readArg(i');
      let (superscript, i') = readSuperscript(i');
      (Gcd({a, b, superscript}), i');
    | Lcm2S =>
      let (a, i') = readArg(i + 1);
      let (b, i') = readArg(i');
      let (superscript, i') = readSuperscript(i');
      (Lcm({a, b, superscript}), i');
    | NRoot2S =>
      let (degree, i') = readArg(i + 1);
      let (radicand, i') = readArg(i');
      let (superscript, i') = readSuperscript(i');
      (NRoot({degree, radicand, superscript}), i');
    | RandInt2S =>
      let (a, i') = readArg(i + 1);
      let (b, i') = readArg(i');
      let (superscript, i') = readSuperscript(i');
      (RandInt({a, b, superscript}), i');
    | Integral3 =>
      let (from, i') = readArg(i + 1);
      let (to_, i') = readArg(i');
      let (body, i') = readArg(i');
      (Integral({from, to_, body}), i');
    | Vector2S => vectorS(i, ~numElements=2)
    | Vector3S => vectorS(i, ~numElements=3)
    | Matrix4S => tableS(i, ~numRows=2, ~numColumns=2)
    | Matrix9S => tableS(i, ~numRows=3, ~numColumns=3)
    | Arg => assert(false)
    }
  and readArg = (~accum=initial, ~start=?, i) => {
    let start = Belt.Option.getWithDefault(start, i);
    switch (Belt.Array.get(input, i)) {
    | None => assert(false)
    | Some(Arg) =>
      let i' = i;
      (map(accum, (start, i')), i' + 1);
    | Some(_) =>
      let (node, i') = readNodeExn(i);
      readArg(~accum=reduce(accum, node, (i, i')), i');
    };
  }
  and readSuperscript = i =>
    switch (Belt.Array.get(input, i)) {
    | Some(Superscript1) =>
      let (superscriptBody, i') = readArg(i + 1);
      (Some({superscriptBody, index: i}), i');
    | _ => (None, i)
    }
  and func = (i, func) => {
    (Function({func, resultSuperscript: None}), i + 1);
  }
  and funcS = (i, func) => {
    let i' = i + 1;
    let (resultSuperscript, i') = readSuperscript(i');
    (Function({func, resultSuperscript}), i');
  }
  and vectorS = (i, ~numElements) => {
    let i' = i + 1;
    let (i', elements) =
      ArrayUtil.foldMake(
        numElements,
        i',
        (s, _) => {
          let (element, index) = readArg(s);
          (index, element);
        },
      );
    let (superscript, i') = readSuperscript(i');
    (Vector({elements, superscript}), i');
  }
  and tableS = (i, ~numRows, ~numColumns) => {
    let i' = i + 1;
    let (i', elements) =
      ArrayUtil.foldMake(
        numRows * numColumns,
        i',
        (s, _) => {
          let (element, index) = readArg(s);
          (index, element);
        },
      );
    let (superscript, i') = readSuperscript(i');
    (Table({elements, numRows, numColumns, superscript}), i');
  };

  let rec readUntilEnd = (~accum=initial, i) =>
    switch (Belt.Array.get(input, i)) {
    | None => map(accum, (0, i))
    | Some(Arg) => assert(false)
    | Some(_) =>
      let (node, i') = readNodeExn(i);
      readUntilEnd(~accum=reduce(accum, node, (i, i')), i');
    };

  readUntilEnd(0);
};
