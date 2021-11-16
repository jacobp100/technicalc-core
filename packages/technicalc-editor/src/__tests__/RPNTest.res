open Jest

let submit = (rpn, editState) =>
  switch RPN.submit(rpn, editState) {
  | Ok(rpn) => Ok((rpn, EditState.empty))
  | Error(_) as e => e
  }

let toValue = rpn => {
  let ast = switch RPN.elements(rpn) {
  | Some(elements) => Value.parse(elements)
  | None => Error(-1)
  }
  switch ast {
  | Ok(ast) =>
    TechniCalcCalculator.AST_Evaluation.eval(
      ~context=TechniCalcCalculator.AST.emptyContext,
      ~config={angleMode: Radian},
      ast,
    )->Some
  | Error(_) => None
  }
}

@warning("-8")
test("push when everything empty is no-op", (. ()) => {
  let rpn = RPN.empty
  let editState = EditState.empty
  let Ok((rpn, editState)) = submit(rpn, editState)
  let Ok((rpn, editState)) = submit(rpn, editState)
  let Ok((rpn, editState)) = submit(rpn, editState)

  expect(rpn)->toEqual(RPN.empty)
  expect(editState)->toEqual(EditState.empty)
})

@warning("-8")
test("push last result when edit state empty", (. ()) => {
  let rpn = RPN.empty
  let editState = EditState.empty
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, N1_S)
  let Ok((rpn, editState)) = submit(rpn, editState)
  let Ok((rpn, editState)) = submit(rpn, editState)
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, Add)

  expect(editState)->toEqual(EditState.empty)

  let Some(value) = toValue(rpn)
  expect(value)->toEqual(TechniCalcCalculator.Value.ofInt(2))
})

@warning("-8")
test("sub", (. ()) => {
  let rpn = RPN.empty
  let editState = EditState.empty
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, N3_S)
  let Ok((rpn, editState)) = submit(rpn, editState)
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, N2_S)
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, Sub)
  expect(editState)->toEqual(EditState.empty)

  let Some(value) = toValue(rpn)
  expect(value)->toEqual(TechniCalcCalculator.Value.ofInt(1))
})

@warning("-8")
test("operators add -> add", (. ()) => {
  let rpn = RPN.empty
  let editState = EditState.empty
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, N1_S)
  let Ok((rpn, editState)) = submit(rpn, editState)
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, N2_S)
  let Ok((rpn, editState)) = submit(rpn, editState)
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, N3_S)
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, Add)
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, Add)

  expect(editState)->toEqual(EditState.empty)

  let Some(elements) = RPN.elements(rpn)
  expect(elements)->toEqual([N1_S, Add, N2_S, Add, N3_S])

  let Some(value) = toValue(rpn)
  expect(value)->toEqual(TechniCalcCalculator.Value.ofInt(6))
})

@warning("-8")
test("operators add -> add", (. ()) => {
  let rpn = RPN.empty
  let editState = EditState.empty
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, N2_S)
  let Ok((rpn, editState)) = submit(rpn, editState)
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, N3_S)
  let Ok((rpn, editState)) = submit(rpn, editState)
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, N4_S)
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, Mul)
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, Mul)

  expect(editState)->toEqual(EditState.empty)

  let Some(elements) = RPN.elements(rpn)
  expect(elements)->toEqual([N2_S, Mul, N3_S, Mul, N4_S])

  let Some(value) = toValue(rpn)
  expect(value)->toEqual(TechniCalcCalculator.Value.ofInt(24))
})

@warning("-8")
test("operators mul -> add", (. ()) => {
  let rpn = RPN.empty
  let editState = EditState.empty
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, N6_S)
  let Ok((rpn, editState)) = submit(rpn, editState)
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, N5_S)
  let Ok((rpn, editState)) = submit(rpn, editState)
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, N4_S)
  let Ok((rpn, editState)) = submit(rpn, editState)
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, Mul)
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, Add)

  expect(editState)->toEqual(EditState.empty)

  let Some(elements) = RPN.elements(rpn)
  expect(elements)->toEqual([N6_S, Add, N5_S, Mul, N4_S])

  let Some(value) = toValue(rpn)
  expect(value)->toEqual(TechniCalcCalculator.Value.ofInt(26))
})

@warning("-8")
test("operators add -> mul", (. ()) => {
  let rpn = RPN.empty
  let editState = EditState.empty
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, N6_S)
  let Ok((rpn, editState)) = submit(rpn, editState)
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, N5_S)
  let Ok((rpn, editState)) = submit(rpn, editState)
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, N4_S)
  let Ok((rpn, editState)) = submit(rpn, editState)
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, Add)
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, Mul)

  expect(editState)->toEqual(EditState.empty)

  let Some(elements) = RPN.elements(rpn)
  expect(elements)->toEqual([N6_S, Mul, OpenBracket, N5_S, Add, N4_S, CloseBracketS])

  let Some(value) = toValue(rpn)
  expect(value)->toEqual(TechniCalcCalculator.Value.ofInt(54))
})

@warning("-8")
test("operators add -> mul -> add", (. ()) => {
  let rpn = RPN.empty
  let editState = EditState.empty
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, N6_S)
  let Ok((rpn, editState)) = submit(rpn, editState)
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, N5_S)
  let Ok((rpn, editState)) = submit(rpn, editState)
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, N4_S)
  let Ok((rpn, editState)) = submit(rpn, editState)
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, N3_S)
  let Ok((rpn, editState)) = submit(rpn, editState)
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, Add)
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, Mul)
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, Add)

  expect(editState)->toEqual(EditState.empty)

  let Some(elements) = RPN.elements(rpn)
  expect(elements)->toEqual([N6_S, Add, N5_S, Mul, OpenBracket, N4_S, Add, N3_S, CloseBracketS])

  let Some(value) = toValue(rpn)
  expect(value)->toEqual(TechniCalcCalculator.Value.ofInt(41))
})

@warning("-8")
test("operators mul -> add -> mul", (. ()) => {
  let rpn = RPN.empty
  let editState = EditState.empty
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, N6_S)
  let Ok((rpn, editState)) = submit(rpn, editState)
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, N5_S)
  let Ok((rpn, editState)) = submit(rpn, editState)
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, N4_S)
  let Ok((rpn, editState)) = submit(rpn, editState)
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, N3_S)
  let Ok((rpn, editState)) = submit(rpn, editState)
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, Mul)
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, Add)
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, Mul)

  expect(editState)->toEqual(EditState.empty)

  let Some(elements) = RPN.elements(rpn)
  expect(elements)->toEqual([N6_S, Mul, OpenBracket, N5_S, Add, N4_S, Mul, N3_S, CloseBracketS])

  let Some(value) = toValue(rpn)
  expect(value)->toEqual(TechniCalcCalculator.Value.ofInt(102))
})

@warning("-8")
test("superscripts", (. ()) => {
  let rpn = RPN.empty
  let editState = EditState.empty
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, N2_S)
  let Ok((rpn, editState)) = submit(rpn, editState)
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, N3_S)
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, Superscript1)

  expect(editState)->toEqual(EditState.empty)

  let Some(value) = toValue(rpn)
  expect(value)->toEqual(TechniCalcCalculator.Value.ofInt(8))
})

@warning("-8")
test("fractions", (. ()) => {
  let rpn = RPN.empty
  let editState = EditState.empty
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, N1_S)
  let Ok((rpn, editState)) = submit(rpn, editState)
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, N2_S)
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, Frac2S)

  expect(editState)->toEqual(EditState.empty)

  let Some(value) = toValue(rpn)
  expect(value)->toEqual(TechniCalcCalculator.Value.ofFloat(0.5))
})

@warning("-8")
test("abs", (. ()) => {
  let rpn = RPN.empty
  let editState = EditState.empty
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, Sub)
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, N1_S)
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, Abs1S)

  expect(editState)->toEqual(EditState.empty)

  let Some(value) = toValue(rpn)
  expect(value)->toEqual(TechniCalcCalculator.Value.one)
})

@warning("-8")
test("sin", (. ()) => {
  let rpn = RPN.empty
  let editState = EditState.empty
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, ConstPiS)
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, SinS)

  expect(editState)->toEqual(EditState.empty)

  let Some(value) = toValue(rpn)
  expect(value)->toEqual(TechniCalcCalculator.Value.zero)
})

@warning("-8")
test("square roots", (. ()) => {
  let rpn = RPN.empty
  let editState = EditState.empty
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, N1_S)
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, N6_S)
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, Sqrt1S)

  expect(editState)->toEqual(EditState.empty)

  let Some(value) = toValue(rpn)
  expect(value)->toEqual(TechniCalcCalculator.Value.ofInt(4))
})

@warning("-8")
test("cube roots", (. ()) => {
  let rpn = RPN.empty
  let editState = EditState.empty
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, N8_S)
  let Ok((rpn, editState)) = RPN.insertArray(rpn, editState, [NRoot2S, N3_S, Arg, Arg])

  expect(editState)->toEqual(EditState.empty)

  let Some(value) = toValue(rpn)
  expect(value)->toEqual(TechniCalcCalculator.Value.ofInt(2))
})

@warning("-8")
test("n roots", (. ()) => {
  let rpn = RPN.empty
  let editState = EditState.empty
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, N3_S)
  let Ok((rpn, editState)) = submit(rpn, editState)
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, N8_S)
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, NRoot2S)

  expect(editState)->toEqual(EditState.empty)

  let Some(value) = toValue(rpn)
  expect(value)->toEqual(TechniCalcCalculator.Value.ofInt(2))
})

@warning("-8")
test("n root with empty arg array", (. ()) => {
  let rpn = RPN.empty
  let editState = EditState.empty
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, N3_S)
  let Ok((rpn, editState)) = submit(rpn, editState)
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, N8_S)
  let Ok((rpn, editState)) = submit(rpn, editState)
  let Ok((rpn, editState)) = RPN.insertArray(rpn, editState, [NRoot2S, Arg, Arg])

  expect(editState)->toEqual(EditState.empty)

  let Some(value) = toValue(rpn)
  expect(value)->toEqual(TechniCalcCalculator.Value.ofInt(2))
})

@warning("-8")
test("log without base", (. ()) => {
  let rpn = RPN.empty
  let editState = EditState.empty
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, ConstES)
  let Ok((rpn, editState)) = submit(rpn, editState)
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, Log)

  expect(editState)->toEqual(EditState.empty)

  let Some(value) = toValue(rpn)
  expect(value)->toEqual(TechniCalcCalculator.Value.one)
})

@warning("-8")
test("log with base", (. ()) => {
  let rpn = RPN.empty
  let editState = EditState.empty
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, N8_S)
  let Ok((rpn, editState)) = submit(rpn, editState)
  let Ok((rpn, editState)) = RPN.insertArray(rpn, editState, [NLog1, N2_S, Arg])

  expect(editState)->toEqual(EditState.empty)

  let Some(value) = toValue(rpn)
  expect(value)->toEqual(TechniCalcCalculator.Value.ofInt(3))
})

@warning("-8")
test("log base", (. ()) => {
  let rpn = RPN.empty
  let editState = EditState.empty
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, N2_S)
  let Ok((rpn, editState)) = submit(rpn, editState)
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, N8_S)
  let Ok((rpn, editState)) = submit(rpn, editState)
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, NLog1)

  expect(editState)->toEqual(EditState.empty)

  let Some(value) = toValue(rpn)
  expect(value)->toEqual(TechniCalcCalculator.Value.ofInt(3))
})

@warning("-8")
test("log base with empty arg array", (. ()) => {
  let rpn = RPN.empty
  let editState = EditState.empty
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, N2_S)
  let Ok((rpn, editState)) = submit(rpn, editState)
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, N8_S)
  let Ok((rpn, editState)) = submit(rpn, editState)
  let Ok((rpn, editState)) = RPN.insertArray(rpn, editState, [NLog1, Arg])

  expect(editState)->toEqual(EditState.empty)

  let Some(value) = toValue(rpn)
  expect(value)->toEqual(TechniCalcCalculator.Value.ofInt(3))
})

@warning("-8")
test("factorial", (. ()) => {
  let rpn = RPN.empty
  let editState = EditState.empty
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, N5_S)
  let Ok((rpn, editState)) = RPN.insert(rpn, editState, Factorial)
  expect(editState)->toEqual(EditState.empty)

  let Some(value) = toValue(rpn)
  expect(value)->toEqual(TechniCalcCalculator.Value.ofInt(120))
})
