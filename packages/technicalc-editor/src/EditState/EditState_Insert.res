open EditState_Types
open EditState_Base

%%private(
  let skipFunction = (x, ~from, ~direction) => {
    let step = direction == EditState_Util.Forwards ? 1 : -1
    let argLevelStep = step

    let rec iter = (~index, ~argLevel) =>
      switch Belt.Array.get(x, index) {
      | None => None
      | Some(v) =>
        let index = index + step
        let argLevel = switch v {
        | AST.Arg => argLevel - argLevelStep
        | _ => argLevel + argLevelStep * AST.argCountExn(v)
        }

        if argLevel == 0 {
          let fn = direction == Forwards ? Belt.Array.getExn(x, from) : v
          Some((index, fn))
        } else if argLevel > 0 {
          iter(~index, ~argLevel)
        } else {
          None
        }
      }
    iter(~index=from, ~argLevel=0)
  }
)

/*
 It's verbose,
 but it'll make the typechecker ensure correctness after adding new elemnets
 */
type skipMode =
  | Movable
  | TopLevelFixed
  | FunctionFixed

%%private(
  let skipMode = (element: AST.t) =>
    switch element {
    | CaptureGroupStart(_)
    | CaptureGroupEndS
    | Acos
    | Acosh
    | Add
    | Asin
    | Asinh
    | Atan
    | Atanh
    | DegreeFunction
    | Div
    | Dot
    | Gamma
    | GradianFunction
    | Im
    | Log
    | Mul
    | RadianFunction
    | Re
    | Sub
    | CosecS
    | CoshS
    | CosS
    | CotS
    | SecS
    | SinhS
    | SinS
    | TanhS
    | TanS
    | NLog1
    | Differential2
    | Product2
    | Sum2
    | Frac2S
    | Vector2S
    | Integral3
    | MFrac3S
    | Vector3S
    | Matrix4S
    | Matrix9S =>
      AST.argCountExn(element) !== 0 ? FunctionFixed : TopLevelFixed
    | Arg
    | ArcMinuteUnit
    | ArcSecondUnit
    | Bin
    | Conj
    | DecimalSeparator
    | DegreeUnit
    | Factorial
    | GradianUnit
    | Hex
    | Oct
    | OpenBracket
    | Percent
    | RadianUnit
    | UnitConversion(_)
    | CloseBracketS
    | ConstES
    | ConstPiS
    | CustomAtomS(_)
    | ImaginaryUnitS
    | IteratorXS
    | N0_S
    | N1_S
    | N2_S
    | N3_S
    | N4_S
    | N5_S
    | N6_S
    | N7_S
    | N8_S
    | N9_S
    | NA_S
    | NB_S
    | NC_S
    | ND_S
    | NE_S
    | NF_S
    | RandS
    | VariableS(_)
    | Magnitude1
    | Superscript1
    | Abs1S
    | Ceil1S
    | Floor1S
    | Round1S
    | Sqrt1S
    | NCR2
    | NPR2
    | GCD2S
    | LCM2S
    | Max2S
    | Min2S
    | NRoot2S
    | RandInt2S =>
      Movable
    }
)

%%private(
  let skipInsertables = (x: array<AST.t>, ~from, ~direction) => {
    let rec iter = (~index, ~bracketLevel) =>
      switch Belt.Array.get(x, index) {
      | None if bracketLevel == 0 => Some(index)
      | Some(element) if bracketLevel == 0 && skipMode(element) == TopLevelFixed => Some(index)
      | None => None
      | Some(v) =>
        let bracketLevel = switch v {
        | OpenBracket => bracketLevel + 1
        | CloseBracketS => bracketLevel - 1
        | _ => bracketLevel
        }
        let shouldBreak = switch direction {
        | EditState_Util.Forwards => bracketLevel < 0
        | Backwards => bracketLevel > 0
        }
        let nextIndex = shouldBreak ? None : skipFunction(x, ~from=index, ~direction)
        switch nextIndex {
        | Some((_, element)) if skipMode(element) == FunctionFixed => Some(index)
        | None => Some(index)
        | Some((index, _)) => iter(~index, ~bracketLevel)
        }
      }
    iter(~index=from, ~bracketLevel=0)
  }
)

%%private(
  let countInsertables = (x: array<AST.t>, ~from, ~direction) =>
    switch skipInsertables(x, ~from, ~direction) {
    | Some(index) =>
      let i = from - index
      i > 0 ? i : -i
    | None => 0
    }
)

%%private(
  let insertElement = (elements, element, index) =>
    switch element {
    | AST.Superscript1
    | Sqrt1S =>
      let e = countInsertables(elements, ~from=index, ~direction=Forwards)
      let (elements, arg) = ArrayUtil.splice(elements, ~offset=index, ~len=e)
      let combined = Belt.Array.concatMany([[element], arg, [Arg]])
      let elements = ArrayUtil.insertArray(elements, combined, index)
      (elements, index + 1)
    | NRoot2S =>
      let e = countInsertables(elements, ~from=index, ~direction=Forwards)
      let (elements, radicand) = ArrayUtil.splice(elements, ~offset=index, ~len=e)
      let combined = Belt.Array.concatMany([[element, Arg], radicand, [Arg]])
      let elements = ArrayUtil.insertArray(elements, combined, index)
      (elements, index + 1)
    | Frac2S =>
      let s = countInsertables(elements, ~from=index - 1, ~direction=Backwards)
      let e = countInsertables(elements, ~from=index, ~direction=Forwards)
      let (elements, den) = ArrayUtil.splice(elements, ~offset=index, ~len=e)
      let (elements, num) = ArrayUtil.splice(elements, ~offset=index - s, ~len=s)
      let frac = Belt.Array.concatMany([[element], num, [Arg], den, [Arg]])
      let elements = ArrayUtil.insertArray(elements, frac, index - s)
      let nextIndex = s > 0 ? index + 2 : index + 1
      (elements, nextIndex)
    | MFrac3S =>
      let s = countInsertables(elements, ~from=index - 1, ~direction=Backwards)
      let (elements, integer) = ArrayUtil.splice(elements, ~offset=index - s, ~len=s)
      let mfrac = Belt.Array.concatMany([[element], integer, [Arg, Arg, Arg]])
      let elements = ArrayUtil.insertArray(elements, mfrac, index - s)
      let nextIndex = s > 0 ? index + 2 : index + 1
      (elements, nextIndex)
    | _ =>
      let args = switch AST.argCountExn(element) {
      | 0 => [element]
      | argCount =>
        let args = Belt.Array.make(argCount + 1, AST.Arg)
        Belt.Array.setExn(args, 0, element)
        args
      }
      let elements = ArrayUtil.insertArray(elements, args, index)
      (elements, index + 1)
    }
)

let insert = ({index, elements, formatCaptureGroups} as editState, element: AST.t) => {
  let elements = AST.normalize(elements)

  if AST_NormalizationContext.elementIsValid(elements, element, index) {
    let (elements, index) = insertElement(elements, element, index)
    make(~index, ~elements, ~formatCaptureGroups)
  } else {
    editState
  }
}

%%private(
  let firstCaptureGroupOrEmptyArgumentIndex = (~formatCaptureGroups, elements: array<AST.t>) => {
    let rec iter = (~argWillFormPlaceholder, i) =>
      switch Belt.Array.get(elements, i) {
      | Some(CaptureGroupStart(_)) if !formatCaptureGroups => Some(i + 1)
      | Some(Arg) =>
        if argWillFormPlaceholder {
          Some(i)
        } else {
          iter(~argWillFormPlaceholder=true, i + 1)
        }
      | Some(e) => iter(~argWillFormPlaceholder=AST.argCountExn(e) !== 0, i + 1)
      | None => None
      }
    iter(~argWillFormPlaceholder=false, 0)
  }
)

let insertArray = (
  {index, elements, formatCaptureGroups} as editState,
  insertedElements: array<AST.t>,
) => {
  let elements = AST.normalize(elements)

  let valid = Belt.Array.everyU(insertedElements, (. element) => {
    AST_NormalizationContext.elementIsValid(elements, element, index)
  })
  if valid {
    let elements = ArrayUtil.insertArray(elements, insertedElements, index)

    let advanceBy =
      firstCaptureGroupOrEmptyArgumentIndex(
        ~formatCaptureGroups,
        insertedElements,
      )->Belt.Option.getWithDefault(Belt.Array.length(insertedElements))

    let index = index + advanceBy
    make(~index, ~elements, ~formatCaptureGroups)
  } else {
    editState
  }
}
