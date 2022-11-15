open EditState_Types
open EditState_Base

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
    | DegFunction
    | Div
    | Dot
    | Gamma
    | GradFunction
    | Im
    | Log
    | Mul
    | RadFunction
    | Re
    | Rem
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
    | Integral3
    | MFrac3S
    | TableNS(_) =>
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
    | IterationXS
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
  let advancePastMovableElements = (~direction: AST.direction, x: array<AST.t>, index) => {
    let rec iter = (~bracketLevel, index) =>
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
        | Forwards => bracketLevel < 0
        | Backwards => bracketLevel > 0
        }
        let nextIndex = shouldBreak ? None : AST.advanceScopeIndex(~direction, x, index)
        switch nextIndex {
        | Some(nextIndex) =>
          let fnIndex = direction == Forwards ? index : nextIndex + 1
          let fn = Belt.Array.getExn(x, fnIndex)

          if skipMode(fn) == FunctionFixed {
            Some(index)
          } else {
            iter(~bracketLevel, nextIndex)
          }
        | None => Some(index)
        }
      }
    iter(~bracketLevel=0, index)
  }
)

%%private(
  let countMovableElements = (~direction, ~from as startIndex, x: array<AST.t>) =>
    switch advancePastMovableElements(~direction, x, startIndex) {
    | Some(endIndex) =>
      let i = startIndex - endIndex
      i > 0 ? i : -i
    | None => 0
    }
)

%%private(
  let bracketLevel = (~direction=AST.Forwards, ~from=0, x: array<AST.t>) => {
    let rec iter = (~bracketLevel, index) =>
      switch Belt.Array.get(x, index) {
      | None => bracketLevel
      | Some(e) =>
        let bracketLevel = switch e {
        | OpenBracket => bracketLevel + 1
        | CloseBracketS => bracketLevel - 1
        | _ => bracketLevel
        }
        switch AST.advanceScopeIndex(x, index, ~direction) {
        | Some(index) => iter(~bracketLevel, index)
        | None => bracketLevel
        }
      }
    iter(~bracketLevel=0, from)
  }
)

%%private(
  let insertElement = (elements, element, index) =>
    switch element {
    | AST.Superscript1
    | Sqrt1S =>
      let e = countMovableElements(elements, ~from=index, ~direction=Forwards)
      let (elements, arg) = ArrayUtil.splice(elements, ~offset=index, ~len=e)
      let combined = Belt.Array.concatMany([[element], arg, [Arg]])
      let elements = ArrayUtil.insertArray(elements, combined, index)
      (elements, index + 1)
    | NRoot2S =>
      let e = countMovableElements(elements, ~from=index, ~direction=Forwards)
      let (elements, radicand) = ArrayUtil.splice(elements, ~offset=index, ~len=e)
      let combined = Belt.Array.concatMany([[element, Arg], radicand, [Arg]])
      let elements = ArrayUtil.insertArray(elements, combined, index)
      (elements, index + 1)
    | Frac2S =>
      let s = countMovableElements(elements, ~from=index - 1, ~direction=Backwards)
      let e = countMovableElements(elements, ~from=index, ~direction=Forwards)
      let (elements, den) = ArrayUtil.splice(elements, ~offset=index, ~len=e)
      let (elements, num) = ArrayUtil.splice(elements, ~offset=index - s, ~len=s)
      let frac = Belt.Array.concatMany([[element], num, [Arg], den, [Arg]])
      let elements = ArrayUtil.insertArray(elements, frac, index - s)
      let nextIndex = s > 0 ? index + 2 : index + 1
      (elements, nextIndex)
    | MFrac3S =>
      let s = countMovableElements(elements, ~from=index - 1, ~direction=Backwards)
      let (elements, integer) = ArrayUtil.splice(elements, ~offset=index - s, ~len=s)
      let mfrac = Belt.Array.concatMany([[element], integer, [Arg, Arg, Arg]])
      let elements = ArrayUtil.insertArray(elements, mfrac, index - s)
      let nextIndex = s > 0 ? index + 2 : index + 1
      (elements, nextIndex)
    | OpenBracket =>
      let shouldAppendCloseBracket = bracketLevel(elements, ~from=index) >= 0
      let combined = shouldAppendCloseBracket ? [element, CloseBracketS] : [element]
      let elements = ArrayUtil.insertArray(elements, combined, index)
      (elements, index + 1)
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

type parentTable = {
  index: int,
  numRows: int,
  numColumns: int,
}

%%private(
  let rec parentTable = (elements: array<AST.t>, index: int): option<parentTable> =>
    switch AST.closestParentFunction(elements, index) {
    | Some((TableNS({numRows, numColumns}), startIndex)) =>
      Some({index: startIndex, numRows, numColumns})
    | Some((_, startIndex)) => parentTable(elements, startIndex)
    | None => None
    }
)

%%private(
  let resizeTable = (
    elements,
    index,
    ~tableStartIndex,
    ~fromRows,
    ~fromColumns,
    ~toRows,
    ~toColumns,
  ) => {
    let cellRanges = AST.functionArgRangesExn(elements, tableStartIndex)
    let tableEndIndex = Belt.Array.getExn(cellRanges, Belt.Array.length(cellRanges) - 1)->snd

    let toCells = Belt.Array.makeByU(toRows * toColumns, (. i) => {
      let column = mod(i, toColumns)
      let row = i / toColumns

      if row < fromRows && column < fromColumns {
        let index = row * fromColumns + column
        let (start, end) = Belt.Array.getExn(cellRanges, index)
        Belt.Array.slice(elements, ~offset=start, ~len=end - start)
      } else {
        [Arg]
      }
    })

    let elements = Belt.Array.concatMany([
      Belt.Array.slice(elements, ~offset=0, ~len=tableStartIndex),
      [TableNS({numRows: toRows, numColumns: toColumns})],
      Belt.Array.concatMany(toCells),
      Belt.Array.sliceToEnd(elements, tableEndIndex),
    ])

    let selectionIndex = Belt.Array.getIndexByU(cellRanges, (. (start, end)) => {
      index >= start && index < end
    })
    let index = switch selectionIndex {
    | Some(i) =>
      let (selectionStart, _) = Belt.Array.getExn(cellRanges, i)
      let column = mod(i, fromColumns)
      let row = i / fromColumns
      let delta = index - selectionStart

      let (row, column, delta) = if row < toRows && column < toColumns {
        (row, column, delta)
      } else {
        (min(row, toRows - 1), min(column, toColumns - 1), Pervasives.max_int)
      }
      let index = row * toColumns + column

      let elementsBefore =
        toCells
        ->Belt.Array.slice(~offset=0, ~len=index)
        ->Belt.Array.reduceU(0, (. accum, cells) => accum + Belt.Array.length(cells))
      let elementsAfter = min(delta, Belt.Array.getExn(toCells, index)->Belt.Array.length - 1)

      tableStartIndex + 1 + elementsBefore + elementsAfter
    | None => tableStartIndex
    }

    (elements, index)
  }
)

let insert = ({index, elements, formatCaptureGroups} as editState, element: AST.t) => {
  let elements = AST.normalize(elements)

  // Avoid parent table lookup if element is not a table
  let resizeTableArgs = switch element {
  | TableNS({numRows, numColumns}) =>
    switch parentTable(elements, index) {
    | Some(parentTable) =>
      Some((parentTable.index, parentTable.numRows, parentTable.numColumns, numRows, numColumns))
    | None => None
    }
  | _ => None
  }

  switch resizeTableArgs {
  | Some((tableStartIndex, fromRows, fromColumns, toRows, toColumns)) =>
    let (elements, index) = resizeTable(
      elements,
      index,
      ~tableStartIndex,
      ~fromRows,
      ~fromColumns,
      ~toRows,
      ~toColumns,
    )

    make(~index, ~elements, ~formatCaptureGroups)
  | _ if AST_NormalizationContext.elementIsValid(elements, element, index) =>
    let (elements, index) = insertElement(elements, element, index)
    make(~index, ~elements, ~formatCaptureGroups)
  | _ => editState
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
