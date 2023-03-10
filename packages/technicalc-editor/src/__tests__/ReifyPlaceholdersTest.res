open Jest
open AST
open ReifyPlaceholders

let captureGroupStart = CaptureGroupStart({placeholder: None})
let captureGroupEnd = CaptureGroupEndS

test("no-op", () => {
  reifyPlaceholders([N1_S, Add, N2_S, Mul, N3_S])->expect->toEqual([N1_S, Add, N2_S, Mul, N3_S])
})

test("fraction", () => {
  reifyPlaceholders([Frac2S, N1_S, Arg, N2_S, Arg])->expect->toEqual([Frac2S, N1_S, Arg, N2_S, Arg])

  reifyPlaceholders([Frac2S, N1_S, Arg, Arg])
  ->expect
  ->toEqual([Frac2S, N1_S, Arg, captureGroupStart, captureGroupEnd, Arg])

  reifyPlaceholders([Frac2S, Arg, N2_S, Arg])
  ->expect
  ->toEqual([Frac2S, captureGroupStart, captureGroupEnd, Arg, N2_S, Arg])

  reifyPlaceholders([Frac2S, Arg, Arg])
  ->expect
  ->toEqual([
    Frac2S,
    captureGroupStart,
    captureGroupEnd,
    Arg,
    captureGroupStart,
    captureGroupEnd,
    Arg,
  ])
})

test("sin", () => {
  reifyPlaceholders([SinS, N1_S])->expect->toEqual([SinS, N1_S])
})

test("superscript", () => {
  reifyPlaceholders([N1_S, Superscript1, N2_S, Arg])
  ->expect
  ->toEqual([N1_S, Superscript1, N2_S, Arg])

  reifyPlaceholders([N1_S, Superscript1, Arg])
  ->expect
  ->toEqual([N1_S, Superscript1, captureGroupStart, captureGroupEnd, Arg])

  reifyPlaceholders([Superscript1, N2_S, Arg])
  ->expect
  ->toEqual([captureGroupStart, captureGroupEnd, Superscript1, N2_S, Arg])

  reifyPlaceholders([Superscript1, Arg])
  ->expect
  ->toEqual([
    captureGroupStart,
    captureGroupEnd,
    Superscript1,
    captureGroupStart,
    captureGroupEnd,
    Arg,
  ])

  reifyPlaceholders([Add, Superscript1, N2_S, Arg])
  ->expect
  ->toEqual([Add, captureGroupStart, captureGroupEnd, Superscript1, N2_S, Arg])

  reifyPlaceholders([Add, Superscript1, Arg])
  ->expect
  ->toEqual([
    Add,
    captureGroupStart,
    captureGroupEnd,
    Superscript1,
    captureGroupStart,
    captureGroupEnd,
    Arg,
  ])
})

test("multiple functions", () => {
  reifyPlaceholders([Sqrt1S, Frac2S, N1_S, Arg, N2_S, Arg, Arg])
  ->expect
  ->toEqual([Sqrt1S, Frac2S, N1_S, Arg, N2_S, Arg, Arg])
})
