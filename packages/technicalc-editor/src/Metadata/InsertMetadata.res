type insertRanges = {
  noTablePermittedRanges: Ranges.t,
  noIterationPermittedRanges: Ranges.t,
}

let insertRanges = (elements: array<AST.t>) => {
  noTablePermittedRanges: AST_NormalizationContext.noTablePermittedRanges(elements),
  noIterationPermittedRanges: AST_NormalizationContext.noIterationPermittedRanges(elements),
}

let canInsertTable = (insertRanges, index) =>
  !Ranges.contains(insertRanges.noTablePermittedRanges, index)

let canInsertIteration = (insertRanges, index) =>
  !Ranges.contains(insertRanges.noIterationPermittedRanges, index)
