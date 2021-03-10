type insertRanges = {
  noTableRanges: Ranges.t,
  noIterationRanges: Ranges.t,
}

let insertRanges = (elements: array<AST.t>) => {
  noTableRanges: AST_NormalizationContext.noTableRanges(elements),
  noIterationRanges: AST_NormalizationContext.noIterationRanges(elements),
}

let canInsertTable = (insertRanges, index) => !Ranges.contains(insertRanges.noTableRanges, index)

let canInsertIteration = (insertRanges, index) =>
  !Ranges.contains(insertRanges.noIterationRanges, index)
