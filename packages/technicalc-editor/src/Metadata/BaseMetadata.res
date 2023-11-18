open AST_Types

type baseStart = {
  start: int,
  base: int,
}

type baseRange = {
  start: int,
  end: int,
  base: int,
}

%%private(
  @inline
  let prependBaseRange = (~rangesRev: list<baseRange>, ~baseStart: option<baseStart>, i) =>
    switch baseStart {
    | Some({start, base}) =>
      let end = i
      let range = {start, end, base}
      let rangesRev = list{range, ...rangesRev}
      rangesRev
    | None => rangesRev
    }
)

%%private(
  let prependBaseRanges = (ast: array<t>, rangesRev, index) => {
    let rec iter = (~rangesRev: list<baseRange>, ~baseStart: option<baseStart>, i) =>
      switch Belt.Array.get(ast, i) {
      | None =>
        let rangesRev = prependBaseRange(~rangesRev, ~baseStart, i)
        rangesRev
      | Some(Bin) =>
        let rangesRev = prependBaseRange(~rangesRev, ~baseStart, i)
        iter(~rangesRev, ~baseStart=Some({start: i + 1, base: 2}), i + 1)
      | Some(Oct) =>
        let rangesRev = prependBaseRange(~rangesRev, ~baseStart, i)
        iter(~rangesRev, ~baseStart=Some({start: i + 1, base: 8}), i + 1)
      | Some(Hex) =>
        let rangesRev = prependBaseRange(~rangesRev, ~baseStart, i)
        iter(~rangesRev, ~baseStart=Some({start: i + 1, base: 16}), i + 1)
      | Some(N0_S | N1_S | N2_S | N3_S | N4_S | N5_S | N6_S | N7_S | N8_S | N9_S)
      | Some(NA_S | NB_S | NC_S | ND_S | NE_S | NF_S | DecimalSeparator) =>
        iter(~rangesRev, ~baseStart, i + 1)
      | Some(_) =>
        let rangesRev = prependBaseRange(~rangesRev, ~baseStart, i)
        iter(~rangesRev, ~baseStart=None, i + 1)
      }
    iter(~rangesRev, ~baseStart=None, index)
  }
)

// Returns base ranges in order of preference
// The first range to encapsulate an index is the one to display to the user
// Returns none on empty list for better value equality in React's useMemo
let baseRanges = (ast: array<t>) =>
  switch prependBaseRanges(ast, list{}, 0) {
  | list{} => None
  | ranges => Belt.List.toArray(ranges)->ArrayUtil.reverseInPlace->Some
  }

let baseRange = (ranges, index) =>
  switch ranges {
  | Some(ranges) => Belt.Array.getByU(ranges, (. {start, end}) => start <= index && end >= index)
  | None => None
  }
