open Formatting_Types

%%private(
  @inline
  let decimalSeparator = locale =>
    switch locale {
    | English => "."
    | European => ","
    }
)

%%private(
  @inline
  let groupingSeparator = locale =>
    switch locale {
    | English => ","
    | European => "."
    }
)

%%private(
  @inline
  let basePrefixExn = base =>
    switch base {
    | 2 => "0b"
    | 8 => "0o"
    | 16 => "0x"
    | _ => ""
    }
)

%%private(
  let getSliceIndex = (~locale, ~startIndex, ~endIndex, string) => {
    let decimalSeparator = decimalSeparator(locale)

    let rec iter = sliceIndex =>
      if sliceIndex >= startIndex {
        switch StringUtil.stringCharAtUnsafe(string, sliceIndex) {
        | "0" => iter(sliceIndex - 1)
        | s if s == decimalSeparator => sliceIndex - 1
        | _ => sliceIndex
        }
      } else {
        sliceIndex
      }
    iter(endIndex)
  }
)
%%private(
  let trimTraillingZeros = (~locale, ~startIndex=0, ~endIndex=?, string) => {
    let endIndex = endIndex->Belt.Option.getWithDefault(String.length(string) - 1)
    let sliceIndex = getSliceIndex(~locale, ~startIndex, ~endIndex, string)

    StringUtil.slice(string, 0, sliceIndex + 1) ++
    StringUtil.slice(string, endIndex + 1, String.length(string))
  }
)

%%private(
  @inline
  let adddigitGrouping = (~locale, ~startIndex=0, ~endIndex=?, string) => {
    let endIndex = endIndex->Belt.Option.getWithDefault(String.length(string))
    let baseStr = ref(string)
    let index = ref(endIndex - 3)

    let groupingSeparator = groupingSeparator(locale)

    while index.contents > startIndex {
      let len = String.length(baseStr.contents)
      baseStr :=
        StringUtil.slice(baseStr.contents, 0, index.contents) ++
        groupingSeparator ++
        StringUtil.slice(baseStr.contents, index.contents, len)
      index := index.contents - 3
    }
    baseStr.contents
  }
)

%%private(
  let decimalToString = (~locale, ~base, num) => {
    let str = switch base {
    | 2 => Decimal.toBinary(num)->Js.String.sliceToEnd(_, ~from=2)
    | 8 => Decimal.toOctal(num)->Js.String.sliceToEnd(_, ~from=2)
    | 10 => Decimal.toString(num)
    | 16 => Decimal.toHexadecimal(num)->Js.String.sliceToEnd(_, ~from=2)->Js.String.toUpperCase
    | _ => assert false
    }
    switch decimalSeparator(locale) {
    | "." => str
    | s => StringUtil.replaceFirst(str, ".", s)
    }
  }
)

let formatInteger = (~locale, ~base, ~digitGrouping, num) => {
  let str = decimalToString(~locale, ~base, num)
  let str = if digitGrouping {
    adddigitGrouping(~locale, ~startIndex=Decimal.lt(num, Decimal.zero) ? 1 : 0, str)
  } else {
    str
  }
  basePrefixExn(base) ++ StringUtil.toUpperCase(str)
}

let formatDecimal = (
  ~locale,
  ~base,
  ~digitGrouping,
  ~minDecimalPlaces=0,
  ~maxDecimalPlaces,
  num,
) => {
  let absNum = Decimal.abs(num)
  let integerPart = Decimal.floor(absNum)
  let decimalPart = Decimal.sub(absNum, integerPart)

  let integer = formatInteger(~locale, ~base, ~digitGrouping, integerPart)
  let integer = Decimal.gte(num, Decimal.zero) ? integer : "-" ++ integer

  let decimal = if maxDecimalPlaces == 0 {
    ""
  } else if Decimal.eq(decimalPart, Decimal.zero) {
    StringUtil.make(minDecimalPlaces, '0')
  } else {
    let decimalAsInteger = {
      open Decimal
      floor(decimalPart * ofInt(base) ** ofInt(maxDecimalPlaces))
    }
    let baseStr = decimalToString(~locale, ~base, decimalAsInteger)
    let str = StringUtil.make(maxDecimalPlaces - String.length(baseStr), '0') ++ baseStr
    trimTraillingZeros(~locale, ~startIndex=minDecimalPlaces, str)
  }

  if decimal != "" {
    let decimalSeparator = decimalSeparator(locale)
    integer ++ decimalSeparator ++ decimal
  } else {
    integer
  }
}

let formatExponential = (
  ~locale,
  ~base,
  ~exponent=?,
  ~minDecimalPlaces=0,
  ~maxDecimalPlaces,
  num,
) => {
  let exponent = switch exponent {
  | Some(exponent) => exponent
  | None => DecimalUtil.magnitude(num)
  }
  let decimalPart = formatDecimal(
    ~locale,
    ~base,
    ~digitGrouping=false,
    ~minDecimalPlaces,
    ~maxDecimalPlaces,
    {
      open Decimal
      num / ofInt(10) ** exponent
    },
  )
  let exponentPart = Decimal.toString(exponent)
  (decimalPart, exponentPart)
}
