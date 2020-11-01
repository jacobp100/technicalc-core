let trimTraillingZeros = (~startIndex=0, ~endIndex=?, string) => {
  let endIndex =
    endIndex->Belt.Option.getWithDefault(String.length(string) - 1);
  let sliceIndex = ref(endIndex);
  let break = ref(false);
  while (sliceIndex^ >= startIndex && ! break^) {
    switch (Js.String.get(string, sliceIndex^)) {
    | "0" => sliceIndex := sliceIndex^ - 1
    | "." =>
      sliceIndex := sliceIndex^ - 1;
      break := true;
    | _ => break := true
    };
  };

  String.sub(string, 0, sliceIndex^ + 1)
  ++ String.sub(string, endIndex + 1, String.length(string) - 1 - endIndex);
};

let adddigitGrouping = (~startIndex=0, ~endIndex=?, string) => {
  let endIndex = endIndex->Belt.Option.getWithDefault(String.length(string));
  let baseStr = ref(string);
  let index = ref(endIndex - 3);
  while (index^ > startIndex) {
    let len = String.length(baseStr^);
    baseStr :=
      String.sub(baseStr^, 0, index^)
      ++ ","
      ++ String.sub(baseStr^, index^, len - index^);
    index := index^ - 3;
  };
  baseStr^;
};

let%private decimalToString = (~base=10, num) =>
  switch (base) {
  | 2 => Decimal.toBinary(num)->Js.String.sliceToEnd(~from=2)
  | 8 => Decimal.toOctal(num)->Js.String.sliceToEnd(~from=2)
  | 10 => Decimal.toString(num)
  | 16 =>
    Decimal.toHexadecimal(num)
    ->Js.String.sliceToEnd(~from=2)
    ->Js.String.toUpperCase
  | _ => assert(false)
  };

let formatInteger = (~base=10, ~digitGrouping=false, num) => {
  let str = decimalToString(~base, num);
  let str =
    if (digitGrouping) {
      adddigitGrouping(~startIndex=Decimal.(num < zero) ? 1 : 0, str);
    } else {
      str;
    };
  String.uppercase_ascii(str);
};

let formatDecimal =
    (
      ~base=10,
      ~digitGrouping=false,
      ~minDecimalPlaces=0,
      ~maxDecimalPlaces,
      num,
    ) => {
  let absNum = Decimal.abs(num);
  let integerPart = Decimal.floor(absNum);
  let decimalPart = Decimal.sub(absNum, integerPart);
  let integer =
    formatInteger(
      ~base,
      ~digitGrouping,
      Decimal.(num >= zero ? integerPart : - integerPart),
    );
  let decimal =
    if (maxDecimalPlaces == 0) {
      "";
    } else if (Decimal.(decimalPart == zero)) {
      String.make(minDecimalPlaces, '0');
    } else {
      let decimalAsInteger =
        Decimal.(
          floor(decimalPart * ofInt(base) ** ofInt(maxDecimalPlaces))
        );
      let baseStr = decimalToString(~base, decimalAsInteger);
      let str =
        String.make(maxDecimalPlaces - String.length(baseStr), '0')
        ++ baseStr;
      trimTraillingZeros(~startIndex=minDecimalPlaces, str);
    };

  if (decimal != "") {
    integer ++ "." ++ decimal;
  } else {
    integer;
  };
};

let formatExponential =
    (~base=10, ~exponent=?, ~minDecimalPlaces=0, ~maxDecimalPlaces, num) => {
  let exponent =
    switch (exponent) {
    | Some(exponent) => exponent
    | None => DecimalUtil.magnitude(num)
    };
  let decimalPart =
    formatDecimal(
      ~base,
      ~digitGrouping=false,
      ~minDecimalPlaces,
      ~maxDecimalPlaces,
      Decimal.(num / ofInt(10) ** ofInt(exponent)),
    );
  let exponentPart = string_of_int(exponent);
  (decimalPart, exponentPart);
};
