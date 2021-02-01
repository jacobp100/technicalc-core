let%private basePrefixExn = base =>
  switch (base) {
  | 2 => "0b"
  | 8 => "0o"
  | 16 => "0x"
  | _ => ""
  };

let%private trimTraillingZeros = (~startIndex=0, ~endIndex=?, string) => {
  let endIndex =
    endIndex->Belt.Option.getWithDefault(String.length(string) - 1);
  let sliceIndex = ref(endIndex);
  let break = ref(false);
  while (sliceIndex^ >= startIndex && ! break^) {
    switch (StringUtil.stringCharAtUnsafe(string, sliceIndex^)) {
    | "0" => sliceIndex := sliceIndex^ - 1
    | "." =>
      sliceIndex := sliceIndex^ - 1;
      break := true;
    | _ => break := true
    };
  };

  StringUtil.slice(string, 0, sliceIndex^ + 1)
  ++ StringUtil.slice(string, endIndex + 1, String.length(string));
};

let%private adddigitGrouping = (~startIndex=0, ~endIndex=?, string) => {
  let endIndex = endIndex->Belt.Option.getWithDefault(String.length(string));
  let baseStr = ref(string);
  let index = ref(endIndex - 3);
  while (index^ > startIndex) {
    let len = String.length(baseStr^);
    baseStr :=
      StringUtil.slice(baseStr^, 0, index^)
      ++ ","
      ++ StringUtil.slice(baseStr^, index^, len);
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
  basePrefixExn(base) ++ StringUtil.toUpperCase(str);
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

  let integer = formatInteger(~base, ~digitGrouping, integerPart);
  let integer = Decimal.(num >= zero) ? integer : "-" ++ integer;

  let decimal =
    if (maxDecimalPlaces == 0) {
      "";
    } else if (Decimal.(decimalPart == zero)) {
      StringUtil.make(minDecimalPlaces, '0');
    } else {
      let decimalAsInteger =
        Decimal.(
          floor(decimalPart * ofInt(base) ** ofInt(maxDecimalPlaces))
        );
      let baseStr = decimalToString(~base, decimalAsInteger);
      let str =
        StringUtil.make(maxDecimalPlaces - String.length(baseStr), '0')
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
  let exponentPart = Belt.Int.toString(exponent);
  (decimalPart, exponentPart);
};
