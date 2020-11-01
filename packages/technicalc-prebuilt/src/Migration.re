module AST = TechniCalcEditor.AST;
module Encoding_Element = TechniCalcEditor.Encoding_Element;
module MutableArrayBuilder = TechniCalcEditor.MutableArrayBuilder;

module StringUtil = {
  [@bs.send] external stringCharAtUnsafe: (string, int) => string = "charAt";
  [@bs.send] external charAtUnsafe: (string, int) => char = "charCodeAt";
  [@bs.send] external join: (array(string), [@bs.as ""] _) => string = "join";
};

module Encoding_VarInt = {
  /* Characters set from URL-safe base64 variant */
  // let%private characters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_";

  let%private charToIndex = character =>
    switch (character) {
    | 'a'..'z' => Char.code(character) - Char.code('a')
    | 'A'..'Z' => Char.code(character) - Char.code('A') + 26
    | '0'..'9' => Char.code(character) - Char.code('0') + 52
    | '-' => 62
    | '_' => 63
    | _ => assert(false)
    };

  // let encodeElement = index =>
  //   if (index < 32) {
  //     StringUtil.stringCharAtUnsafe(characters, index);
  //   } else {
  //     let char0 = StringUtil.stringCharAtUnsafe(characters, index / 32 + 31);
  //     let char1 = StringUtil.stringCharAtUnsafe(characters, index mod 32);
  //     char0 ++ char1;
  //   };

  let decodedLength = string => {
    let rec iter = (~elementIndex, ~stringIndex) =>
      if (stringIndex < String.length(string)) {
        let index0 =
          StringUtil.charAtUnsafe(string, stringIndex)->charToIndex;
        let charactersRead = index0 < 32 ? 1 : 2;
        iter(
          ~elementIndex=elementIndex + 1,
          ~stringIndex=stringIndex + charactersRead,
        );
      } else {
        elementIndex;
      };
    iter(~elementIndex=0, ~stringIndex=0);
  };

  let decodeU = (string, callback) => {
    let output =
      Belt.Array.makeUninitializedUnsafe(decodedLength(string))->Some->ref;

    let elementIndex = ref(0);
    let stringIndex = ref(0);
    while (stringIndex^ < String.length(string) && output^ != None) {
      let index0 = StringUtil.charAtUnsafe(string, stringIndex^)->charToIndex;
      let element =
        if (index0 < 32) {
          stringIndex := stringIndex^ + 1;
          callback(. index0);
        } else if (stringIndex^ + 1 < String.length(string)) {
          let index1 =
            StringUtil.charAtUnsafe(string, stringIndex^ + 1)->charToIndex;
          let value = index1 + (index0 - 31) * 32;
          stringIndex := stringIndex^ + 2;
          callback(. value);
        } else {
          None;
        };

      switch (output^, element) {
      | (Some(output), Some(element)) =>
        Belt.Array.setExn(output, elementIndex^, element)
      | _ => output := None
      };

      elementIndex := elementIndex^ + 1;
    };

    output^;
  };
};

module Value_Encoding = {
  open TechniCalcCalculator;
  open Value_Types;
  open Value_Base;

  type realEncoding;

  // let%private encodeReal = (a: Real.t): realEncoding =>
  //   switch (a) {
  //   | Rational(n, d, Unit) => Obj.magic([|n, d, 0|])
  //   | Rational(n, d, Pi) => Obj.magic([|n, d, 1|])
  //   | Rational(n, d, Exp(i)) => Obj.magic([|n, d, 2, i|])
  //   | Rational(n, d, Sqrt(i)) => Obj.magic([|n, d, 3, i|])
  //   | Decimal(f) => Obj.magic(Decimal.toString(f))
  //   };

  let%private decodeReal = (a: realEncoding): Real.t =>
    if (Js.typeof(a) == "string") {
      Decimal(Decimal.ofString(Obj.magic(a)));
    } else if (Js.Array.isArray(a)) {
      switch (Obj.magic(a)) {
      | [|n, d, 0|] => Rational(n, d, Unit)
      | [|n, d, 1|] => Rational(n, d, Pi)
      | [|n, d, 2, i|] => Rational(n, d, Exp(i))
      | [|n, d, 3, i|] => Rational(n, d, Sqrt(i))
      | _ => Real.nan
      };
    } else {
      Real.nan;
    };

  type scalarEncoding =
    | Zero
    | Real(realEncoding)
    | Imag(realEncoding)
    | Complex(realEncoding, realEncoding);

  // let%private encodeScalar = (a: Scalar.t): scalarEncoding =>
  //   switch (a) {
  //   | `Z => Zero
  //   | `R(re) => Real(encodeReal(re))
  //   | `I(im) => Imag(encodeReal(im))
  //   | `C(re, im) => Complex(encodeReal(re), encodeReal(im))
  //   };

  let%private decodeScalar = (a: scalarEncoding): Scalar.t =>
    switch (a) {
    | Zero => `Z
    | Real(re) => `R(decodeReal(re))
    | Imag(im) => `I(decodeReal(im))
    | Complex(re, im) => `C((decodeReal(re), decodeReal(im)))
    };

  type encoding =
    | Zero
    | Real(realEncoding)
    | Imag(realEncoding)
    | Complex(realEncoding, realEncoding)
    | Vector(array(scalarEncoding))
    | Matrix(int, int, array(scalarEncoding))
    | Percent(scalarEncoding)
    | NaN;

  // let encode = (a: t): encoding =>
  //   switch (a) {
  //   | `Z => Zero
  //   | `R(re) => Real(encodeReal(re))
  //   | `I(im) => Imag(encodeReal(im))
  //   | `C(re, im) => Complex(encodeReal(re), encodeReal(im))
  //   | `V(elements) => Vector(elements->Belt.Array.map(encodeScalar))
  //   | `M({numRows, numColumns, elements}) =>
  //     Matrix(numRows, numColumns, elements->Belt.Array.map(encodeScalar))
  //   | `P(p) => Percent(encodeScalar(p))
  //   | `N => NaN
  //   };

  let decode = (a: encoding): t =>
    switch (a) {
    | Zero => `Z
    | Real(re) => ofReal(decodeReal(re))
    | Imag(im) => ofImag(decodeReal(im))
    | Complex(re, im) => ofComplex(decodeReal(re), decodeReal(im))
    | Vector(vector) => ofVector(Belt.Array.map(vector, decodeScalar))
    | Matrix(numRows, numColumns, elements) =>
      let elements = elements->Belt.Array.map(decodeScalar);
      ofMatrix(Matrix.{numRows, numColumns, elements});
    | Percent(p) => `P(decodeScalar(p))
    | NaN => `N
    };
};

module Encoding = {
  type unitConversion = {
    fromUnits: array(TechniCalcCalculator.Unit_Types.unitPower),
    toUnits: array(TechniCalcCalculator.Unit_Types.unitPower),
  };
  type customAtom = {
    mml: string,
    value: Value_Encoding.encoding,
  };

  type t = {
    elements: string,
    unitConversions: array(unitConversion),
    customAtoms: array(customAtom),
    labels: array(string),
    variables: array(string),
  };

  let specialCharBias = 256;
  let specialCharBase = 32;

  // let%private appendTo = (mutableArray, value, specialCharIndex) => {
  //   let index = MutableArrayBuilder.length(mutableArray^);
  //   mutableArray := MutableArrayBuilder.append(mutableArray^, value);
  //   specialCharBias + specialCharIndex * specialCharBase + index;
  // };

  // let encode = (input: array(AST.t)): t => {
  //   let unitConversions = ref(MutableArrayBuilder.empty);
  //   let customAtoms = ref(MutableArrayBuilder.empty);
  //   let labels = ref(MutableArrayBuilder.empty);
  //   let variables = ref(MutableArrayBuilder.empty);

  //   let elements =
  //     input
  //     ->Belt.Array.mapU((. element) => {
  //         let code =
  //           switch (element) {
  //           | UnitConversion({fromUnits, toUnits}) =>
  //             appendTo(unitConversions, {fromUnits, toUnits}, 0)
  //           | CustomAtomS({mml, value}) =>
  //             let value =
  //               TechniCalcCalculator.Encoding.decode(value)
  //               ->Belt.Option.getWithDefault(
  //                   TechniCalcCalculator.Value_Base.nan,
  //                 )
  //               ->Value_Encoding.encode;
  //             appendTo(customAtoms, {mml, value}, 1);
  //           | LabelS({mml}) => appendTo(labels, mml, 3)
  //           | VariableS(string) => appendTo(variables, string, 2)
  //           | element => Encoding_Element.toInt(element)
  //           };

  //         Encoding_VarInt.encodeElement(code);
  //       })
  //     ->StringUtil.join;

  //   {
  //     elements,
  //     unitConversions: MutableArrayBuilder.toArray(unitConversions^),
  //     customAtoms: MutableArrayBuilder.toArray(customAtoms^),
  //     labels: MutableArrayBuilder.toArray(labels^),
  //     variables: MutableArrayBuilder.toArray(variables^),
  //   };
  // };

  let decode =
      ({elements, unitConversions, customAtoms, labels, variables}: t)
      : option(array(AST.t)) => {
    Encoding_VarInt.decodeU(elements, (. value) =>
      if (value < specialCharBias) {
        Encoding_Element.ofInt(value);
      } else {
        let specialCharType = (value - specialCharBias) / specialCharBase;
        let argumentIndex = (value - specialCharBias) mod specialCharBase;
        switch (specialCharType) {
        | 0 =>
          switch (Belt.Array.get(unitConversions, argumentIndex)) {
          | Some({fromUnits, toUnits}) =>
            Some(UnitConversion({fromUnits, toUnits}))
          | None => None
          }
        | 1 =>
          switch (Belt.Array.get(customAtoms, argumentIndex)) {
          | Some({mml, value}) =>
            let value =
              Value_Encoding.decode(value)
              ->TechniCalcCalculator.Value_Encoding.encode;
            Some(CustomAtomS({mml, value}));
          | None => None
          }
        | 3 =>
          switch (Belt.Array.get(labels, argumentIndex)) {
          | Some(mml) => Some(LabelS({mml: mml}))
          | None => None
          }
        | 2 =>
          switch (Belt.Array.get(variables, argumentIndex)) {
          | Some(string) => Some(VariableS(string))
          | None => None
          }
        | _ => None
        };
      }
    );
  };
};
