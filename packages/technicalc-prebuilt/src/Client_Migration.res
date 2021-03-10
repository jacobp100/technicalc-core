module TechniCalcCalculator = {
  open TechniCalcCalculator

  module StringUtil = {
    @scope("String") @val
    external ofChar: int => string = "fromCharCode"
    @send external stringCharAtUnsafe: (string, int) => string = "charAt"
    @send external charAtUnsafe: (string, int) => int = "charCodeAt"
    @send
    external join: (array<string>, @as("") _) => string = "join"
    @send external joinWith: (array<string>, string) => string = "join"
    @send
    external split: (string, ~separator: string) => array<string> = "split"
    @send
    external replaceFirst: (string, string, string) => string = "replace"
    @send external slice: (string, int, int) => string = "slice"
  }

  module Encoding = {
    /*
     URL-safe encoding to string

     Also used directly in technicalc-editor
 */

    type reader = {
      string: string,
      mutable index: option<int>,
    }

    %%private(
      let readInvalid = reader => {
        reader.index = None
        None
      }
    )

    let read = (string, decoder) => {
      let reader = {string: string, index: Some(0)}
      let value = decoder(reader)

      let isComplete = switch reader.index {
      | Some(index) => index === String.length(string)
      | None => false
      }

      isComplete ? value : None
    }

    /* Character set from URL-safe base64 variant */
    %%private(let characters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_")

    %%private(
      let charToIndex = (character: int) => {
        let character: char = Obj.magic(character)
        switch character {
        | 'a' .. 'z' => Char.code(character) - Char.code('a')
        | 'A' .. 'Z' => Char.code(character) - Char.code('A') + 26
        | '0' .. '9' => Char.code(character) - Char.code('0') + 52
        | '-' => 62
        | '_' => 63
        | _ => assert false
        }
      }
    )

    let encodeUint = value => {
      assert (value >= 0)

      let rec iter = (~current="", ~flag=false, value) => {
        let power = value / 32
        let char = StringUtil.stringCharAtUnsafe(characters, mod(value, 32) + (flag ? 32 : 0))
        let current = char ++ current
        if power == 0 {
          current
        } else {
          iter(~current, ~flag=true, power)
        }
      }
      iter(value)
    }

    let readUint = ({string, index} as reader): option<int> => {
      let rec iter = (~accum=0, index) =>
        if index < String.length(string) {
          let value = StringUtil.charAtUnsafe(string, index)->charToIndex

          if value >= 32 {
            iter(~accum=(accum + mod(value, 32)) * 32, index + 1)
          } else {
            reader.index = Some(index + 1)
            Some(accum + value)
          }
        } else {
          readInvalid(reader)
        }

      switch index {
      | Some(index) => iter(index)
      | None => None
      }
    }

    let encodeInt = value => {
      let value = lor(lsl(IntUtil.abs(value), 1), value < 0 ? 1 : 0)
      encodeUint(value)
    }

    let readInt = reader =>
      switch readUint(reader) {
      | Some(value) => Some(lsr(value, 1) * (land(value, 1) !== 0 ? -1 : 1))
      | None => None
      }

    let encodeArray = (array, encoder) =>
      encodeUint(Belt.Array.length(array)) ++ Belt.Array.mapU(array, encoder)->StringUtil.join

    let readArray = (reader, decoder) =>
      switch readUint(reader) {
      | Some(length) =>
        let out = Belt.Array.makeUninitializedUnsafe(length)

        let rec iter = i =>
          if i < length {
            switch decoder(. reader) {
            | Some(value) =>
              Belt.Array.setExn(out, i, value)
              iter(i + 1)
            | None => readInvalid(reader)
            }
          } else {
            Some(out)
          }

        iter(0)
      | _ => readInvalid(reader)
      }

    type stringOptimisation =
      | Text
      | Numbers

    %%private(
      let charXor = x =>
        switch x {
        | Text => 0b1100100
        | Numbers => 0b0100000
        }
    )

    let encodeString = (~optimizeFor=Text, string) => {
      let charXor = charXor(optimizeFor)
      Belt.Array.makeByU(String.length(string), (. i) =>
        lxor(StringUtil.charAtUnsafe(string, i), charXor)
      )->encodeArray((. value) => encodeUint(value))
    }

    let readString = (~optimizeFor=Text, reader) => {
      let charXor = charXor(optimizeFor)
      let readChar = (. reader) =>
        switch readUint(reader) {
        | Some(char) => Some(StringUtil.ofChar(lxor(char, charXor)))
        | None => readInvalid(reader)
        }

      switch readArray(reader, readChar) {
      | Some(charArray) => Some(StringUtil.join(charArray))
      | None => readInvalid(reader)
      }
    }
  }

  module Encoding_Value = {
    open Encoding

    %%private(
      let encodeConstant = (constant: Real_Constant.t) =>
        switch constant {
        | Unit => encodeUint(0)
        | Pi(exp) => encodeUint(4) ++ encodeInt(exp)
        | Exp(exp) => encodeUint(3) ++ encodeInt(exp)
        | Sqrt(sqrt) => encodeUint(2) ++ encodeInt(sqrt)
        }
    )

    %%private(
      let readConstant = reader =>
        switch readUint(reader) {
        | Some(0) => Some(Real_Constant.Unit)
        | Some(1) => Some(Pi(1))
        | Some(4) =>
          switch readInt(reader) {
          | Some(exp) => Some(Pi(exp))
          | None => None
          }
        | Some(3) =>
          switch readInt(reader) {
          | Some(exp) => Some(Exp(exp))
          | None => None
          }
        | Some(2) =>
          switch readInt(reader) {
          | Some(sqrt) => Some(Sqrt(sqrt))
          | None => None
          }
        | _ => None
        }
    )

    %%private(
      let encodeReal = (real: Real_Types.t) =>
        switch real {
        | Rational(n, d, c) =>
          encodeUint(0) ++ (encodeInt(n) ++ (encodeInt(d) ++ encodeConstant(c)))
        | Decimal(f) => encodeUint(1) ++ Decimal.toString(f)->encodeString(~optimizeFor=Numbers, _)
        }
    )

    %%private(
      let readReal = reader =>
        switch readUint(reader) {
        | Some(0) =>
          switch (readInt(reader), readInt(reader), readConstant(reader)) {
          | (Some(n), Some(d), Some(c)) => Some(Real_Base.ofRational(n, d, c))
          | _ => None
          }
        | Some(1) =>
          switch readString(~optimizeFor=Numbers, reader) {
          | Some(string) =>
            let decimal = Decimal.ofString(string)->Real_Base.ofDecimal
            Some(decimal)
          | _ => None
          }
        | _ => None
        }
    )

    %%private(
      let encodeScalar = (. scalar: Scalar_Types.t) =>
        switch scalar {
        | #Z => encodeUint(0)
        | #R(re) => encodeUint(1) ++ encodeReal(re)
        | #I(im) => encodeUint(2) ++ encodeReal(im)
        | #C(re, im) => encodeUint(3) ++ (encodeReal(re) ++ encodeReal(im))
        }
    )

    %%private(
      let readScalar = (. reader) =>
        switch readUint(reader) {
        | Some(0) => Some(Scalar_Base.zero)
        | Some(1) =>
          switch readReal(reader) {
          | Some(real) => Some(#R(real))
          | None => None
          }
        | Some(2) =>
          switch readReal(reader) {
          | Some(imag) => Some(#I(imag))
          | None => None
          }
        | Some(3) =>
          switch (readReal(reader), readReal(reader)) {
          | (Some(real), Some(imag)) => Some(#C(real, imag))
          | _ => None
          }
        | _ => None
        }
    )

    let encodeValue = (value: Value_Types.t) =>
      switch value {
      | #...Scalar.t as scalar => encodeUint(0) ++ encodeScalar(. scalar)
      | #V(elements) => encodeUint(1) ++ encodeArray(elements, encodeScalar)
      | #M({numRows, numColumns, elements}) =>
        encodeUint(2) ++
        (encodeUint(numRows) ++
        (encodeUint(numColumns) ++ encodeArray(elements, encodeScalar)))
      | #P(scalar) => encodeUint(3) ++ encodeScalar(. scalar)
      | #N => encodeUint(4)
      }

    let readValue = (reader): option<Value_Types.t> =>
      switch readUint(reader) {
      | Some(0) =>
        switch readScalar(. reader) {
        | Some(scalar) => Some(Value_Base.ofScalar(scalar))
        | None => None
        }
      | Some(1) =>
        switch readArray(reader, readScalar) {
        | Some(elements) => Some(Value_Base.ofVector(elements))
        | None => None
        }
      | Some(2) =>
        switch (readUint(reader), readUint(reader), readArray(reader, readScalar)) {
        | (Some(numRows), Some(numColumns), Some(elements)) =>
          let matrix = Matrix_Base.make(numRows, numColumns, elements)->Value_Base.ofMatrix
          Some(matrix)
        | _ => None
        }
      | Some(3) =>
        switch readScalar(. reader) {
        | Some(scalar) => Some(Value_Base.ofPercent(scalar))
        | None => None
        }
      | Some(4) => Some(Value_Base.nan)
      | _ => None
      }
  }

  module Value_Encoding = {
    let encode = Encoding_Value.encodeValue

    let decode = encoded => Encoding.read(encoded, Encoding_Value.readValue)
  }
}

module TechniCalcEditor = {
  open TechniCalcEditor

  module Encoding_Element = {
    open AST

    let toUint = (element: t): int =>
      Belt.Array.getExn(Encoding_ElementMap_Eval.mapping, Obj.magic(element))
    let ofUint = (index: int): option<t> =>
      Belt.Array.get(Encoding_ElementMap_Eval.reverseMapping, index)
  }

  module Encoding_Elements = {
    open TechniCalcCalculator.Encoding

    // let%private encodeUnitConversion = (~fromUnits, ~toUnits) =>
    //   Encoding_Units.encodeUnitParts(fromUnits)
    //   ++ Encoding_Units.encodeUnitParts(toUnits);

    // let%private readUnitConversion = reader =>
    //   switch (
    //     Encoding_Units.readUnitParts(reader),
    //     Encoding_Units.readUnitParts(reader),
    //   ) {
    //   | (Some(fromUnits), Some(toUnits)) =>
    //     Some(AST.UnitConversion({fromUnits, toUnits}))
    //   | _ => None
    //   };

    %%private(let encodeCustomAtom = (~mml, ~value) => encodeString(mml) ++ value)

    %%private(
      let readCustomAtom = reader =>
        switch (readString(reader), TechniCalcCalculator.Encoding_Value.readValue(reader)) {
        | (Some(mml), Some(value)) =>
          let value = TechniCalcCalculator.Encoding_Value.encodeValue(value)
          AST.CustomAtomS({mml: mml, value: value})->Some
        | _ => None
        }
    )

    %%private(
      let encodeElement = (. element: AST.t) =>
        switch element {
        | UnitConversion(_) => ""
        | CustomAtomS({mml, value}) => encodeUint(257) ++ encodeCustomAtom(~mml, ~value)
        | VariableS({id, name}) => encodeUint(261) ++ (encodeString(id) ++ encodeString(name))
        | CaptureGroupStart({placeholderMml}) => encodeUint(260) ++ encodeString(placeholderMml)
        | element => Encoding_Element.toUint(element)->encodeUint
        }
    )

    // Required to handle legacy LabelS encoding
    %%private(
      let removeMeWrapInArray = (x: option<AST.t>): option<array<AST.t>> =>
        switch x {
        | Some(x) => Some([x])
        | None => None
        }
    )

    %%private(
      let removeMeConcatArray = (x: option<array<array<AST.t>>>): option<array<AST.t>> =>
        switch x {
        | Some(flattenMe) => Some(Belt.Array.concatMany(flattenMe))
        | None => None
        }
    )

    %%private(
      let readElement = (. reader) =>
        switch readUint(reader) {
        | Some(256) => None
        | Some(257) => readCustomAtom(reader)->removeMeWrapInArray
        | Some(258) =>
          switch readString(reader) {
          | Some(placeholderMml) =>
            Some([CaptureGroupStart({placeholderMml: placeholderMml}), CaptureGroupEndS])
          | None => None
          }

        | Some(259) =>
          removeMeWrapInArray(
            switch readString(reader) {
            | Some("x") => Some(IteratorXS)
            | Some("Ans") => Some(VariableS({id: "Ans", name: "Ans"}))
            | _ => None
            },
          )
        | Some(261) =>
          removeMeWrapInArray(
            switch (readString(reader), readString(reader)) {
            | (Some(id), Some(name)) => Some(VariableS({id: id, name: name}))
            | _ => None
            },
          )
        | Some(260) =>
          removeMeWrapInArray(
            switch readString(reader) {
            | Some(placeholderMml) => Some(CaptureGroupStart({placeholderMml: placeholderMml}))
            | None => None
            },
          )
        | Some(value) => Encoding_Element.ofUint(value)->removeMeWrapInArray
        | None => None
        }
    )

    let encodeElements = (input: array<AST.t>) => encodeArray(input, encodeElement)

    let readElements = reader => readArray(reader, readElement)->removeMeConcatArray
  }

  module Encoding = {
    open TechniCalcCalculator.Encoding

    let encode = input => Encoding_Elements.encodeElements(input)

    let decode = string => read(string, Encoding_Elements.readElements)
  }
}
