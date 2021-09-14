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
    | Rational(n, d, c) => encodeUint(0) ++ encodeInt(n) ++ encodeInt(d) ++ encodeConstant(c)
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
  let encodeScalar = (scalar: Scalar_Types.t) =>
    switch scalar {
    | #Z => encodeUint(0)
    | #R(re) => encodeUint(1) ++ encodeReal(re)
    | #I(im) => encodeUint(2) ++ encodeReal(im)
    | #C(re, im) => encodeUint(3) ++ encodeReal(re) ++ encodeReal(im)
    | #N => encodeUint(4)
    }
)

%%private(
  let readScalar = reader =>
    switch readUint(reader) {
    | Some(0) => Some(Scalar_Base.zero)
    | Some(1) =>
      switch readReal(reader) {
      | Some(real) => Some(Scalar_Base.ofReal(real))
      | None => None
      }
    | Some(2) =>
      switch readReal(reader) {
      | Some(imag) => Some(Scalar_Base.ofImag(imag))
      | None => None
      }
    | Some(3) =>
      switch (readReal(reader), readReal(reader)) {
      | (Some(real), Some(imag)) => Some(Scalar_Base.ofComplex(real, imag))
      | _ => None
      }
    | Some(4) => Some(Scalar_Base.nan)
    | _ => None
    }
)

%%private(let encodeFinite: Scalar.finite => string = Obj.magic(encodeScalar))

let encodeValue = (value: Value_Types.t) =>
  switch value {
  | #...Scalar.t as scalar => encodeUint(0) ++ encodeScalar(scalar)
  | #V(elements) => encodeUint(1) ++ encodeArray(elements, encodeFinite)
  | #M({numRows, numColumns, elements}) =>
    encodeUint(2) ++
    encodeUint(numRows) ++
    encodeUint(numColumns) ++
    encodeArray(elements, encodeFinite)
  | #P(scalar) => encodeUint(3) ++ encodeFinite(scalar)
  }

let readValue = (reader): option<Value_Types.t> =>
  switch readUint(reader) {
  | Some(0) =>
    switch readScalar(reader) {
    | Some(scalar) => Some(Value_Base.ofScalar(scalar))
    | None => None
    }
  | Some(1) =>
    switch readArray(reader, readScalar) {
    | Some(elements) => Some(Vector_Base.make(elements)->Value_Base.ofVector)
    | None => None
    }
  | Some(2) =>
    switch (readUint(reader), readUint(reader), readArray(reader, readScalar)) {
    | (Some(numRows), Some(numColumns), Some(elements)) =>
      let matrix = Matrix_Base.make(~numRows, ~numColumns, elements)->Value_Base.ofMatrix
      Some(matrix)
    | _ => None
    }
  | Some(3) =>
    switch readScalar(reader) {
    | Some(scalar) => Some(Value_Base.ofPercent(scalar))
    | None => None
    }
  // Old encoding
  | Some(4) => Some(Value_Base.nan)
  | _ => None
  }
