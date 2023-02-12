open UrlSafeEncoding

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
    | #Zero => encodeUint(0)
    | #Real(re) => encodeUint(1) ++ encodeReal(re)
    | #Imag(im) => encodeUint(2) ++ encodeReal(im)
    | #Cmpx(re, im) => encodeUint(3) ++ encodeReal(re) ++ encodeReal(im)
    | #NaNN => encodeUint(4)
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

%%private(external scalarElements: array<Scalar.Finite.t> => array<Scalar.t> = "%identity")

let encode = (value: Value_Types.t) =>
  switch value {
  | #...Scalar.t as scalar => encodeUint(0) ++ encodeScalar(scalar)
  | #Vect(elements) => encodeUint(1) ++ scalarElements(elements)->encodeArray(_, encodeScalar)
  | #Matx({numRows, numColumns, elements}) =>
    encodeUint(2) ++
    encodeUint(numRows) ++
    encodeUint(numColumns) ++
    scalarElements(elements)->encodeArray(_, encodeScalar)
  | #Pcnt(scalar) => encodeUint(3) ++ Scalar_Finite.toScalar(scalar)->encodeScalar
  | #Mesr({value, units}) => encodeUint(5) ++ encodeReal(value) ++ Encoding_Units.encodeUnits(units)
  }

let read = (reader): option<Value_Types.t> =>
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
      Some(Matrix_Base.make(~numRows, ~numColumns, elements)->Value_Base.ofMatrix)
    | _ => None
    }
  | Some(3) =>
    switch readScalar(reader) {
    | Some(scalar) => Some(Value_Base.ofPercent(scalar))
    | None => None
    }
  | Some(5) =>
    switch (readReal(reader), Encoding_Units.readUnits(reader)) {
    | (Some(value), Some(units)) => Some(Measure_Base.ofReal(value, ~units)->Value_Base.ofMeasure)
    | _ => None
    }
  // Old encoding
  | Some(4) => Some(Value_Base.nan)
  | _ => None
  }
