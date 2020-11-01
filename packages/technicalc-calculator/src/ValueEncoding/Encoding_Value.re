open Encoding;

let%private encodeConstant = (constant: Real_Constant.t) =>
  switch (constant) {
  | Unit => encodeUint(0)
  | Pi => encodeUint(1)
  | Sqrt(sqrt) => encodeUint(2) ++ encodeUint(sqrt)
  | Exp(exp) => encodeUint(3) ++ encodeUint(exp)
  };

let%private readConstant = reader => {
  switch (readUint(reader)) {
  | Some(0) => Some(Real_Constant.Unit)
  | Some(1) => Some(Pi)
  | Some(2) =>
    switch (readUint(reader)) {
    | Some(sqrt) => Some(Sqrt(sqrt))
    | None => None
    }
  | Some(3) =>
    switch (readUint(reader)) {
    | Some(exp) => Some(Exp(exp))
    | None => None
    }
  | _ => None
  };
};

let%private encodeReal = (real: Real_Types.t) =>
  switch (real) {
  | Rational(n, d, c) =>
    encodeUint(0) ++ encodeUint(n) ++ encodeUint(d) ++ encodeConstant(c)
  | Decimal(f) => encodeUint(1) ++ Decimal.toString(f)->encodeString
  };

let%private readReal = reader =>
  switch (readUint(reader)) {
  | Some(0) =>
    switch (readUint(reader), readUint(reader), readConstant(reader)) {
    | (Some(n), Some(d), Some(c)) => Some(Real_Base.ofRational(n, d, c))
    | _ => None
    }
  | Some(1) =>
    switch (readString(reader)) {
    | Some(string) =>
      let decimal = Decimal.ofString(string)->Real_Base.ofDecimal;
      Some(decimal);
    | _ => None
    }
  | _ => None
  };

let%private encodeScalar =
  (. scalar: Scalar_Types.t) =>
    switch (scalar) {
    | `Z => encodeUint(0)
    | `R(re) => encodeUint(1) ++ encodeReal(re)
    | `I(im) => encodeUint(2) ++ encodeReal(im)
    | `C(re, im) => encodeUint(3) ++ encodeReal(re) ++ encodeReal(im)
    };

let%private readScalar =
  (. reader) =>
    switch (readUint(reader)) {
    | Some(0) => Some(Scalar_Base.zero)
    | Some(1) =>
      switch (readReal(reader)) {
      | Some(real) => Some(`R(real))
      | None => None
      }
    | Some(2) =>
      switch (readReal(reader)) {
      | Some(imag) => Some(`I(imag))
      | None => None
      }
    | Some(3) =>
      switch (readReal(reader), readReal(reader)) {
      | (Some(real), Some(imag)) => Some(`C((real, imag)))
      | _ => None
      }
    | _ => None
    };

let encodeValue = (value: Value_Types.t) =>
  switch (value) {
  | #Scalar.t as scalar => encodeUint(0) ++ encodeScalar(. scalar)
  | `V(elements) => encodeUint(1) ++ encodeArray(elements, encodeScalar)
  | `M({numRows, numColumns, elements}) =>
    encodeUint(2)
    ++ encodeUint(numRows)
    ++ encodeUint(numColumns)
    ++ encodeArray(elements, encodeScalar)
  | `P(scalar) => encodeUint(3) ++ encodeScalar(. scalar)
  | `N => encodeUint(4)
  };

let readValue = (reader): option(Value_Types.t) =>
  switch (readUint(reader)) {
  | Some(0) =>
    switch (readScalar(. reader)) {
    | Some(scalar) => Some(Value_Base.ofScalar(scalar))
    | None => None
    }
  | Some(1) =>
    switch (readArray(reader, readScalar)) {
    | Some(elements) => Some(Value_Base.ofVector(elements))
    | None => None
    }
  | Some(2) =>
    switch (
      readUint(reader),
      readUint(reader),
      readArray(reader, readScalar),
    ) {
    | (Some(numRows), Some(numColumns), Some(elements)) =>
      let matrix =
        Matrix_Base.make(numRows, numColumns, elements)->Value_Base.ofMatrix;
      Some(matrix);
    | _ => None
    }
  | Some(3) =>
    switch (readScalar(. reader)) {
    | Some(scalar) => Some(Value_Base.ofPercent(scalar))
    | None => None
    }
  | Some(4) => Some(Value_Base.nan)
  | _ => None
  };
