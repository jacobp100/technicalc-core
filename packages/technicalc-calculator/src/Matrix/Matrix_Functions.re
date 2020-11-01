open Matrix_Types;
open Matrix_Base;
open Matrix_Arithmetic;

let neg = (m: t) => map(m, Scalar.neg);

let pow = (m: t, i: int): t =>
  if (i >= 0 && m.numColumns == m.numRows) {
    let x = ref(identity(m.numRows));
    for (_ in 0 to i - 1) {
      x := mul(x^, m);
    };
    x^;
  } else {
    empty;
  };

let determinant = (m: t): Scalar.t =>
  switch (m.elements) {
  | [|a, b, c, d|] => Scalar.(a * d - b * c)
  | [|a, b, c, d, e, f, g, h, i|] =>
    /* https://www.wolframalpha.com/input/?i=det(%7B%7Ba,b,c%7D,%7Bd,e,f%7D,%7Bg,h,i%7D%7D) */
    Scalar.(
      a * e * i - a * f * h - b * d * i + b * f * g + c * d * h - c * e * g
    )
  | _ => Scalar.nan
  };

let inverse = (m: t): t =>
  switch (m.elements) {
  | [|a, b, c, d|] =>
    open Scalar;
    let factor = a * d - b * c;
    let elements = [|d / factor, - b / factor, - c / factor, a / factor|];
    {...m, elements};
  | [|a, b, c, d, e, f, g, h, i|] =>
    /* https://www.wolframalpha.com/input/?i=%7B%7Ba,b,c%7D,%7Bd,e,f%7D,%7Bg,h,i%7D%7D%5E-1 */

    open Scalar;
    let factor =
      a * e * i - a * f * h - b * d * i + b * f * g + c * d * h - c * e * g;
    let elements = [|
      (e * i - f * h) / factor,
      (c * h - b * i) / factor,
      (b * f - c * e) / factor,
      (f * g - d * i) / factor,
      (a * i - c * g) / factor,
      (c * d - a * f) / factor,
      (d * h - e * g) / factor,
      (b * g - a * h) / factor,
      (a * e - b * d) / factor,
    |];
    {...m, elements};
  | _ => empty
  };
