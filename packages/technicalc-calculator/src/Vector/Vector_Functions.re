open Vector_Types;
open Vector_Base;

let neg = (a: t): t => map(a, Scalar.neg);

let magnitudeSquared = (a: t): Scalar.t =>
  Belt.Array.reduceU(a, Scalar.zero, (. a, s) => Scalar.(a + s * s));

let dot = (a: t, b: t): Scalar.t =>
  if (Belt.Array.length(a) == Belt.Array.length(b)) {
    let out = ref(Scalar.zero);

    for (i in 0 to Belt.Array.length(a) - 1) {
      Js.log("Hello");
      out :=
        Scalar.(
          out^ + Belt.Array.getUnsafe(a, i) * Belt.Array.getUnsafe(b, i)
        );
    };

    out^;
  } else {
    Scalar.nan;
  };
