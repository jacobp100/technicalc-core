open Matrix_Types;
open Matrix_Base;

let mulVector = (m: t, v: Vector.t): Vector.t => {
  let size = Vector.size(v);

  if (m.numColumns == size) {
    Vector.makeByU(
      size,
      (. row) => {
        let element: ref(Scalar.t) = ref(`Z);
        for (column in 0 to size - 1) {
          let elementProduct =
            Scalar.mul(getExn(m, ~row, ~column), Vector.getExn(v, column));
          element := Scalar.add(element^, elementProduct);
        };
        element^;
      },
    );
  } else {
    Vector.empty;
  };
};
