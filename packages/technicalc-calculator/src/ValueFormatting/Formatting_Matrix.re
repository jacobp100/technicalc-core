type matrixFormat = {
  matrixOpen: string,
  matrixClose: string,
  rowOpen: string,
  rowClose: string,
  rowSeparator: string,
  elementSeparator: string,
  compactVectorFormat: bool,
};

let formatString = {
  matrixOpen: "{",
  matrixClose: "}",
  rowOpen: "{",
  rowClose: "}",
  rowSeparator: ",",
  elementSeparator: ",",
  compactVectorFormat: true,
};

let formatTex = {
  matrixOpen: "\\begin{bmatrix}\n",
  matrixClose: "\n\\end{bmatrix}",
  rowOpen: "",
  rowClose: "",
  rowSeparator: "\\\\\n",
  elementSeparator: " && ",
  compactVectorFormat: false,
};

let formatMathML = {
  matrixOpen: "<mrow><mo>[</mo><mtable>",
  matrixClose: "</mtable><mo>]</mo></mrow>",
  rowOpen: "<mtr><mtd>",
  rowClose: "</mtd></mtr>",
  rowSeparator: "",
  elementSeparator: "</mtd><mtd>",
  compactVectorFormat: false,
};

let toString = (~format, matrix: Matrix.t, tableFormat) => {
  let out = ref(tableFormat.matrixOpen);
  let skipRowBoundary =
    matrix.numColumns == 1 && tableFormat.compactVectorFormat;

  for (row in 0 to matrix.numRows - 1) {
    if (row != 0) {
      out := out^ ++ tableFormat.rowSeparator;
    };

    if (!skipRowBoundary) {
      out := out^ ++ tableFormat.rowOpen;
    };

    for (column in 0 to matrix.numColumns - 1) {
      if (column != 0) {
        out := out^ ++ tableFormat.elementSeparator;
      };

      let element =
        Matrix.getExn(matrix, ~row, ~column)
        ->Formatting_Scalar.toString(~format?, _);
      out := out^ ++ element;
    };

    if (!skipRowBoundary) {
      out := out^ ++ tableFormat.rowClose;
    };
  };

  out := out^ ++ tableFormat.matrixClose;

  out^;
};
