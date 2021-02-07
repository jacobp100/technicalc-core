open Formatting_Types;

let%private f = (format, v) => {
  let (base, digitGrouping) =
    switch (format) {
    | Some(format) => (format.base, format.digitGrouping)
    | _ => (10, false)
    };
  Formatting_Number.formatInteger(~base, ~digitGrouping, Decimal.ofInt(v));
};

let toString = (~format, a) => {
  let mode =
    switch (format) {
    | Some(format) => Some(format.mode)
    | None => None
    };
  switch (mode, a) {
  | (_, Real_Constant.Unit) => ""
  | (Some(String) | None, Pi(1)) => "pi"
  | (Some(String) | None, Pi(v)) => "pi^" ++ f(format, v)
  | (Some(String) | None, Exp(v)) => "exp(" ++ f(format, v) ++ ")"
  | (Some(String) | None, Sqrt(v)) => "sqrt(" ++ f(format, v) ++ ")"
  | (Some(Unicode), Pi(1)) => Formatting_Unicode.pi
  | (Some(Unicode), Pi(v)) =>
    Formatting_Unicode.pi
    ++ f(format, v)->Formatting_Unicode.formatSuperscriptNumbers
  | (Some(Unicode), Exp(1)) => "e"
  | (Some(Unicode), Exp(v)) =>
    "e" ++ f(format, v)->Formatting_Unicode.formatSuperscriptNumbers
  | (Some(Unicode), Sqrt(v)) =>
    Formatting_Unicode.sqrt ++ "(" ++ f(format, v) ++ ")"
  | (Some(Tex), Pi(1)) => "\\pi"
  | (Some(Tex), Pi(v)) => "\\pi^{" ++ f(format, v) ++ "}"
  | (Some(Tex), Exp(1)) => "e"
  | (Some(Tex), Exp(v)) => "e^{" ++ f(format, v) ++ "}"
  | (Some(Tex), Sqrt(v)) => "\\sqrt{" ++ f(format, v) ++ "}"
  | (Some(MathML), Pi(1)) => "<mi>&#960;</mi>"
  | (Some(MathML), Pi(v)) =>
    "<msup><mi>&#960;</mi><mn>" ++ f(format, v) ++ "</mn></msup>"
  | (Some(MathML), Exp(1)) => "<mi>e</mi>"
  | (Some(MathML), Exp(v)) =>
    "<msup><mi>e</mi><mn>" ++ f(format, v) ++ "</mn></msup>"
  | (Some(MathML), Sqrt(v)) =>
    "<msqrt><mn>" ++ f(format, v) ++ "</mn></msqrt>"
  };
};
