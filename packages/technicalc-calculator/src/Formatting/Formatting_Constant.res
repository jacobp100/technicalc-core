open Formatting_Types

%%private(
  let f = (~format, v) => {
    let {decimalSeparator, groupingSeparator, base, digitGrouping} = format
    Formatting_Number.formatInteger(
      ~decimalSeparator,
      ~groupingSeparator,
      ~base,
      ~digitGrouping,
      Decimal.ofInt(v),
    )
  }
)

let toString = (~format, a) => {
  switch (format.mode, a) {
  | (_, Real_Constant.Unit) => ""
  | (Ascii, Pi(1)) => "pi"
  | (Ascii, Pi(v)) => `pi^${f(~format, v)}`
  | (Ascii, Exp(v)) => `exp(${f(~format, v)})`
  | (Ascii, Sqrt(v)) => `sqrt(${f(~format, v)})`
  | (Unicode, Pi(1)) => Formatting_Unicode.pi
  | (Unicode, Pi(v)) =>
    Formatting_Unicode.pi ++ f(~format, v)->Formatting_Unicode.formatSuperscriptNumbers
  | (Unicode, Exp(1)) => "e"
  | (Unicode, Exp(v)) => "e" ++ f(~format, v)->Formatting_Unicode.formatSuperscriptNumbers
  | (Unicode, Sqrt(v)) => `${Formatting_Unicode.sqrt}(${f(~format, v)})`
  | (Tex, Pi(1)) => "\\pi"
  | (Tex, Pi(v)) => `${"\\"}pi^{${f(~format, v)}}`
  | (Tex, Exp(1)) => "e"
  | (Tex, Exp(v)) => `e^{${f(~format, v)}}`
  | (Tex, Sqrt(v)) => `${"\\"}sqrt{${f(~format, v)}}`
  | (MathML, Pi(1)) => "<mi>&#960;</mi>"
  | (MathML, Pi(v)) => `<msup><mi>&#960;</mi><mn>${f(~format, v)}</mn></msup>`
  | (MathML, Exp(1)) => "<mi>e</mi>"
  | (MathML, Exp(v)) => `<msup><mi>e</mi><mn>${f(~format, v)}</mn></msup>`
  | (MathML, Sqrt(v)) => `<msqrt><mn>${f(~format, v)}</mn></msqrt>`
  }
}
