const escapeRe = /&#(x?)([0-9A-F]+);/gi;

const escapeCharCode = (_fullMatch, x, charCode) => {
  const n = parseInt(charCode, x.length !== 0 ? 16 : 10);

  if (n < 0x10000) {
    return String.fromCharCode(n);
  }
  //
  // Use surrogate pair for values outside the BMP0
  //
  n -= 0x10000;
  const hi = (n >> 10) + 0xd800;
  const lo = (n & 0x3ff) + 0xdc00;
  return String.fromCharCode(hi, lo);
};

export const translate = (text) => text.replace(escapeRe, escapeCharCode);
