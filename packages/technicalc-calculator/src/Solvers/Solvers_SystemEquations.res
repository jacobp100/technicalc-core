open Value

let var2 = (x0, y0, c0, x1, y1, c1) => {
  let denom = x0 * y1 - y0 * x1
  ((y1 * c0 - y0 * c1) / denom, (x0 * c1 - x1 * c0) / denom)
}

let var3 = (x0, y0, z0, c0, x1, y1, z1, c1, x2, y2, z2, c2) => {
  // https://www.wolframalpha.com/input/?i=inverse(%7B%7Ba,b,c%7D,%7Bd,e,f%7D,%7Bg,h,i%7D%7D)+*+%7B%7Bj%7D,%7Bk%7D,%7Bl%7D%7D
  let denom =
    -z0 * y1 * x2 + y0 * z1 * x2 + z0 * x1 * y2 - x0 * z1 * y2 - y0 * x1 * z2 + x0 * y1 * z2
  let p1 = (y1 * z2 - z1 * y2) * c0 + (z0 * y2 - y0 * z2) * c1 + (y0 * z1 - z0 * y1) * c2
  let p2 = (z1 * x2 - x1 * z2) * c0 + (x0 * z2 - z0 * x2) * c1 + (z0 * x1 - x0 * z1) * c2
  let p3 = (x1 * y2 - y1 * x2) * c0 + (y0 * x2 - x0 * y2) * c1 + (x0 * y1 - y0 * x1) * c2
  (p1 / denom, p2 / denom, p3 / denom)
}
