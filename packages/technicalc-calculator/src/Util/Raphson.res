let epsilon = 1e-8

let cubic = (a, b, c, d, start) => {
  let x = ref(start)
  let i = ref(0)
  let maxI = 20

  while i.contents < maxI {
    let fx = a *. x.contents ** 3. +. b *. x.contents ** 2. +. c *. x.contents +. d
    let f'x = 3. *. a *. x.contents ** 2. +. 2. *. b *. x.contents +. c
    let dx = fx != 0. && f'x != 0. ? fx /. f'x : 0.

    if FloatUtil.abs(dx) < epsilon {
      i := maxI
    } else {
      x := x.contents -. fx /. f'x
      i := i.contents + 1
    }
  }

  x.contents
}
