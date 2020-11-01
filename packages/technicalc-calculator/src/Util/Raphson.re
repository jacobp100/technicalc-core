let epsilon = 1e-8;

let cubic = (a, b, c, d, start) => {
  let x = ref(start);
  let i = ref(0);
  let maxI = 20;

  while (i^ < maxI) {
    let fx = a *. x^ ** 3. +. b *. x^ ** 2. +. c *. x^ +. d;
    let f'x = 3. *. a *. x^ ** 2. +. 2. *. b *. x^ +. c;
    let dx = fx != 0. && f'x != 0. ? fx /. f'x : 0.;

    if (abs_float(dx) < epsilon) {
      i := maxI;
    } else {
      x := x^ -. fx /. f'x;
      i := i^ + 1;
    };
  };

  x^;
};