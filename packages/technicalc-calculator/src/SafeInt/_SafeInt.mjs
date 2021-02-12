/* eslint-disable no-bitwise */

const nanInt = 1 << 31;

export const toInt = a => (a === nanInt ? undefined : a);

export const abs = a => {
  if (a === nanInt) {
    return nanInt;
  }

  const intVal = Math.abs(a) | 0;
  return intVal === Math.abs(a) ? intVal : nanInt;
};

export const neg = a => {
  if (a === nanInt) {
    return nanInt;
  }

  const intVal = -a | 0;
  return intVal === -a ? intVal : nanInt;
};

export const add = (a, b) => {
  if (a === nanInt || b === nanInt) {
    return nanInt;
  }

  const intVal = (a + b) | 0;
  return intVal === a + b ? intVal : nanInt;
};

export const sub = (a, b) => {
  if (a === nanInt || b === nanInt) {
    return nanInt;
  }

  const intVal = (a - b) | 0;
  return intVal === a - b ? intVal : nanInt;
};

export const mul = (a, b) => {
  if (a === nanInt || b === nanInt) {
    return nanInt;
  }

  const intVal = Math.imul(a, b);
  return intVal === a * b ? intVal : nanInt;
};

export const div = (a, b) => {
  if (a === nanInt || b === nanInt) {
    return nanInt;
  }

  if (b === 0) {
    return nanInt;
  }

  return (a / b) | 0;
};

export const pow = (a, b) => {
  if (a === nanInt || b === nanInt) {
    return nanInt;
  }

  if (a === 0 && b === 0) {
    return nanInt;
  }

  const intVal = (a ** b) | 0;
  return intVal === a ** b ? intVal : nanInt;
};

export const mod = (a, b) => {
  if (a === nanInt || b === nanInt) {
    return nanInt;
  }

  if (b === 0) {
    return nanInt;
  }

  return a % b | 0;
};
