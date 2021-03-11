/* eslint-disable no-bitwise */

export const abs = (a) => {
  if (a === undefined) {
    return undefined;
  }

  const intVal = Math.abs(a) | 0;
  return intVal === Math.abs(a) ? intVal : undefined;
};

export const neg = (a) => {
  if (a === undefined) {
    return undefined;
  }

  const intVal = -a | 0;
  return intVal === -a ? intVal : undefined;
};

export const add = (a, b) => {
  if (a === undefined || b === undefined) {
    return undefined;
  }

  const intVal = (a + b) | 0;
  return intVal === a + b ? intVal : undefined;
};

export const sub = (a, b) => {
  if (a === undefined || b === undefined) {
    return undefined;
  }

  const intVal = (a - b) | 0;
  return intVal === a - b ? intVal : undefined;
};

export const mul = (a, b) => {
  if (a === undefined || b === undefined) {
    return undefined;
  }

  const intVal = Math.imul(a, b);
  return intVal === a * b ? intVal : undefined;
};

export const div = (a, b) => {
  if (a === undefined || b === undefined) {
    return undefined;
  }

  if (b === 0) {
    return undefined;
  }

  return (a / b) | 0;
};

export const pow = (a, b) => {
  if (a === undefined || b === undefined) {
    return undefined;
  }

  if (a === 0 && b === 0) {
    return undefined;
  }

  const intVal = (a ** b) | 0;
  return intVal === a ** b ? intVal : undefined;
};

export const mod = (a, b) => {
  if (a === undefined || b === undefined) {
    return undefined;
  }

  if (b === 0) {
    return undefined;
  }

  return a % b | 0;
};
