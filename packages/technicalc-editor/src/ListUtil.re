let takeUpto = (elements, n) =>
  elements->Belt.List.take(n)->Belt.Option.getWithDefault(elements);

let prependMany = (list, count, value) => {
  let out = ref(list);
  for (_ in 1 to count) {
    out := [value, ...out^];
  };
  out^;
};
