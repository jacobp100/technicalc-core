let prependMany = (list, count, value) => {
  let out = ref(list)
  for _ in 1 to count {
    out := list{value, ...out.contents}
  }
  out.contents
}
