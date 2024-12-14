let mml = (~metadata=?, ~attributes=list{}, tag, body) => {
  let attributes =
    attributes
    ->Belt.List.toArray
    ->Belt.Array.keepMap(attribute => Mml_Attributes.toString(~metadata?, attribute))
    ->StringUtil.joinWith(" ")

  let elementWithAttributes = attributes != "" ? tag ++ " " ++ attributes : tag

  `<${elementWithAttributes}>${body}</${tag}>`
}
