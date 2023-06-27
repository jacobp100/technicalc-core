# TechniCalc Editor

Represents a math AST that can be converted to both MathML and an AST for TechniCalc Calculator.

## AST Representation

The main aim of the editor is to make editing maths equations as natural as possible. I copy how most word processors (like word) do it - you have a cursor and you can use left and right to scroll through the text. When you have a superscript, the cursor gets smaller and moves upwards.

Functions - like `sin` - act like a single character. You can select directly before the function, or directly after, but you can't edit inside it. Fractions work similar to superscripts. If your cursor is directly before the fraction, pressing right will move you to the numerator, then when you're at the end of the numerator, pressing right will take you to the denominator. When you're at the end of the denominator and press right, you move to directly after the fraction. Square roots act in a similar way.

Because of this reason, the primary AST is represented as a a flat array of "elements". An element can be anything from a digit, to an operator, to a function like `sin`. In text input analogy, each element is like a character. The internal representation of this is a variant. Almost variants don't have arguments; however, some special elements require them - for example, there is a custom element with a custom value and MathML representation.

In reality, we quickly have to break this 'just a text input' analogy. For example, a fraction has a numerator and denominator, which are both editable, and affect both rendering and parsing. Elements that act in this way accept "element arguments". These completely separate from the arguments of the variant types, and we'll get into more detail later.

As well as element arguments, optional superscripts are handled in a special way too.

There is a strict naming convention with element. We start with the element name. If the element takes element arguments, the number of arguments it takes is added as a suffix (as an integer). If they accept an optional superscript, the `S` suffix is added (after any element argument suffix).

- `SomeElement` - No element arguments, no superscript
- `SomeElement1` - 1 element argument, no superscript
- `SomeElement2` - 2 element arguments, no superscript
- `SomeElementS` - No element arguments, has an optional superscript
- `SomeElement2S` - 2 element arguments, has an optional superscript
- `SomeElementNS` - dynamic number of element arguments, has an optional superscript
- `N0_S` - The digit '0', which accepts no element arguments, has an optional superscript (underscore added for clarity)
- `CustomAtomS({value, mml: string})` - A custom atom, which accepts no element arguments, has an optional superscript

Putting this all together, and going back to our 'just a text input' analogy, we have can make the following inputs.

```reason
/* 1 + 2 */
[|N1_S, Add, N2_S|];

/* 1 + 2 * 3 */
[|N1_S, Add, N2_S, Mul, N3_S|];

/* sin 45 degrees */
[|Sin, N4_S, N5_S, Degree|];
```

It's worth highlighting that operator precedence isn't encoded here.

This format was picked because it makes editing easy, and it's relatively sound.

### Element Arguments

Element arguments alter the way you input expressions for the element. For example, a fraction has a numerator and denominator placed out of line. This element has two element arguments and accepts a superscript, so is represented as `Frac2S`.

Here, we need to introduce a special element type, `Arg`. This is purely semantical, and is used to indicate end of one argument. It does not render anything.

Every element that accepts element arguments must be preceded at some point by an amount of `Arg` elements equal to the number of element arguments accepted. For example, the fraction must be preceded by two `Arg` elements.

There is an analogy to function calls here - if you had a call to `frac(num, den)`, the function name **and** the opening bracket combined (`frac(`) are represented as `Frac2S`, and the commas closing bracket are both use same representation: `Arg`. Putting this together, we get,

```reason
/* Empty fraction */
[|Frac2S, Arg, Arg|];

/* Fraction of one half */
[|Frac2S, N1_S, Arg, N2_S, Arg|];

/* random integer between 1 and 10 */
[|RandInt2S, N1_S, Arg, N1_S, N0_S, Arg|];

/* Empty 2x2 (4 element) matrix */
[|Matrix4S, Arg, Arg, Arg, Arg|];
```

It is possible to nest elements accepting arguments. An `Arg` element corresponds to the most recent element accepting element arguments, until it has received all its arguments. Then it goes to the second most recent, and so forth.

```reason
/* Fraction of one over another fraction of a half */
[|Frac2S, N1_S, Arg, Frac2S, N1_S, Arg, N2_S, Arg, Arg|];

/* Fraction of a random integer between 1 and 10, all over 2 */
[|Frac2S, RandInt2S, N1_S, Arg, N1_S, N0_S, Arg, Arg, N2_S, Arg|];
```

This does lead to it being possible to represent invalid ASTs, although this should not normally occur. In these cases, extraneous `Arg` elements are dropped, and missing ones are appended to the end.

### Superscripts

There is additionally a superscript element, `Superscript1`, which accepts one argument. This is a regular element, and - when isollated - is rendered as a placeholder square with a superscript.

&#x25a1;<sup>&#x25a1;</sup>

However, in the case we are converting to MathML or a TechniCalc Calculator AST, and the superscript immediately precedes an element that accepts a superscript (the element is suffixed with an `S`), they are merged together, much like ligatures in fonts. If an element does not accept a superscript, it is left unaltered.

1 &#x25a1;<sup>&#x25a1;</sup> &#x2192; 1<sup>&#x25a1;</sup>

1 &#x25a1;<sup>2</sup> &#x2192; 1<sup>2</sup>

log &#x25a1;<sup>&#x25a1;</sup> &#x2192; log &#x25a1;<sup>&#x25a1;</sup> (non-trig functions don't accept superscripts; no change)

```reason
/* Fraction of a half with an empty superscript */
[|Frac2S, N1_S, Arg, N2_S, Arg, Superscript1, Arg|];

/* Fraction of a half raised to the power of 3 */
[|Frac2S, N1_S, Arg, N2_S, Arg, Superscript1, N3_S, Arg|];
```

### Mutation

Insertion and deletion of elements happen directly on the array. There is no preprocessing step. This is useful, as it is sometimes required to get the surrounding context.

It is impossible for a user to insert of delete `Arg` elements directly. There is care to ensure that when inserting, we add the right amount of `Arg` elements, and when we delete an element, that we delete the right amount too.

Some elements have special insertion and deletion logic. For example, if you insert a fraction in the middle of `1 2`, you'll get a fraction of a half, and if you delete that fraction with the numerator and denominator in tact, it will revert to `1 2`. Most other elements don't let you delete them unless they're empty.

The superscript encoding leads to a really natural editing experience. If you have one digit raised to a power, you could insert another digit between the the first and the superscript to move the superscript. You could also put a close bracket between the number and the superscript, and the superscript will move to the close bracket - very handy for refactoring equations.

### Convertion to MathML and TechniCalc Calculator AST

For converting to either MathML or a TechniCalc Calculator AST, we first do a transformation to a node-based AST. For example, The fraction example above transforms to something like.

```reason
type node('t) = | Frac({num: node('t), den: node('t), superscript: option(node('t))})
```

The representation of the entire tree would be `type ast = node(list(ast))`.

From there, we reduce the tree using a fold function.

```reason
type fold('accumulator, 'output) = (
  array(element),
  ~reduce: ('accumulator, node('output)) => 'accumulator,
  ~map: ('accumulator) => 'output,
  ~initialAccumulator: 'accumulator
) => 'output
```

The initial accumulator is reset for every new list of elements. For the fraction example, `num`, `den`, and `superscript` each start with a fresh value of `initialAccumulator`, and the list that contained the `Frac({ ... })` would also have a fresh value.

It's worth highlighting that nodes that contain child nodes are reduced with their child nodes already folded. When converting the fraction example to MathML (where `'output` is a string), `num` and `den` would be strings, and `superscript` would be `option(string)`.

Also of note here is we do some grouping of like-elements: digits, functions, and tables to name a few. This makes transforms easier, while allowing a very flat raw element data structure. There's no hard-and-fast rules for when to group, it's normally done when it significantly simplifies code in either the MathML or value AST processing code.

A side note is that we never fully construct a node-based AST, as the reduction can be done at the same time as we form the AST.

### Indices

Every element of the elements array is addressable by a single index. This index is imporant for MathML, so we know where to put the cursor; and also in the conversion to a TechniCalc Calculator AST, as if there is a parsing error, we need to return the index.

To handle this, the `~reduce` and `~map` functions are the start (`i`) and end index of the node (`i'`). For a simple digit, `i'` is just `i + 1`, but for a fraction, `i'` will be the `i'` of the last element in the denominator, plus one.

MathML will attach these indices to certain MathML elements in the form `id="startIndex:endIndex"` (e.g. `id="5:8"`). Every index from 0 to the length of the elements in the element array is representable by at least one of the `startIndex`s or `endIndex`s. There are never duplicates within the start indices or end indices themselves, but you may find an start index is equal to the end index.

This may seem like duplication, but we don't always have the start index. For example, the index after the last element is only representable by an `endIndex`.

By default, one element's `startIndex` has priority after another elements `endIndex`. However, for `mo` elements, we prefer to use any other available indices, and use the `mo`'s indices as a last resort. This is so expressions like `1 + 2` have the cursor stick to the numbers rather than the operator. This is done by changing the priority of indices: you can add `~` before an index to lower its priority, and a `!` to raise its priority. For now, we only support lowering the priority of start index, and raising the priority of the end index.

There are also times where a different MathML element will recieve the indices than the obvious element. For a digit on `1`, we would normally write this as `<mn id="(i):(i')">...</mn>`. However, for a digit of `1` with a superscript of `2` applied, we create am outer `<msup>` element takes the indicies instead of the `<mn>` element. (The `2` in the superscript behaves as normal).

This is bevause if you recall from the superscript mechanics explained above, it's possible to insert an element after the `1`, but before the first element of superscript. To handle this, we track the index of the superscript element in our AST representation.

```reason
type superscript('t) = {superscriptBody: 't, index: int};
type node('t) =
  | Digit({nucleus: string, superscript: option(superscript(node('t)))})
  | Frac({num: node('t), den: node('t), superscript: option(superscript(node('t)))})
```

Now we can apply superscript index as the end index of the base `<mn>` element. We allow specifying just one index in an `id` by omitting any indices you don't want to specify. For the `1^2` example above, the resulting MathML looks like this.

```xml
<msup id="0:4">
  <mi id=":1">1</mi>
  <mi id="2:3">2</mi>
</msup>
```

### Brackets

The implementation for bracket handling in `Mml.re`, `Value_Map.re`, and `BracketUtil.re` is different in every case. It's not really possible to combine these under a generic implementation. However, they all follow the same rules:

- An open bracket starts a new group on a stack
- A close bracket closes the most recent group on a stack
- A close bracket with no groups on the stack is ignored (and treated as invalid)
- Groups left on the stack that were not terminated by a close bracket are ignored (and treated as invalid)
- Each function argument starts a new stack, and restores the previous stack after the closing `Arg`

### Labels

In the standard editing mode, labels act as placeholders, although they have content in (rather than being a blank square). The blank square is created by converting an empty array of elements to MML. However, labels are explicit elements.

Because it's possible for the user to input label elements, and they'd need to be able to manipulate them, there are two editing modes: standard and label editing mode. They are defined by `allowLabelEditing`.

In the standard editing mode, labels (`LabelS`) stop some indices being selectable, so they behave more like placeholders. In particular, it's not possible to select the index immediately after a label at the end of a row.

In the label editing mode, they as standard elements (and have no extra logic).

The deletion logic also changes slightly in label editing mode to prefer deleting a label at the index rather than deleting the element before.

---

### Build

```
npm run build
```

### Watch

```
npm run watch
```

### Editor

If you use `vscode`, Press `Windows + Shift + B` it will build automatically
