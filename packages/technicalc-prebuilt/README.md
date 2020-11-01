# TechniCalc Prebuilt

Bulids [technicalc-calculator](https://github.com/jacobp100/technicalc-calculator), [technicalc-editor](https://github.com/jacobp100/technicalc-editor) for the [TechniCalc app](https://jacobdoes.code.com/technicalc), and for sharing equations on the TechniCalc page on [jacob does code](https://jacobdoescode.com/technicalc).

Also builds MathJax for use in React Native, using more hacks than you could shake a stick at.

Split into two client and worker packages, where the worker will do potentially heavy computation. Communication between the two is done via `Work.re`. Best attempts are made to keep each package as small as possible by not importing more than we need to.

Not published as a package, because for my workflow, it's easier to just copy and paste files. 

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
