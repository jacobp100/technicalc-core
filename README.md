# TechniCalc Core

Monorepo for various open source aspects of the [TechniCalc app](https://jacobdoescode.com/technicalc) [(iOS)](https://apps.apple.com/gb/app/technicalc-calculator/id1504965415) [(Android)](https://play.google.com/store/apps/details?id=com.technicalc).

This whole project almost exclusively uses ReScript. It uses ReasonML syntax until the new ReScript syntax supports redefining infix operators (it's used extensively in the technicalc-calculator package).

Each package has its own (hopefully) comprehensive readme. See,

- [technicalc-calculator](https://github.com/jacobp100/technicalc-core/tree/master/packages/technicalc-calculator)
- [technicalc-editor](https://github.com/jacobp100/technicalc-core/tree/master/packages/technicalc-editor)
- [technicalc-prebuilt](https://github.com/jacobp100/technicalc-core/tree/master/packages/technicalc-prebuilt)

All packages share a lint and test infrastructure. They each have their own build setup, but there's a top level build command for convenience.

```
yarn lint
yarn test
yarn build
```
