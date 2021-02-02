# TechniCalc Core

Monorepo for various open source aspects of the [TechniCalc app](https://jacobdoescode.com/technicalc) [(iOS)](https://apps.apple.com/gb/app/technicalc-calculator/id1504965415) [(Android)](https://play.google.com/store/apps/details?id=com.technicalc).

This whole project almost exclusively uses ReScript. It uses ReasonML syntax until the new ReScript syntax supports redefining infix operators (it's used extensively in the technicalc-calculator package).

Each package has its own (hopefully) comprehensive readme. See,

- [technicalc-calculator](https://github.com/jacobp100/technicalc-core/tree/master/packages/technicalc-calculator)
- [technicalc-editor](https://github.com/jacobp100/technicalc-core/tree/master/packages/technicalc-editor)
- [technicalc-prebuilt](https://github.com/jacobp100/technicalc-core/tree/master/packages/technicalc-prebuilt)

All packages share compilation, linting, and testing infrastructure.

```bash
yarn lint
yarn test
yarn build-packages # compiles all packages with rescript-compiler
yarn watch-packages # same as above - but in watch mode
```

To generate a bundle (via technicalc-prebuilt), it's

```bash
yarn build # output to /dist
```
