/* eslint-disable import/no-unresolved */
import React from "react";
import Svg from "react-native-svg/lib/module/elements/Svg";
import G from "react-native-svg/lib/module/elements/G";
import Rect from "react-native-svg/lib/module/elements/Rect";
import Path from "react-native-svg/lib/module/elements/Path";
import Text from "react-native-svg/lib/module/elements/Text";

const opacities = {
  placeholder: 0.25,
  invalid: 0.5,
};

const FillContext = React.createContext("black");

const TopLevelSvg = (props) =>
  React.createElement(
    FillContext.Provider,
    { value: props.fill },
    React.createElement(Svg, props)
  );

const InheritFillSvg = ({ x, y, ...props }) => {
  const fill = React.useContext(FillContext);

  // x/y attributes broken in rn-svg SVG elements so done in a G
  return React.createElement(
    G,
    { x, y },
    React.createElement(Svg, { fill, ...props })
  );
};

export default (node, children, index) => {
  const { kind, attributes } = node;
  switch (kind) {
    case "svg":
      return attributes.xmlns != null
        ? // Top level svg - most props set in MathView
          React.createElement(TopLevelSvg, null, children)
        : // Used as a clip mask
          React.createElement(
            InheritFillSvg,
            {
              key: index,
              viewBox: attributes.viewBox,
              x: attributes.x,
              y: attributes.y,
              width: attributes.width,
              height: attributes.height,
            },
            children
          );
    case "g":
      return React.createElement(
        G,
        {
          key: index,
          transform: attributes.transform,
          opacity:
            attributes.class != null ? opacities[attributes.class.trim()] : 1,
        },
        children
      );
    case "path":
      return React.createElement(Path, {
        key: index,
        d: attributes.d,
        transform: attributes.transform, // for brackets
      });
    case "rect":
      return React.createElement(Rect, {
        key: index,
        x: attributes.x,
        y: attributes.y,
        width: attributes.width,
        height: attributes.height,
      });
    case "text": {
      const variant = attributes["data-variant"];
      const fontFamily =
        variant[0] === "-" ? `mathjax${variant}` : `mathjax-${variant}`;
      return React.createElement(
        Text,
        {
          key: index,
          transform: attributes.transform,
          fontFamily,
          fontSize: 1000,
        },
        children
      );
    }
    case "#text":
      return node.value;
    default:
      throw new Error(`Unknown element ${kind}`);
  }
};
