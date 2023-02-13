/* global __DEV__ */
import { MathML } from "mathjax-full/js/input/mathml";
import { SVG } from "mathjax-full/js/output/svg";
import { HTMLDocument } from "mathjax-full/js/handlers/html/HTMLDocument";
import { liteAdaptor } from "mathjax-full/js/adaptors/liteAdaptor";
import transformElement from "./transformElement";
import {
  parseViewbox,
  parseTransform,
  parseId,
  combineTransforms,
} from "./util";

const inputMml = new MathML();
const outputSvg = new SVG({ fontCache: "none" });
const adaptor = liteAdaptor();
const html = new HTMLDocument("", adaptor, {
  InputJax: inputMml,
  OutputJax: outputSvg,
});
const em = 16;
const ex = 8;
const containerWidth = 80 * 16;

export default (mml, display, metadata) => {
  const math = new html.options.MathItem(mml, inputMml, display);
  math.setMetrics(em, ex, containerWidth, 100000, 1);
  math.compile(html);
  math.typeset(html);

  outputSvg.setDocument(html);
  outputSvg.math = math;
  outputSvg.pxPerEm = math.metrics.ex / outputSvg.font.params.x_height;
  math.root.setTeXclass(null);
  outputSvg.nodeMap = new Map();
  const wrapper = outputSvg.factory.wrap(math.root);
  const g = outputSvg.svg("g");
  wrapper.toSVG(g);

  const rootNode = math.typesetRoot.children[0];

  // eslint-disable-next-line no-use-before-define
  const positionMap = metadata ? getPositionMap({ rootNode, wrapper }) : null;

  const buildSvg = (element, index) => {
    const { children } = element;
    return transformElement(
      element,
      children != null && children.length > 0 ? children.map(buildSvg) : null,
      index
    );
  };
  const svg = buildSvg(rootNode);

  const { width, height, ascent } = parseViewbox(rootNode.attributes.viewBox);

  return { width, height, ascent, positionMap, svg };
};

const getPositionMap = ({ rootNode, wrapper }) => {
  const bboxes = new Map();
  const advanceWidths = new Map();
  const setBboxes = (wrappedNode) => {
    if (!wrappedNode.element) return;
    const { id } = wrappedNode.element.attributes;
    const { current, after } = parseId(id);

    const bbox = wrappedNode.getBBox();

    if (current != null) {
      bboxes.set(current, bbox);
    }
    if (after != null) {
      advanceWidths.set(after, bbox.w);
    }

    wrappedNode.childNodes.forEach(setBboxes);
  };
  setBboxes(wrapper);

  const positionMap = new Map();
  const setPositions = (element, inputTransform) => {
    const { attributes = {}, children } = element;
    const {
      id,
      transform: cssTransform,
      "data-mml-node": kind,
      "data-transform-x": transformX,
      "data-transform-y": transformY,
      "data-transform-scale": transformScale,
    } = attributes;

    const nodeTransform = {
      s: transformScale != null ? Number(transformScale) : 1,
      tX: transformX != null ? Number(transformX) : 0,
      tY: transformY != null ? Number(transformY) : 0,
    };
    const transform = combineTransforms(inputTransform, nodeTransform);

    if (__DEV__) {
      const parsedCssTransform = parseTransform(cssTransform);

      if (
        nodeTransform.s !== parsedCssTransform.s ||
        nodeTransform.tX !== parsedCssTransform.tX ||
        nodeTransform.tY !== parsedCssTransform.tY
      ) {
        throw new Error(
          `Invalid transform\n${JSON.stringify(
            nodeTransform
          )}\n${JSON.stringify(parsedCssTransform)}`
        );
      }
    }

    if (id != null) {
      const {
        avoidsSelection: manualAvoidsSelection,
        current,
        after,
      } = parseId(id);
      // eslint-disable-next-line prefer-const
      let { tX: x, tY: y, s: scale } = transform;
      x *= 1e-3;
      y *= 1e-3;

      const bbox = bboxes.get(current);
      const width = (bbox?.w ?? 0) * scale;
      const ascent = (bbox?.h ?? 0) * scale;
      const descent = (bbox?.d ?? 0) * scale;

      const avoidsSelection = manualAvoidsSelection || kind === "mo";

      if (current != null && !(avoidsSelection && positionMap.has(current))) {
        const position = { x, y, scale, width, ascent, descent };
        positionMap.set(current, position);
      }

      if (after != null && !positionMap.has(after)) {
        const advanceWidth = advanceWidths.get(after);
        if (advanceWidth == null) throw new Error(`No width for ${after}`);
        const positionX = x + advanceWidth * scale;
        const position = { x: positionX, y, scale, width: 0, ascent, descent };
        positionMap.set(after, position);
      }
    }

    if (children != null) {
      children.forEach((child) => setPositions(child, transform));
    }
  };
  setPositions(rootNode, { s: 1, tX: 0, tY: 0 });

  return positionMap;
};
