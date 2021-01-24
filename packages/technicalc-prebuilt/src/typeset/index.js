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

export default (mml, display) => {
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

  const bboxes = new Map();
  const advanceWidths = new Map();
  const setBboxes = (wrappedNode) => {
    if (!wrappedNode.element) return;
    const { id } = wrappedNode.element.attributes;
    const [current, after] = parseId(id);

    const bbox = wrappedNode.getBBox();

    if (!Number.isNaN(current)) {
      bboxes.set(current, bbox);
    }
    if (!Number.isNaN(after)) {
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
      "data-selection-before": selectionBefore,
    } = attributes;

    const nodeTransform = parseTransform(cssTransform);
    const transform = combineTransforms(inputTransform, nodeTransform);

    if (id != null) {
      const [current, after] = parseId(id);
      // eslint-disable-next-line prefer-const
      let { tX: x, tY: y, s: scale } = transform;
      x *= 1e-3;
      y *= 1e-3;

      const bbox = bboxes.get(current);
      const width = (bbox?.w ?? 0) * scale;
      const ascent = (bbox?.h ?? 0) * scale;
      const descent = (bbox?.d ?? 0) * scale;

      const defaultSelectionBefore = kind === "mo" ? "avoid" : undefined;
      const shouldAvoidSelectionBefore =
        (selectionBefore ?? defaultSelectionBefore) === "avoid";

      if (
        !Number.isNaN(current) &&
        !(shouldAvoidSelectionBefore && positionMap.has(current))
      ) {
        const position = { x, y, scale, width, ascent, descent };
        positionMap.set(current, position);
      }

      if (!Number.isNaN(after) && !positionMap.has(after)) {
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
