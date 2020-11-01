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

  const widths = new Map();
  const setWidths = (wrappedNode) => {
    if (!wrappedNode.element) return;
    const { id } = wrappedNode.element.attributes;
    const [, after] = parseId(id);

    if (!Number.isNaN(after)) {
      const bbox = wrappedNode.getBBox();
      const width = bbox.w;
      widths.set(after, width);
    }

    wrappedNode.childNodes.forEach(setWidths);
  };
  setWidths(wrapper);

  const positionMap = new Map();
  const setPositions = (element, inputTransform) => {
    const { attributes = {}, children } = element;
    const { id, transform: cssTransform } = attributes;

    const nodeTransform = parseTransform(cssTransform);
    const transform = combineTransforms(inputTransform, nodeTransform);

    if (id != null) {
      const [current, after] = parseId(id);
      // eslint-disable-next-line prefer-const
      let { tX: x, tY: y, s: scale } = transform;
      x /= 1e3;
      y /= 1e3;

      if (!Number.isNaN(current)) {
        positionMap.set(current, { x, y, scale });
      }
      if (!Number.isNaN(after) && !positionMap.has(after)) {
        const width = widths.get(after);
        if (width == null) throw new Error(`No width for ${after}`);
        positionMap.set(after, { x: x + width * scale, y, scale });
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
