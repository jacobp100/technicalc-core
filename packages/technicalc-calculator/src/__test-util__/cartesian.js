const baseCartesian = require("cartesian");
const { sampleSize } = require("lodash");

const sample = 20;
module.exports = (values) => {
  const fullResult = baseCartesian(values);
  return sample != null ? sampleSize(fullResult, sample) : fullResult;
};
