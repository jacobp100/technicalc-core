const { sampleSize } = require("lodash");

const sample = Number(process.env.SAMPLE);
module.exports = (fullResult) =>
  !Number.isNaN(sample) ? sampleSize(fullResult, sample) : fullResult;
