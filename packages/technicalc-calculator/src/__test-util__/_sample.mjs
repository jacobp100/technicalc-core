import _ from "lodash";

const sample = Number(process.env.SAMPLE);
export default (fullResult) =>
  !Number.isNaN(sample) ? _.sampleSize(fullResult, sample) : fullResult;
