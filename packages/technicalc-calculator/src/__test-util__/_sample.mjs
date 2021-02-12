import { sampleSize } from "lodash";

const sample = Number(process.env.SAMPLE);
export default (fullResult) =>
  !Number.isNaN(sample) ? sampleSize(fullResult, sample) : fullResult;
