const baseCartesian = require("cartesian");
const sample = require("./sample");

module.exports = (values) => sample(baseCartesian(values));
