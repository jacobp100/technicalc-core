const { ofString, ofStringBase } = require("../Value.bs");
const { toString } = require("../ValueTestUtil.bs");

it("parses strings", () => {
  expect(toString(ofString("100"))).toBe("100");
});

it("parses strings in other bases", () => {
  expect(toString(ofStringBase(16, "100"))).toBe("256");
});
