const names = ["Zero", "Real", "Imag", "Cmpx", "Pcnt", "Vect", "Matx", "NaNN"];

const mapping = new Map(names.map((name, index) => [name, index]));

export default (contents) =>
  contents.replace(
    new RegExp(`"(${names.join("|")})"`, "g"),
    (_, name) => `${mapping.get(name)}`
  );
