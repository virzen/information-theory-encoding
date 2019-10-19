const fs = require("fs");
const _ = require("lodash");

const IN_FILE_NAME = "116-binary-tree.pdf";
const BINARY_FILE_NAME = "binary.01";

// MAIN

const convertFileTo01Format = (inFilename, outFilename) => {
  const inFileBytes = [...fs.readFileSync(IN_FILE_NAME)];
  const inFileBytesInBinary = inFileBytes.map(inFileByte =>
    inFileByte.toString(2).padStart(8, "0")
  );

  fs.writeFileSync(BINARY_FILE_NAME, inFileBytesInBinary.join(""));
};

const readBinaryNumbersOfLengthFrom = (length, filename) => {
  const binaryFileBytes = [...fs.readFileSync(filename)];
  const onesAndZeros = binaryFileBytes
    .map(byte => {
      switch (byte) {
        case 48:
          return 0;
        case 49:
          return 1;
        default:
          throw new Error(`uknown byte, was ${byte}`);
      }
    })
    .map(String);

  const chunks = _.chunk(onesAndZeros, length);
  const binaryNumers = chunks.map(chunk => chunk.join(""));
  const bytesAgain = binaryNumers.map(binaryNumber =>
    Number.parseInt(binaryNumber, 2)
  );

  return bytesAgain;
};

const getProbabilityDistributionOf = numbers => {
  const total = numbers.length;
  const occurencesPerByte = _.countBy(numbers);

  return _.mapValues(occurencesPerByte, occurences => occurences / total);
};

const bytes8 = readBinaryNumbersOfLengthFrom(8, BINARY_FILE_NAME);
const bytes8Distribution = getProbabilityDistributionOf(bytes8);
console.log(bytes8Distribution);

const bytes4 = readBinaryNumbersOfLengthFrom(4, BINARY_FILE_NAME);
const bytes4Distribution = getProbabilityDistributionOf(bytes4);
console.log(bytes4Distribution);

const bytes1 = readBinaryNumbersOfLengthFrom(1, BINARY_FILE_NAME);
const bytes1Distribution = getProbabilityDistributionOf(bytes1);
console.log(bytes1Distribution);
