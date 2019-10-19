import { BINARY_FILE_NAME } from "./lib.mjs";
import { OUT_FILE_NAME } from "./lib.mjs";
import { convertFileFromBinary } from "./lib.mjs";
import { write } from "./lib.mjs";

const contentsAsBytes = convertFileFromBinary(BINARY_FILE_NAME, 8);
write(OUT_FILE_NAME, contentsAsBytes)
