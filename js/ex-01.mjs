import { readBytes, write, byteToBinary } from "./lib.mjs";
import { IN_FILE_NAME } from "./lib.mjs";
import { BINARY_FILE_NAME } from "./lib.mjs";
import { convertFileToBinary } from "./lib.mjs";

const contentsAsBinary = convertFileToBinary(IN_FILE_NAME, 8)
write(BINARY_FILE_NAME, contentsAsBinary)

