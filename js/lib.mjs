import fs from 'fs'
import _ from 'lodash'

export const IN_FILE_NAME = '116-binary-tree.pdf'
export const BINARY_FILE_NAME = 'file-in-binary.txt'
export const OUT_FILE_NAME = 'output.txt'

export const readBytes = (filename) => {
    const buffer = fs.readFileSync(filename)
    return [...buffer]
}

export const write = (filename, contents) => {
    fs.writeFileSync(filename, contents)
}

export const byteToBinary = (length, byte) => byte.toString(2).padStart(length, '0')

export const convertFileToBinary = (inputFilename, binaryLength) => {
    const bytes = readBytes(inputFilename)
    const bytesInBinary = bytes.map(byte => byteToBinary(binaryLength, byte))
    const binaryString = bytesInBinary.join('')

    return binaryString
}

const byteToBinaryDigit = (byte) => {
    switch (byte) {
        case 48:
            return 0;
        case 49:
            return 1;
        default:
            throw new Error(`incorrect byte value, was ${byte}`)
    }
}

const chunkToBinary = chunk => chunk.map(byteToBinaryDigit).map(String).join('')

export const convertFileFromBinary = (inputFilename, binaryLength) => {
    const bytes = readBytes(inputFilename);
    const chunks = _.chunk(bytes, binaryLength)
    const binaryBytes = chunks.map(chunkToBinary)
    const bytesAsNumbers = binaryBytes.map(binaryByte => Number.parseInt(binaryByte, 2))

    console.log(bytesAsNumbers)
    const buffer = Buffer.from(bytesAsNumbers)

    console.log(buffer)
}