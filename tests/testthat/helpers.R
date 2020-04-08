
read_char <- function(path, encoding = "UTF-8") {
  txt <- rawToChar(readBin(path, "raw", file.info(path)$size))
  Encoding(txt) <- encoding
  txt
}
