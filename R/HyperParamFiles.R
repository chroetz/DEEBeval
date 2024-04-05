#' @export
getMethodOpts <- function(dbPath) {
  pattern <- "^([^_]+)_([^\\.]+)\\.csv$"
  fileNames <- list.files(DEEBpath::summaryDir(dbPath), pattern = pattern)
  mat <- str_match(fileNames, pattern)
  colnames(mat) <- c("fileName", "model", "method")
  tbl <- as_tibble(mat)
  methodOpts <-
    tbl |>
    mutate(
      filePath = normalizePath(file.path(DEEBpath::summaryDir(dbPath), fileName), mustWork=TRUE))
  return(methodOpts)
}
