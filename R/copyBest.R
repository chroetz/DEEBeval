#' @export
copyBest <- function(fromDbPath, toDbPath) {

  bests <- getBests(fromDbPath, onlyHashed = FALSE)

  if (!dir.exists(DEEBpath::hyperDir(toDbPath))) dir.create(DEEBpath::hyperDir(toDbPath))

  nFilesCopied <- 0
  for (i in seq_len(nrow(bests))) {
    info <- bests[i,] |> as.list()
    ok <- file.copy(
      from = info$filePath,
      to = file.path(DEEBpath::hyperDir(toDbPath), paste0(info$bestMethod, ".json")),
      overwrite = TRUE)
    if (ok) nFilesCopied <- nFilesCopied + 1
  }

  cat("Copied", nFilesCopied, "out of", nrow(bests), "files.\n")

  bestMethodCsv <- bests |> select(model, bestMethod, obsNr) |> rename(obs = obsNr, method = bestMethod)
  outFilePath <- file.path(DEEBpath::hyperDir(toDbPath), "methods_Best.csv")
  write_csv(bestMethodCsv, outFilePath)
  cat("Created", outFilePath, "\n")

  return(invisible())
}


getBests <- function(dbPath, onlyHashed = TRUE, methodFilter = NULL) {

  data <-
    DEEBpath::summaryTablePath(dbPath) |>
    read_csv(col_types = readr::cols()) |>
    filter(methodFull != "Truth")
  if (onlyHashed) data <- data |> filter(!is.na(hash))

  if (!is.null(methodFilter)) {
    data <- data |> filter(methodBase %in% methodFilter)
  }

  meta <-
    data |>
    select(model, obsNr, methodBase) |>
    distinct() |>
    drop_na()

  res <-
    meta |>
    rowwise() |>
    mutate(bestMethod = getBestMethod(dbPath, data, model, obsNr, methodBase))

  bests <-
    res |>
    drop_na() |>
    mutate(filePath = DEEBpath::getRanMethodOptsPath(dbPath, model, bestMethod))

  return(bests)
}
