#' @export
copyBest <- function(fromDbPath, toDbPath) {

  bests <- getBests(fromDbPath, onlyHashed = FALSE)
  if (NROW(bests) == 0) {
    cat("Was not able to collect any best method.\n")
    return(NULL)
  }

  if (!dir.exists(DEEBpath::hyperDir(toDbPath))) dir.create(DEEBpath::hyperDir(toDbPath))

  nFilesCopied <- 0
  for (i in seq_len(nrow(bests))) {
    info <- bests[i,] |> as.list()
    targetPath <- file.path(DEEBpath::hyperDir(toDbPath), paste0(info$bestMethod, ".json"))
    ok <- file.copy(
      from = info$filePath,
      to = targetPath,
      overwrite = TRUE)
    if (ok) nFilesCopied <- nFilesCopied + 1
  }

  cat("Copied", nFilesCopied, "out of", nrow(bests), "files.\n")

  bestMethodCsv <-
    bests |>
    select(model, bestMethod, obsNr) |>
    rename(obs = obsNr, methodFile = bestMethod)
  outFilePath <- file.path(DEEBpath::hyperDir(toDbPath), "methods_Best.csv")
  write_csv(bestMethodCsv, outFilePath, progress = FALSE)
  cat("Created", outFilePath, "\n")

  return(invisible())
}


getBests <- function(dbPath, onlyHashed = TRUE, methodTable = NULL) {

  data <-
    DEEBpath::summaryTablePath(dbPath) |>
    read_csv(col_types = readr::cols()) |>
    filter(methodFull != "Truth")
  if (onlyHashed) data <- data |> filter(!is.na(hash))

  if (hasValue(methodTable)) {
    data <- data |> semi_join(methodTable, join_by("methodBase" == "methodBaseFile", "model"))
  }

  meta <-
    data |>
    select(model, obsNr, methodBase) |>
    distinct() |>
    drop_na()

  if (NROW(meta) == 0) return(NULL)

  res <-
    meta |>
    rowwise() |>
    mutate(bestMethod = getBestMethod(dbPath, data, model, obsNr, methodBase))

  bests <-
    res |>
    drop_na() |>
    rowwise() |>
    mutate(filePath = DEEBpath::getRanMethodOptsPath(dbPath, model, bestMethod)) |>
    ungroup()

  return(bests)
}
