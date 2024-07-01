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
    rowwise() |>
    mutate(obs = DEEBpath::getObsNameFromNr(fromDbPath, model, obsNr)) |>
    select(model, bestMethod, obs) |>
    rename(obs, methodFile = bestMethod)
  outFilePath <- file.path(DEEBpath::hyperDir(toDbPath), "methods_Best.csv")
  write_csv(bestMethodCsv, outFilePath, progress = FALSE)
  cat("Created", outFilePath, "\n")

  return(invisible())
}


getBests <- function(dbPath, onlyHashed = TRUE, methodTable = NULL, autoId = NULL) {

  data <-
    bind_rows(
      if (!is.null(autoId) && file.exists(DEEBpath::summaryTablePath(dbPath, autoId)))
        read_csv(DEEBpath::summaryTablePath(dbPath, autoId), col_types = readr::cols()),
      if (file.exists(DEEBpath::summaryTablePath(dbPath)))
        read_csv(DEEBpath::summaryTablePath(dbPath), col_types = readr::cols())
    )
  if (NCOL(data) == 0) stop("Could not read any score.csv files.")
  data <- data |>
    filter(methodFull != "Truth") |>
    dplyr::distinct()
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
    mutate(bestMethod = getBestMethod(dbPath, data, model, obsNr, methodBase)) |>
    drop_na()

  if (NROW(res) == 0) return(NULL)

  bests <-
    res |>
    rowwise() |>
    mutate(filePath = DEEBpath::getRanMethodOptsPath(dbPath, model, bestMethod)) |>
    ungroup()

  return(bests)
}
