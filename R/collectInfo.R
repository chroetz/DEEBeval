loadInfo <- function(path) {
  files <- list.files(path, pattern = "truth\\d{4}obs\\d{4}info.json", full.names=TRUE)
  if (length(files)==0) return(NULL)
  infoList <- lapply(files, jsonlite::read_json, simplifyVector = TRUE)
  infoList |> bind_rows()
}

#' @export
collectInfo <- function(dbPath) {
  models <- DEEBpath::getModels(dbPath)
  methodsTable <- DEEBpath::getMethods(dbPath, models)
  data <-
    methodsTable |>
    rowwise() |>
    mutate(info = list(loadInfo(path))) |>
    tidyr::unnest_longer(info) |>
    tidyr::unnest_wider(info)
  return(data)
}

#' @export
writeInfo <- function(dbPath) {

  infoTbl <- collectInfo(dbPath)
  filePath <- file.path(DEEBpath::summaryDir(dbPath), "_info.csv")
  cat("Writing info table to", filePath, "\n")
  write_csv(infoTbl, file = filePath, progress = FALSE)

  # TODO: assumes that there is only one task...

  infoSumm <-
    infoTbl |>
    mutate(methodBase = DEEBpath::removeHashFromName(method)) |>
    summarize(
      meanEstiElapsedTime = mean(estiElapsedTime, na.rm=TRUE),
      meanParmsElapsedTime = mean(parmsElapsedTime, na.rm=TRUE),
      meanTaskElapsedTimes = mean(taskElapsedTimes, na.rm=TRUE),
      medianEstiElapsedTime = median(estiElapsedTime, na.rm=TRUE),
      medianParmsElapsedTime = median(parmsElapsedTime, na.rm=TRUE),
      medianTaskElapsedTimes = median(taskElapsedTimes, na.rm=TRUE),
      n = dplyr::n(),
      .by = c(methodBase, obsNr, model))
  filePath <- file.path(DEEBpath::summaryDir(dbPath), "_infoSumm.csv")
  cat("Writing infoSumm table to", filePath, "\n")
  write_csv(infoSumm, file = filePath, progress = FALSE)

  infoSummSumm <-
    infoTbl |>
    mutate(methodBase = DEEBpath::removeHashFromName(method)) |>
    summarize(
      meanEstiElapsedTime = mean(estiElapsedTime, na.rm=TRUE),
      meanParmsElapsedTime = mean(parmsElapsedTime, na.rm=TRUE),
      meanTaskElapsedTimes = mean(taskElapsedTimes, na.rm=TRUE),
      medianEstiElapsedTime = median(estiElapsedTime, na.rm=TRUE),
      medianParmsElapsedTime = median(parmsElapsedTime, na.rm=TRUE),
      medianTaskElapsedTimes = median(taskElapsedTimes, na.rm=TRUE),
      n = dplyr::n(),
      .by = c(methodBase, obsNr))
  filePath <- file.path(DEEBpath::summaryDir(dbPath), "_infoSummSumm.csv")
  cat("Writing infoSummSumm table to", filePath, "\n")
  write_csv(infoSummSumm, file = filePath, progress = FALSE)

  return(invisible(infoTbl))
}
