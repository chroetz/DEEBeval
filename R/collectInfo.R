loadInfo <- function(path) {
  files <- list.files(path, pattern = "truth\\d{4}obs\\d{4}info.json", full.names=TRUE)
  if (length(files)==0) return(NULL)
  infoList <- lapply(files, saveLoadJson)
  infoList |> bind_rows()
}


saveLoadJson <- function(file) {
  result <- tryCatch(
    jsonlite::read_json(file, simplifyVector=TRUE),
    error = \(cond) cond
  )
  if (inherits(result, c("error", "condition"))) {
    cat("ERROR in file", file, ":", result$message, "\n")
    return(NULL)
  }
  return(result)
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

  infoTbl <-
    collectInfo(dbPath) |>
    mutate(methodBase = DEEBpath::removeHashFromName(method))
  filePath <- file.path(DEEBpath::summaryDir(dbPath), "_info.csv")
  cat("Writing info table to", filePath, "\n")
  write_csv(infoTbl, file = filePath, progress = FALSE)

  # TODO: assumes that there is only one task...

  infoSumm1 <-
    infoTbl |>
    summarize(
      meanEstiElapsedTime = mean(estiElapsedTime, na.rm=TRUE),
      meanParmsElapsedTime = mean(parmsElapsedTime, na.rm=TRUE),
      meanTaskElapsedTimes = mean(taskElapsedTimes, na.rm=TRUE),
      medianEstiElapsedTime = median(estiElapsedTime, na.rm=TRUE),
      medianParmsElapsedTime = median(parmsElapsedTime, na.rm=TRUE),
      medianTaskElapsedTimes = median(taskElapsedTimes, na.rm=TRUE),
      n = dplyr::n(),
      .by = c(method, obsNr, model))
  filePath <- file.path(DEEBpath::summaryDir(dbPath), "_infoSumm1.csv")
  cat("Writing infoSumm1 table to", filePath, "\n")
  write_csv(infoSumm1, file = filePath, progress = FALSE)

  infoSumm2 <-
    infoTbl |>
    summarize(
      meanEstiElapsedTime = mean(estiElapsedTime, na.rm=TRUE),
      meanParmsElapsedTime = mean(parmsElapsedTime, na.rm=TRUE),
      meanTaskElapsedTimes = mean(taskElapsedTimes, na.rm=TRUE),
      medianEstiElapsedTime = median(estiElapsedTime, na.rm=TRUE),
      medianParmsElapsedTime = median(parmsElapsedTime, na.rm=TRUE),
      medianTaskElapsedTimes = median(taskElapsedTimes, na.rm=TRUE),
      n = dplyr::n(),
      .by = c(methodBase, obsNr, model))
  filePath <- file.path(DEEBpath::summaryDir(dbPath), "_infoSumm2.csv")
  cat("Writing infoSumm2 table to", filePath, "\n")
  write_csv(infoSumm2, file = filePath, progress = FALSE)


  infoSumm3 <-
    infoTbl |>
    summarize(
      meanEstiElapsedTime = mean(estiElapsedTime, na.rm=TRUE),
      meanParmsElapsedTime = mean(parmsElapsedTime, na.rm=TRUE),
      meanTaskElapsedTimes = mean(taskElapsedTimes, na.rm=TRUE),
      medianEstiElapsedTime = median(estiElapsedTime, na.rm=TRUE),
      medianParmsElapsedTime = median(parmsElapsedTime, na.rm=TRUE),
      medianTaskElapsedTimes = median(taskElapsedTimes, na.rm=TRUE),
      n = dplyr::n(),
      .by = c(methodBase, model))
  filePath <- file.path(DEEBpath::summaryDir(dbPath), "_infoSumm3.csv")
  cat("Writing infoSumm3 table to", filePath, "\n")
  write_csv(infoSumm3, file = filePath, progress = FALSE)


  infoSumm4 <-
    infoTbl |>
    summarize(
      meanEstiElapsedTime = mean(estiElapsedTime, na.rm=TRUE),
      meanParmsElapsedTime = mean(parmsElapsedTime, na.rm=TRUE),
      meanTaskElapsedTimes = mean(taskElapsedTimes, na.rm=TRUE),
      medianEstiElapsedTime = median(estiElapsedTime, na.rm=TRUE),
      medianParmsElapsedTime = median(parmsElapsedTime, na.rm=TRUE),
      medianTaskElapsedTimes = median(taskElapsedTimes, na.rm=TRUE),
      n = dplyr::n(),
      .by = c(methodBase, obsNr))
  filePath <- file.path(DEEBpath::summaryDir(dbPath), "_infoSumm4.csv")
  cat("Writing infoSumm4 table to", filePath, "\n")
  write_csv(infoSumm4, file = filePath, progress = FALSE)

  infoSumm5 <-
    infoTbl |>
    summarize(
      meanEstiElapsedTime = mean(estiElapsedTime, na.rm=TRUE),
      meanParmsElapsedTime = mean(parmsElapsedTime, na.rm=TRUE),
      meanTaskElapsedTimes = mean(taskElapsedTimes, na.rm=TRUE),
      medianEstiElapsedTime = median(estiElapsedTime, na.rm=TRUE),
      medianParmsElapsedTime = median(parmsElapsedTime, na.rm=TRUE),
      medianTaskElapsedTimes = median(taskElapsedTimes, na.rm=TRUE),
      n = dplyr::n(),
      .by = c(methodBase))
  filePath <- file.path(DEEBpath::summaryDir(dbPath), "_infoSumm5.csv")
  cat("Writing infoSumm5 table to", filePath, "\n")
  write_csv(infoSumm5, file = filePath, progress = FALSE)

  return(invisible(infoTbl))
}
