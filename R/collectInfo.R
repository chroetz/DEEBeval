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

  filePath <- file.path(DEEBpath::hyperDir(dbPath), "_info.csv")

  cat("Writing info table to", filePath, "\n")
  write_csv(infoTbl, file = filePath, progress = FALSE)

  return(invisible(infoTbl))
}
