#' @export
collectComputeTime <- function(dbPath) {
  dirPath <- file.path(dbPath, "_log")
  fileNames <- list.files(dirPath, pattern = "\\.out$")
  firstLines <- lapply(file.path(dirPath, fileNames), read_lines, n_max = 1)
  firstLines <- firstLines[sapply(firstLines, length) == 1]
  matches <- str_match(firstLines, "([^/]+), (\\d+) took (\\d+.?\\d*)s")
  matches <- matches[, -1]
  colnames(matches) <- c("file", "nr", "duration")
  durationTable <-
    matches |>
    as_tibble() |>
    mutate(nr = as.integer(nr), duration = as.double(duration)) |>
    drop_na()
  durations <-
    durationTable |>
    summarize(
      mean = mean(duration),
      median = stats::median(duration),
      max = max(duration),
      n = dplyr::n(),
      .by = file)
  return(durations)
}

#' @export
writeComputeTime <- function(dbPath) {

  durations <- collectComputeTime(dbPath)

  filePath <- file.path(DEEBpath::hyperDir(dbPath), "durations.csv")

  cat("Writing durations to", filePath, "\n")
  write_csv(durations, file = filePath, progress = FALSE)

  return(invisible(durations))
}
