writeDoc <- function(outDir, file, ...) {
  tempDir <- paste0("_", rlang::hash(list(Sys.time(), outDir, file, ...)))
  dir.create(tempDir)
  fileName <- paste0("eval_", file, ".html")
  rmarkdown::render(
    system.file("rmarkdown", paste0(file, ".Rmd"), package = "DEEBeval"),
    params = list(...),
    output_dir = tempDir,
    intermediates_dir = tempDir,
    output_file = fileName)
  file.copy(file.path(tempDir, fileName), file.path(outDir, fileName), overwrite = TRUE)
  unlink(tempDir, recursive=TRUE)
}
