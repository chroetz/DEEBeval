writeDoc <- function(outDir, file, ...) {
  rmarkdown::render(
    system.file("rmarkdown", paste0(file, ".Rmd"), package = "DEEBeval"),
    params = list(...),
    output_dir = outDir,
    output_file = paste0("eval_", file))
}
