writeDoc <- function(path, file, ...) {
  rmarkdown::render(
    system.file("rmarkdown", paste0(file, ".Rmd"), package = "DEEBeval"),
    params = list(...),
    output_dir = path,
    output_file = paste0("eval_", file))
}
