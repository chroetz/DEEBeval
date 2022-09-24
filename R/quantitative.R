writeQuantsDoc <- function(truthQuantTbl, quantTbl, path) {
  rmarkdown::render(
    system.file("rmarkdown", "quants.Rmd", package = "DEEBeval"),
    params = list(quantTbl = quantTbl, truthQuantTbl = truthQuantTbl),
    output_dir = path,
    output_file = "evalQuants")
}
