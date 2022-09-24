writePlotsPdf <- function(plotsTbl, outPath) {
  pltNames <- names(plotsTbl$plots[[1]])
  nMethods <- length(unique(plotsTbl$method))
  nTruths <- length(unique(plotsTbl$truthNr))
  grDevices::pdf(
    file.path(outPath, "evalPlots.pdf"),
    width = 3 * nMethods,
    height = 3 * nTruths
  )
  for (pltNm in pltNames) {
    pltTbl <-
      plotsTbl |>
      rowwise() |>
      mutate(plot = list(.data$plots[[pltNm]]), plots = NULL) |>
      arrange(.data$truthNr, .data$method)
    plt <- gridExtra::arrangeGrob(grobs = pltTbl$plot, ncol = nMethods)
    plot(plt)
  }
  grDevices::dev.off()
}
