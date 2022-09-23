createPlotsPdf <- function(
    truthNrs,
    methods,
    model,
    db,
    example,
    outPath
) {

  plotFuns <- list(getStateSpacePlot, getTimeDependencePlot)

  grDevices::pdf(
    file.path(outPath, paste0(model, ".pdf")),
    width = 3 * length(methods),
    height = 3 * length(truthNrs)
  )

  for (pf in plotFuns) {
    pltsListList <- lapply(truthNrs, \(truthNr) {
      lapply(methods, \(method) {
        info <- expandInfo(db, example, model, truthNr, method)
        pf(info$truth, info$esti, info$obs, info$title)
      })
    })
    pltsList <- unlist(pltsListList, recursive = FALSE)
    plt <- gridExtra::arrangeGrob(grobs = pltsList, ncol = length(methods))
    plot(plt)
  }

  grDevices::dev.off()
}

