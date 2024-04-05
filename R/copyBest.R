# # TODO
#
# library(tidyverse)
# dbPath <- "//clusterfs.pik-potsdam.de/tmp/cschoetz/DEEBpaper10"
#
# data <-
#   read_csv(DEEBpath::summaryTablePath(dbPath), col_types=cols()) |>
#   filter(!is.na(hash))
#
# methodOpts <- getMethodOpts(dbPath)
#
# targetMethodAndScore <- DEEBpath::getTargetMethodAndScore(dbPath)
#
# methods <- unique(data$methodBase)
# models <- unique(data$model)
# obsNrs <- unique(data$obsNr)
#
# for (model in models) for (method in methods) for (obsNr in obsNrs) {
#   d <-
#     data |>
#     filter(methodBase == method, model == .env$model, obsNr == .env$obsNr) |>
#     semi_join(targetMethodAndScore, join_by(model, scoreName, taskNr))
#
#   # TODO: getBest by using targetFun(), see getParamCube()
#
#   browser()
# }
