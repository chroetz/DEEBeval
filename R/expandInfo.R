expandInfo <- function(db, example, model, truthNr, method) {
  if (example) {
    truthFile <- sprintf("~/%s/%s/example/truth%04d.csv", db, model, truthNr)
    obsFile <- sprintf("~/%s/%s/example/truth%04dobs0001.csv", db, model, truthNr)
    if (!missing(method))estiFile <- sprintf("~/%s/%s/example/%s/truth%04dobs0001esti.csv", db, model, method, truthNr)
  } else {
    truthFile <- sprintf("~/%s/%s/truth/truth%04d.csv", db, model, truthNr)
    obsFile <- sprintf("~/%s/%s/observation/truth%04dobs0001.csv", db, model, truthNr)
    if (!missing(method))estiFile <- sprintf("~/%s/%s/submission/%s/truth%04dobs0001esti.csv", db, model, method, truthNr)
  }
  if (!missing(method))
  title <- sprintf("truth%04d, %s", truthNr, method)

  truth <- readTrajs(truthFile)
  if (!missing(method)) esti <- readTrajs(estiFile)
  obs <- readTrajs(obsFile)

  if (!hasTrajId(truth)) {
    truth <- setTrajId(truth, 0)
    if (!missing(method)) esti <- setTrajId(esti, 0)
    obs <- setTrajId(obs, 0)
  }

  list(
    truth = truth,
    obs = obs,
    esti = if (!missing(method)) esti,
    title = if (!missing(method)) title
  )
}
