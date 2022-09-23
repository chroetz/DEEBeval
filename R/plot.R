getStateSpacePlot <- function(truth, esti, obs = NULL, title = "") {
  if (is.null(obs)) {
    obs <- truth[0,]
  }
  d <- getDim(truth)
  if (d == 2) {
    projection2D <- getIdentityProjection()
  } else if (d > 2) {
    projection2D <- calculateProjection(truth$state, dim = 2)
  } else {
    stop("d invalid: ", d)
  }
  data <- bind_rows(
    truth |> mutate(kind = "truth", state2D = projection2D$project(truth$state)),
    esti |> mutate(kind = "esti", state2D = projection2D$project(esti$state))
  )
  obs <-
    obs |>
    mutate(kind = "obs", state2D = projection2D$project(obs$state))

  plt <-
    ggplot(
      data,
      aes(
        x = .data$state2D[, 1],
        y = .data$state2D[, 2],
        group = paste0(.data$trajId, .data$kind),
        color = .data$kind
      )) +
    geom_path() +
    geom_point(
      data = obs,
      mapping = aes(group = NULL, color = NULL),
      alpha = 0.1
    ) +
    xlab(NULL) + ylab(NULL) +
    theme_void() +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, vjust = -2)) +
    coord_fixed(ratio = 1) +
    ggtitle(title)

  return(plt)
}


getTimeDependencePlot <- function(truth, esti, obs = NULL, title = "") {

  if (is.null(obs)) {
    obs <- truth[0,]
  }

  data <-
    bind_rows(
      truth |> mutate(kind = "truth"),
      esti |> mutate(kind = "esti")) |>
    unpackStateLong()

  obs <-
    obs |>
    mutate(kind = "obs") |>
    unpackStateLong()

  plt <-
    ggplot(data, aes(
      x = .data$time,
      y = .data$state,
      color = .data$kind,
      group = paste0(.data$trajId, .data$kind)
    )) +
    geom_path() +
    geom_point(
      data = obs,
      mapping = aes(color = NULL, group = NULL),
      alpha = 0.1
    ) +
    facet_wrap(vars(dim), ncol = 1, scales = "free_y") +
    xlab(NULL) + ylab(NULL) +
    theme(legend.position = "none") +
    ggtitle(title)

  return(plt)
}


unpackStateLong <- function(trajs) {
  trajs |>
    mutate(tmp = as_tibble(structure(.data$state, dimnames = list(
      NULL, paste0("state", 1:ncol(.data$state))
    )))) |>
    tidyr::unpack(.data$tmp) |>
    select(-.data$state) |>
    tidyr::pivot_longer(
      starts_with("state"),
      names_to = "dim",
      values_to = "state",
      names_transform =  ~ stringr::str_sub(., start = 6)
    ) |>
    mutate(dim = as.integer(.data$dim))
}

