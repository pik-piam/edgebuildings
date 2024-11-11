# various helper functions for targets pipeline

# FUNCTIONS---------------------------------------------------------------------

# separate variable column into "variable" and "scenario"
sepVarScen <- function(df) {
  # Split the "variable" column at the first occurrence of "_"
  sepStr <- strsplit(as.character(df$variable), "_", fixed = TRUE)

  # Extract the first part for column1 and the rest for column2
  df <- df %>%
    mutate(variable = sapply(sepStr, function(x) x[1]), # nolint
           scenario = sapply(sepStr, function(x) paste(x[-1], collapse = "_"))) # nolint
  return(df)
}


# separate scen_assump_speed input
# note: the individual assignment of "fullconv" has proven difficult otherwise
sepScenAssSpeed <- function(df) {
  tmp <- rbind(df %>%
                 dplyr::select("scenario", "lambda"),
               df %>%
                 dplyr::select("fullconv") %>%
                 mutate(scenario = "fullconv") %>%
                 rename(lambda = "fullconv"))
  return(tmp)
}


# separate history from scenario data
sepHistScen <- function(df,
                        endOfHistory = 2020,
                        scen = NULL,
                        reverse = FALSE) {
  # set scenario
  if (is.null(scen)) {
    scen <- unique(df[df$period == max(df$period), ]$scenario)
  }

  # check if split is necessary
  if (min(df$period) <= endOfHistory && !("history" %in% df$scenario)) {
    # split data (scenario -> hist + scenario)
    if (!reverse) {
      tmp <- rbind(df %>%
                     filter(.data[["period"]] <= endOfHistory) %>%
                     mutate(scenario = "history"),
                   df %>%
                     filter(.data[["period"]] > endOfHistory))
    }
  }
  # aggregate data (hist + scenario -> scenario)
  else if (reverse) { # nolint
    tmp <- df %>%
      mutate(scenario = scen)
  } else {
    stop("Invalid or incompatible input data.")
  }
  return(tmp)
}


# skip lines in input files
skiprow <- function(file) {
  con <- file(file, "r")
  nrow <- 0
  while (TRUE) {
    line <- readLines(con, n = 1)
    nrow <- nrow + 1
    if (length(line) == 0 || !grepl("^\\*", line))
      break
  }
  close(con)
  return(nrow - 1)
}
