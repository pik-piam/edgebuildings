#' Create reporting file summarising EDGE-B runs
#'
#' @param path character, path to run folder
#' @param reporting reporting configuration, either a path to a file or the name
#'   of an internal reporting configuration
#'
#' @author Robin Hasse
#'
#' @importFrom yaml read_yaml
#' @importFrom piamutils getSystemFile
#' @importFrom utils tail write.csv
#' @importFrom dplyr left_join filter .data %>% mutate summarise group_by
#'   full_join

reportResults <- function(path, reporting) {

  # READ -----------------------------------------------------------------------

  ## reporting config ====

  # defines how scenarios in the run should be reported

  reportingConfig <- if (file.exists(reporting)) {
    reporting
  } else {
    getSystemFile("reporting", paste0(reporting, ".yaml"),
                  package = "edgebuildings", mustWork = TRUE)
  }
  message("Using the reporting config: ", reportingConfig)
  cfg <- read_yaml(reportingConfig)


  ## variable mapping ====

  if (is.null(cfg[["mapping"]])) {
    mapping <- NULL
  } else {
    mapping <- getSystemFile("reporting", "mappings", cfg[["mapping"]],
                             package = "edgebuildings", mustWork = TRUE) %>%
      read.csv()

    missingCols <- setdiff(c("edgeVariable", "reportingVariable", "weight"),
                           colnames(mapping))
    if (length(missingCols) > 0) {
      stop("The mapping file ", cfg[["mapping"]],
           " is missing the following columns: ",
           paste(missingCols, collapse = ", "))
    }}


  ## EDGE-B results ====

  pattern <- "projections_(.+)\\.csv"
  projectionFiles <- list.files(path, pattern, full.names = TRUE)
  names(projectionFiles) <- sub(pattern, "\\1", basename(projectionFiles))
  data <- lapply(projectionFiles, read.csv)



  # PROCESS --------------------------------------------------------------------


  ## merge history ====

  # check if history is identical across scenarios
  historyIsIdentical <- if (length(data) > 1) {
    unlist(lapply(tail(data, -1), function(scen) {
      delta <- full_join(filter(scen,      .data[["scenario"]] == "history"),
                         filter(data[[1]], .data[["scenario"]] == "history"),
                         by = c(setdiff(colnames(scen), "value"))) %>%
        mutate(absErr = abs(.data[["value.y"]] - .data[["value.x"]]),
               relErr = .data[["absErr"]] / abs(.data[["value.x"]])) %>%
        filter(.data[["absErr"]] > 1E-4,
               .data[["relErr"]] > 1E-2 )
      ncol(delta) == 0
    }))
  } else {
    TRUE
  }

  identicalHistMsg <- if (!all(historyIsIdentical)) {
    paste(
      "The 'history' scenarios are not identical across the projection files",
      paste(basename(projectionFiles), collapse = ", "),
      "in", path, "."
    )
  } else {
    NULL
  }

  # create one df without duplicated history scenarios
  if (isTRUE(cfg[["mergeHistory"]])) {
    if (!all(historyIsIdentical)) {
      warning(identicalHistMsg)
    }

    # historic periods as part of each scenario
    data <- mergeHistory(data)
  } else {
    if (!all(historyIsIdentical)) {
      stop(identicalHistMsg)
    }

    # one single history scenario
    data <- do.call(rbind, lapply(data, filter, .data[["scenario"]] != "history")) %>%
      rbind(filter(data[[1]], .data[["scenario"]] == "history"))
  }


  ## map variables ====

  if (!is.null(mapping)) {
    data <- mapping %>%
      select("edgeVariable", variable = "reportingVariable", "weight") %>%
      left_join(data, by = c(edgeVariable = "variable")) %>%
      group_by(across(all_of(
        setdiff(colnames(data), c("value", "edgeVariable", "weight"))))) %>%
      summarise(value = signif(sum(.data[["weight"]] * .data[["value"]]), 4),
                .groups = "drop")

    if (any(is.na(c(data[["region"]], data[["period"]])))) {
      warning("Region and/or period column contains NA values. ",
              "This will lead to errors when converting the data to magclass objects.")
    }
  }


  ## remove not needed columns ====

  if (!is.null(cfg[["removeCol"]])) {
    lapply(cfg[["removeCol"]], function(col) {
      if (!all(is.na(data[[col]]))) {
        warning("The column ", col, " to be removed contains non-NA data.")
      }
    })
    data <- select(data, !any_of(cfg[["removeCol"]]))

  }

  ## remove undesired regions ====

  if (!is.null(cfg[["excludeRegion"]])) {
    data <- filter(data, !.data[["region"]] %in% cfg[["excludeRegion"]])
  }



  # WRITE ----------------------------------------------------------------------

  write.csv(data, file.path(path, cfg[["fileName"]]), row.names = FALSE)

  return(invisible(data))

}
