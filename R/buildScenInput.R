#' build EDGE-B function inputs from config file
#'
#' This function reads the scenario configuration data frame from readConfig and
#' creates specified data frames as input for EDGE-B functions.
#' NOTE: This function is developed for a single scenario input.
#'
#' @param config data.frame name of scenario config df
#' @param subtype EDGE-B input type
#' @param regionmap regional mapping
#' @param singleScen single scenario only
#' @param valueOnly returns list of values with regions as row names
#'
#' @returns data.frame with specified input data
#'
#' @author Hagen Tockhorn, Robin Hasse
#'
#' @importFrom dplyr select mutate rename
#' @importFrom tidyr pivot_longer

buildScenInput <- function(config,
                           subtype = c("scen_assump",
                                       "scen_assump_speed",
                                       "fe_shares",
                                       "mapping"),
                           regionmap,
                           singleScen = TRUE,
                           valueOnly = FALSE) {


  subtype <- match.arg(subtype)

  regions <- unique(regionmap[["EDGE_EUR_ETP"]])



  # INITIAL CHECKS -------------------------------------------------------------

  if (is.data.frame(config)) {
    if (nrow(config) == 0) {
      stop("Config dataframe is empty.")
    }

    if (singleScen) {
      if (!(nrow(config) == 1)) {
        stop("Config dataframe contains multiple scenarios.")
      }
    }

    if (!(is.character(row.names(config)))) {
      stop("Config rows are not equal to scenario names.")
    }
  }



  # PROCESS DATA----------------------------------------------------------------

  if (subtype == "scen_assump") {

    ## general scenario parameter assumptions ====

    .expandToRegions(config, regions, prefix = "econCoef_") %>%
      .deriveElecHeatingAssump()


  } else if (subtype == "scen_assump_speed") {

    ## temporal convergence assumptions ====

    .expandToRegions(config, regions, switches = c("speed", "speed_fullconv")) %>%
      rename(lambda = "speed",
             fullconv = "speed_fullconv")


  } else if (subtype == "fe_shares") {

    ## final energy shares ====

    .expandToRegions(config, regions, prefix = "feShare_") %>%
      pivot_longer(-c("scenario", "region"),
                   names_to = "variable", values_to = "obj_share") %>%
      mutate(variable = sub("^(.+)_([a-z]+)$", "\\1.\\2", .data[["variable"]]),
             obj_share = .data[["obj_share"]] / 100)


  } else if (subtype == "mapping") {
    ## transform value / list into regional resolution ====

    # TODO: move this to another function and make it a target

    # note: can only take care of single value or list of values
    if (length(unlist(config)) > 1) {
      valueMap <- config %>%
        as.data.frame() %>%
        mutate(value = unlist(.data[["value"]]))
    } else {
      valueMap <- data.frame(region = regions,
                             value  = unlist(config))
    }

    if (valueOnly) {
      row.names(valueMap) <- valueMap$region
      valueMap <- select(valueMap, -"region")
    }

    return(valueMap)
  }
}



#' Unpack scenAssump entry to regional data.frame
#'
#' @param x scenAssump value, either a data.frame or scalar number
#' @param regions character vector of all regions
#' @returns data.frame with region and value column

.unpackDf <- function(x, regions) {
  if (is.data.frame(x)) {
    return(x[x[["region"]] %in% regions, ])
  }
  data.frame(region = regions, value = x)
}



#' Turn row names into scenario column
#'
#' @param x data.frame with row names
#' @returns data.frame with additional scenario column

.scenCol <- function(x) {
  x[["scenario"]] <- rownames(x)
  return(x)
}



#' Remove prefix from character column
#'
#' @param x data.frame
#' @param prefix character, starting pattern to be removed
#' @param col character, column name
#' @returns data.frame where prefix has been removed from col

.removePrefix <- function(x, prefix, col){
  if (!is.null(prefix)) {
    x[[col]] <- sub(paste0("^", prefix), "", x[[col]])
  }
  return(x)
}



#' select switches and remove prefix
#'
#' @param x data.frame
#' @param switches character vector of switch names (without prefix)
#' @param prefix common switch prefix that will be removed
#' @returns data.frame with selected switches without prefix

.filterSwitches <- function(x, switches, prefix) {
  if (!is.null(switches)) {
    x <- .removePrefix(x, prefix, "name")
    x[x[["name"]] %in% switches, ]
  } else if (!is.null(prefix)) {
    x <- x[grepl(paste0("^", prefix), x[["name"]]), ]
    .removePrefix(x, prefix, "name")
  } else {
    stop("either switches or prefix has to be different from NULL")
  }
}


#' Convert config to data frame with values for each region
#'
#' @param config data.frame with one line per scenario and switches as columns
#' @param regions character vector of all regions
#' @param switches character vector of switch names (without prefix)
#' @param prefix common switch prefix that will be removed
#' @returns data.frame with switch values for each scenario and region
#'
#' @importFrom dplyr %>% .data mutate filter group_by across everything reframe
#'   ungroup
#' @importFrom tidyr pivot_longer pivot_wider

.expandToRegions <- function(config, regions, switches = NULL, prefix = NULL) {

  config %>%
    .scenCol() %>%
    pivot_longer(-"scenario") %>%
    mutate(name = gsub("[()]", ".", .data[["name"]])) %>%
    .filterSwitches(switches, prefix) %>%
    group_by(across(everything())) %>%
    reframe(.unpackDf(.data[["value"]][[1]], regions)) %>%
    ungroup() %>%
    pivot_wider()
}



#' Calculate electric heating Asym from share and efficiency Asym
#'
#' @param x data.frame with switches
#' @returns data.frame with additional electric heating Asym

.deriveElecHeatingAssump <- function(x) {
  x[["space_heating.elec_X_Asym"]] <-
    (1 - x[["space_heating.elecHP_share_X_Asym"]]) * 1 +
    x[["space_heating.elecHP_share_X_Asym"]] * x[["space_heating.elecHP_eff_X_Asym"]]
  x[["water_heating.elec_X_Asym"]] <-
    (1 - x[["water_heating.elecHP_share_X_Asym"]]) * 1 +
    x[["water_heating.elecHP_share_X_Asym"]] * x[["water_heating.elecHP_eff_X_Asym"]]
  return(x)
}
