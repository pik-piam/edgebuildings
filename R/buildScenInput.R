#' build EDGE-B function inputs from config file
#'
#' This function reads the scenario configuration data frame from readConfig and
#' creates specified data frames as input for EDGE-B functions.
#' NOTE: This function is developed for a single scenario input.
#'
#' @param config data.frame name of scenario config df
#' @param subtype EDGE-B input type
#' @param regionmap regional mapping
#' @param regionalTargetDimension regional resolution
#' @param singleScen single scenario only
#' @param valueOnly returns list of values with regions as row names
#'
#' @returns data.frame with specified input data
#'
#' @author Hagen Tockhorn
#'
#' @importFrom dplyr select mutate_all mutate rename filter matches
#' @importFrom tidyr pivot_wider
#' @importFrom data.table :=


buildScenInput <- function(config,
                           subtype,
                           regionmap,
                           regionalTargetDimension = "EDGE_EUR_ETP",
                           singleScen = TRUE,
                           valueOnly = FALSE) {

  if (is.null(regionmap) || is.null(regionalTargetDimension)) {
    stop("regionmap and regionalTargetDimension cannot be NULL.")
  }

  regions <- unique(regionmap[["EDGE_EUR_ETP"]])



  # PARAMETERS------------------------------------------------------------------



  inputFormat <- c("scen_assump",
                   "scen_assump_speed",
                   "scen_assump_corr",
                   "fe_shares",
                   "mapping")

  # former scen_assump columns (might be changed later)
  scenAssumpCols <- c("floorspace",
                      "space_heating_m2_Uval_X_HDD",
                      "appliances_light_pop_X_gdppop",
                      "appliances_light_elas_FACTOR",
                      "cooking_pop_X_.Intercept.",
                      "water_heating_pop_X_Asym",
                      "space_cooling_m2_CDD_Uval_X_Asym",
                      "uvalues_X_min",
                      "biotrad",
                      "space_heating.elecHP_eff_X_Asym",
                      "space_heating.elecHP_share_X_Asym",
                      "space_heating.elec_X_lrc",
                      "space_cooling.elec_X_Asym",
                      "space_cooling.elec_X_lrc",
                      "water_heating.elecHP_eff_X_Asym",
                      "water_heating.elecHP_share_X_Asym",
                      "water_heating.elec_X_lrc",
                      "appliances_light.elec_X_Asym")

  # relevant scen_assump_corr variables
  scenCorrVars <- c("space_heating.elec",
                    "space_cooling.elec",
                    "water_heating.elec",
                    "appliances_light.elec")



  # Initial Checks--------------------------------------------------------------

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

  if (!(subtype %in% inputFormat)) {
    stop("Invalid subtype given.")
  }


  # PROCESS DATA----------------------------------------------------------------

  # general scenario parameter assumptions
  if (subtype == "scen_assump") {

    scen <- row.names(config) %>%
      unique() %>%
      unlist()

    tmp <- config %>%
      select(colnames(config)[grepl("econCoef", colnames(config))])

    colnames(tmp) <- gsub("econCoef_", "", colnames(tmp))
    colnames(tmp) <- gsub("[()]", ".", colnames(tmp))

    row.names(tmp) <- seq_len(nrow(tmp))

    # check if all columns match with requirements
    if (!(identical(colnames(tmp), scenAssumpCols))) {
      stop("The config file does not contain all required parameters for scen_assump.")
    }

    # disaggregate value to regional level
    # TODO: Make sure here that regions really match?
    if (!is.null(regionmap)) {
      tmp <- do.call("cbind", lapply(colnames(tmp), function(var) {
        # check if single value or list
        if (length(unlist(tmp[[var]])) > 1) {
          data <- tmp[[var]] %>%
            as.data.frame() %>%
            filter(.data[["region"]] != "EUR") %>%
            select(-"region")
        } else {
          data <- data.frame(value  = rep(unlist(tmp[[var]]),
                                          length(regions)))
        }

        data <- data %>%
          rename(!!var := "value")

        return(data)
      }))
    }

    scenAssump <- tmp %>%
      mutate_all(as.numeric) %>%
      mutate(space_heating.elec_X_Asym = 1 - .data[["space_heating.elecHP_share_X_Asym"]]
             + .data[["space_heating.elecHP_share_X_Asym"]] * .data[["space_heating.elecHP_eff_X_Asym"]],
             water_heating.elec_X_Asym = 1 - .data[["water_heating.elecHP_share_X_Asym"]]
             + .data[["water_heating.elecHP_share_X_Asym"]] * .data[["water_heating.elecHP_eff_X_Asym"]]) %>%
      # mutate(across(contains("heating.elecHP_eff"), # Not functional, but maybe this can work somehow? Maybe use pivot? Or map2? (https://stackoverflow.com/questions/78953190/is-there-a-multiple-columns-as-input-version-of-dplyrs-across-function)
      #               ~ 1 - .data[[gsub("eff", "share", {.col})]] + .data[[gsub("eff","share", {.col})]] * .x,
      #               .names = gsub("HP_eff", "new", {.col})))
      mutate(region = regions,
             scenario = scen) %>%
      select("scenario", "region", colnames(tmp), "space_heating.elec_X_Asym", "water_heating.elec_X_Asym")

    return(scenAssump)

  }

  # temporal convergence assumptions
  else if (subtype == "scen_assump_speed") { # nolint

    scen <- row.names(config)

    tmp <- config %>%
      select("speed", "speed_fullconv")

    row.names(tmp) <- seq_len(nrow(config))

    # assign assumptions on regional resolution
    tmpReg <- do.call(
      "rbind", lapply(
        colnames(tmp), function(var) {
          data <- tmp %>%
            select(var)

          # check if single value or list
          if (length(unlist(data[[var]])) > 1) {
            data <- data[[var]] %>%
              as.data.frame()
          } else {
            data <- data.frame(region = regions,
                               value  = unlist(data[[var]]))
          }

          data <- data %>%
            mutate(variable = var,
                   scenario = scen)
        }
      )
    )

    scenAssumpSpeed <- tmpReg %>%
      pivot_wider(names_from = "variable", values_from = "value") %>%
      rename(lambda = "speed",
             fullconv = "speed_fullconv") %>%
      mutate(lambda = as.numeric(.data[["lambda"]]),
             fullconv = as.numeric(.data[["fullconv"]])) %>%
      select("scenario", "region", "lambda", "fullconv") %>%
      filter(.data[["region"]] != "EUR")

    return(scenAssumpSpeed)

  }

  # final energy shares
  else if (subtype == "fe_shares") { # nolint
    tmp <- config %>%
      select(colnames(config)[grepl("feShare_", colnames(config))]) %>%
      select(-matches("speed|ariadneFix"))

    scen <- unique(row.names(config))

    row.names(tmp) <- seq_len(nrow(config))
    colnames(tmp)  <- gsub("feShare_", "", colnames(tmp))
    colnames(tmp)  <- sub("_(?=[^_]*$)", ".", colnames(tmp), perl=TRUE)

    # assign assumptions on regional resolution
    scenAssumpFEshares <- do.call(
      "rbind", lapply(
        colnames(tmp), function(var) {
          data <- tmp %>% select(var)

          # check if single value or list
          if (length(unlist(data[[var]])) > 1) {
            data <- data[[var]] %>%
              as.data.frame()
          } else {
            data <- data.frame(region = regions,
                               value  = unlist(data[[var]]))
          }

          data <- data %>%
            mutate(variable = var,
                   scenario = scen)
        }
      )
    ) %>%
      select("region", "scenario", "variable", "value") %>%
      rename("obj_share" = "value") %>%
      mutate(obj_share = unlist(.data[["obj_share"]]) / 100)

    return(scenAssumpFEshares)
  }

  # transform value / list into regional resolution
  else if (subtype == "mapping") { # nolint
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

  # not sure yet if necessary
  else if (subtype == "scen_assump_corr") { # nolint
    fitPars <- c("Asym", "R0", "lrc", "phi3")

    # build dataframe
    scenAssumpCorr <-
      do.call("rbind", lapply(scenCorrVars, function(var) {
        do.call("rbind", lapply(fitPars, function(p) {
          configID <- paste0("econCoef_", var, "_X_", p)

          if (configID %in% colnames(config)) {
            tmp <- data.frame(parameter = p,
                              variable  = var,
                              value     = unlist(config[[configID]]))
          } else {
            tmp <- data.frame(parameter = p,
                              variable  = var,
                              value     = NA)
          }
          return(tmp)
        }))
      }))

    scenAssumpCorr <- spread(scenAssumpCorr, key = "variable", value = "value")

    return(scenAssumpCorr)
  }

}
