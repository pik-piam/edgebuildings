#' read scenario configuration
#'
#' This function reads the scenario configuration file and creates a data frame
#' for targets to define dynamic branches.
#'
#' @note TODO: allow child scenarios with this syntax: childScen(parentScen)
#'
#' @param config name of scenario config file
#' @param default name of default config file w/ historic and default scenario values
#' @param regiongroups region mapping for regional/economic groups
#' @param subtype specifies output
#'
#' @returns data.frame with full scenario configuration
#'
#' @author Robin Hasse, Hagen Tockhorn
#'
#' @importFrom dplyr left_join mutate_all select %>%
#' @importFrom tidyr replace_na
#' @importFrom stats setNames
#' @importFrom piamutils getSystemFile
#' @importFrom utils head tail
#' @export
#'
readConfig <- function(config = getSystemFile("config", "configTest.csv", package = "edgebuildings"),
                       default = getSystemFile("config", "default.csv", package = "edgebuildings"),
                       regiongroups = getSystemFile("config", "region_groups.csv", package = "edgebuildings"),
                       subtype = "all") {



  # FUNCTIONS ------------------------------------------------------------------

  # convert to numeric if possible
  tryAsNumeric <- function(x) {
    switch(as.character(length(x)),
      "0" = {
        xNumeric <- NULL
      },
      "1" = {
        xNumeric <- suppressWarnings(as.numeric(x))
        xNumeric <- if (is.na(xNumeric)) x else xNumeric
      },      {
        xNumeric <- lapply(x, function(xIter) {
          if (is.character(xIter)) {
            xNumericIter <- suppressWarnings(as.numeric(xIter))
            xNumericIter <- if (is.na(xNumericIter)) xIter else xNumericIter
          } else {
            xNumericIter <- xIter
          }
          return(xNumericIter)
        })
      }
    )
    return(xNumeric)
  }


  # split each string in vector
  lstrsplit <- function(x, split, idx, fixed = FALSE, perl = FALSE,
                        useBytes = FALSE) {
    unlist(lapply(x, function(xIter) {
      unlist(strsplit(xIter, split, fixed, perl, useBytes))[[idx]]
    }))
  }


  # read string as named list
  namedList <- function(x) {
    as.list(stats::setNames(lstrsplit(x, ":", 2), lstrsplit(x, ":", 1)))
  }


  # create list of values for each region
  regionalValues <- function(values, default) {
    # scenarios with regional values
    regional <- grepl(":|,", values)

    values[regional] <- as.vector(lapply(values[regional], function(val) {

      regVals <- unlist(strsplit(val, ","))

      # default value for all regions outside of specified region groups
      scenarioDefault <- regVals[!grepl(":", regVals)]
      if (length(scenarioDefault) == 0) {
        scenarioDefault <- default
      } else if (length(scenarioDefault) > 1) {
        stop("A regional parameter has more than one default value in the ",
             "scenario configuration.")
      }

      # list of values for region groups
      lstVal <- namedList(regVals[grepl(":", regVals)])

      # final data frame with values for each region
      df <- data.frame(region = regionGroups[["region"]],
                       value = scenarioDefault)

      # change value of specified regions
      for (reg in names(lstVal)) {
        if (reg %in% regionGroupNames) {
          # region groups
          df[df$region %in% df[regionGroups[[reg]], "region"], "value"] <-
            lstVal[reg]
        } else if (reg %in% regions) {
          # individual regions
          df[reg, "value"] <- lstVal[reg]
        }
      }

      # convert numeric strings to numbers
      df[["value"]] <- tryAsNumeric(df[["value"]]) %>% unlist()

      return(df)
    }))
    return(values)
  }


  # READ FILES -----------------------------------------------------------------

  # config
  if (file.exists(config)) {
    userConfig <- read.csv2(config, check.names = FALSE, na.strings = c("", "NA"), sep = ",")
  } else {
    userConfig <- file.path("config", config) %>%
      read.csv2(check.names = FALSE, na.strings = c("", "NA"), sep = ",")
  }

  # default
  if (file.exists(default)) {
    defaultConfig <- read.csv2(default, na.strings = c("", "NA"), sep = ",")
  } else {
    defaultConfig <- file.path("config", default) %>%
      read.csv2(na.strings = c("", "NA"), sep = ",")
  }

  # region groups
  if (file.exists(regiongroups)) {
    regionGroups <- read.csv2(regiongroups, na.strings = c("", "NA"), sep = ";")
  } else {
    regionGroups <- file.path("config", regiongroups) %>%
    read.csv2(na.strings = c("", "NA"), sep = ";")
  }

  ## check files ====

  colNamesDefault <- c("parameter", "parameterComment", "history", "default")
  colNamesUser <- c("parameter", "valueComment")
  regionGroupNames <- setdiff(colnames(regionGroups), "region")
  regions <- regionGroups[["region"]]


  # check column names
  if (!identical(colnames(defaultConfig), colNamesDefault)) {
    stop("default.csv must have the following colnames: ",
         paste(colNamesDefault, collapse = ", "))
  }
  if (!identical(head(colnames(userConfig), length(colNamesUser)),
                 colNamesUser)) {
    stop("The first columns of the scenario config must be: ",
         paste(colNamesUser, collapse = ", "))
  }
  if (any(is.na(colnames(userConfig)))) {
    stop("All scenarios must be named.")
  }
  if (any(duplicated(tail(colnames(userConfig), -length(colNamesUser))))) {
    stop("All scenarios in scenario config must have distinct names.")
  }
  if (intersect(colnames(userConfig), colnames(defaultConfig)) != "parameter") {
    stop("Invalid scenario names in scenario config.")
  }


  # check parameters
  if (length(setdiff(userConfig[["parameter"]],
                     defaultConfig[["parameter"]]) > 0)) {
    stop("One or more parameters in the scenario configuration are not ",
         "defined in default.csv.")
  }
  if (any(duplicated(defaultConfig[["parameter"]]))) {
    stop("No duplicated parameterisation allowed in defaults.csv.")
  }
  if (any(duplicated(userConfig[["parameter"]]))) {
    stop("No duplicated parameterisation allowed in scenario configuration.")
  }
  if (any(is.na(defaultConfig[["default"]]))) {
    stop("The following parameters are missing default values: ",
         paste(defaultConfig[["parameter"]][is.na(defaultConfig[["default"]])],
               collapse = ", "))
  }
  if (length(intersect(regionGroupNames, regions)) > 0) {
    stop("Region groups cannot be named like individual regions.")
  }

  # check subtypes
  if (!(subtype %in% c("all", "history", "scenario", "environment"))) {
    stop("Invalid subtype given.")
  }



  # CREATE FINAL CONFIG OBJECT -------------------------------------------------


  # join default and scenario config
  fullConfig <- left_join(defaultConfig, userConfig, by = "parameter") %>%
    select(-"parameterComment", -"valueComment")

  scenNames <- setdiff(colnames(fullConfig), colNamesDefault)
  defaultValues <- as.list(stats::setNames(fullConfig[["default"]],
                                           fullConfig[["parameter"]]))

  # transpose config
  rownames(fullConfig) <- fullConfig[["parameter"]]
  fullConfig <- fullConfig %>%
    select(-"parameter", -"default") %>%
    t() %>%
    as.data.frame()

  # fill gaps in scenario configuration with default
  fullConfig[scenNames, ] <- fullConfig[scenNames, ] %>%
    replace_na(defaultValues)

  # parameters with regional values are filled with data frames that contain
  # the value for each region
  fullConfig[scenNames, ] <- mapply(regionalValues, fullConfig[scenNames, ], # nolint
                                    defaultValues) %>%
    as.data.frame()


  # make all numbers numeric
  fullConfig <- fullConfig %>%
    mutate_all(tryAsNumeric)



  # OUTPUT----------------------------------------------------------------------

  return(switch(subtype,
                "all"         = fullConfig,
                "history"     = fullConfig["history", ],
                "scenario"    = fullConfig[-which(rownames(fullConfig) == "history"), ]))

}
