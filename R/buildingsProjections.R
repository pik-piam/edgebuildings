#' Make projections for useful and final energy levels at (enduse, carrier) level
#'
#' This function covers the entire projection part of EDGE-B. It has been adapted
#' from the previous version of Antoine Levesque to allow scenario-wise processing
#' rather than running all scenarios at once.
#'
#' @note The functionality of some features has not (yet) been integrated into this
#' new version. This includes the entire plot part, the correction for heat pumps,
#' \code{hpCorrection}, and the \code{lifestyle} and \code{variableSpeed} parameters.
#'
#' @param config scenario-wise parameter configuration
#' @param floor historical and future residential/commercial floorspace (per capita)
#' @param hddcdd historical and future HDD's/CDD's
#' @param pop historical and future population data
#' @param gdppop historical and future gdp per capita
#' @param uvalue historical and future building insulation values
#' @param pfu historical final and useful energy data
#' @param feueEff historical and future FE->UE conversion efficiencies
#' @param feSharesEC historical and future final energy carrier shares
#' @param regionmap regional mapping
#' @param scenAssump carrier/enduse-specific scenario assumptions
#' @param scenAssumpSpeed long-term temporal scenario assumptions
#' @param scenAssumpCorrect specific corrections
#' @param outputDir output directory
#' @param hpCorrection heatpump correction for electric space heating
#' @param lifestyle lifestyle parameter
#' @param variableSpeed variable speed
#' @param ariadneFix ariadneFix flag
#'
#' @author Antoine Levesque, Hagen Tockhorn
#'
#' @importFrom dplyr inner_join left_join group_by mutate ungroup select
#'   reframe filter anti_join across all_of %>% .data
#' @importFrom tidyr gather unite
#' @importFrom quitte interpolate_missing_periods removeColNa aggregate_map calc_addVariable
#'   calc_addVariable_ getVars
#' @importFrom stats as.formula na.omit approx
#' @importFrom utils write.csv

buildingsProjections <- function(config,
                                 floor,
                                 hddcdd,
                                 pop,
                                 gdppop,
                                 uvalue,
                                 pfu,
                                 feueEff,
                                 feSharesEC,
                                 regionmap,
                                 scenAssump,
                                 scenAssumpSpeed,
                                 scenAssumpCorrect,
                                 outputDir = "output",
                                 hpCorrection = TRUE,
                                 lifestyle = NULL,
                                 variableSpeed = NULL,
                                 ariadneFix = FALSE) {
  # PARAMETERS--------------------------------------------------------------------

  # scenario
  scen <- row.names(config) %>% unique()

  if (length(scen) > 1) {
    stop("buildingsProjections() can not handle more than one scenario.")
  }

  # nolint start
  # unit conversions
  EJ2KwH          <- 2.77778E+11
  Million2Units   <- 1e6
  EJ2Wyr          <- EJ2KwH / (24 * 365) * 1e3
  # nolint end

  # upper temporal threshold of historic data
  endOfHistory <- config[scen, "endOfHistory"] %>%
    unlist()

  # enduses
  enduses <- c("space_heating",
               "appliances_light",
               "water_heating",
               "cooking",
               "space_cooling")

  # european countries
  eurCountries <- c("FIN", "AUT", "BEL", "BGR", "CYP", "CZE", "DEU", "DNK", "ESP",
                    "EST", "FRA", "GBR", "GRC", "HRV", "HUN", "IRL", "ITA", "LTU",
                    "LUX", "LVA", "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN",
                    "SWE")

  print(paste0("Initialize projection preparation for scenario: ", scen))



  # PRE-PROCESS DATA--------------------------------------------------------------

  # aggregate fe/ue data to enduse resolution
  pfu <- pfu %>%
    filter(.data[["unit"]] == "ue") %>%
    group_by(across(all_of(c("scenario", "region", "period", "enduse")))) %>%
    reframe(value = sum(.data[["value"]], na.rm = TRUE)) %>%
    ungroup() %>%
    rename("variable" = "enduse") %>%
    as.data.frame() %>%
    as.quitte() %>%
    missingToNA()

  # remove duplicate data
  floor <- unique(floor)


  #--- Data is made compliant with config file

  # floorspace
  floor <- floor %>%
    filter(.data[["scenario"]] == scen)

  # heating/cooling degree days
  hddcdd <- hddcdd %>%
    filter(.data[["scenario"]] == scen) %>%
    as.quitte()

  # population
  pop <- pop %>%
    filter(.data[["variable"]] == config[, "popScen"]) %>%
    unique() %>%
    sepVarScen() %>%
    mutate(scenario = scen) %>%
    as.quitte()

  # gdppop
  gdppop <- gdppop  %>%
    filter(.data[["variable"]] == config[, "gdppopScen"]) %>%
    unique() %>%
    sepVarScen() %>%
    mutate(scenario = scen) %>%
    as.quitte()

  # U-value
  uvalue <- uvalue %>%
    filter(.data[["scenario"]] == scen) %>%
    as.quitte()

  # fe->ue efficiencies
  feueEff <- feueEff %>%
    filter(.data[["scenario"]] == scen) %>%
    rename("efficiency" = "value") %>%
    removeColNa()

  # fe carrier shares
  feSharesEC <- feSharesEC %>%
    filter(.data[["scenario"]] == scen) %>%
    rename("share" = "value") %>%
    removeColNa()

  # scenario convergence speed assumptions
  scenAssumpSpeed <- scenAssumpSpeed %>%
    filter(.data[["scenario"]] == scen)

  # scenario parameter assumptions
  scenAssump <- scenAssump %>%
    filter(.data[["scenario"]] == scen)

  # temporal convergence shares
  lambda <- compLambdaScen(scenAssumpSpeed, startYearVector = 1960, startPolicyYear = 2020)
  lambdaDelta <- compLambdaScen(scenAssumpSpeed, startYearVector = 1960, startPolicyYear = 2030)

  dfPlot <- joinReduceYearsRegion(pop, hddcdd)
  dfPlot <- joinReduceYearsRegion(dfPlot, pfu)
  dfPlot <- joinReduceYearsRegion(dfPlot, floor)



  # PROCESS DATA----------------------------------------------------------------

  # build region global mapping
  scenAssumpRegion <- data.frame(
    region       = unique(regionmap[["EDGE_EUR_ETP"]]), # nolint
    regionTarget = "GLO") # nolint


  #--- Make Projections --------------------------------------------------------

  # standardize missing column entries
  df <- rbind(gdppop, pop, hddcdd, pfu, floor, uvalue) %>%
    filter(.data[["period"]] <= 2100) %>%
    missingToNA()


  # split history / scenario for all data
  df         <- history2SSPs(df,         reverse = TRUE, endOfHistory = endOfHistory)
  feueEff    <- history2SSPs(feueEff,    reverse = TRUE, endOfHistory = endOfHistory)
  feSharesEC <- history2SSPs(feSharesEC, reverse = TRUE, endOfHistory = endOfHistory)


  # match periods with lambda
  df <- df %>%
    filter(.data[["period"]] %in% getPeriods(lambda))


  # derive variables of historic data for projection
  df <- df %>%
    calc_addVariable("rvalue" = "1/uvalue") %>%
    calc_addVariable("coefCDD" = "(1 - 0.949 * exp(-0.00187 * CDD * 1.26))",
                     units = NA) %>%
    # calc_addVariable("coefGDP" = "(1 / (1 + exp(4.152 - 0.237*(gdppop/1e3))))",
    #                  units = NA) %>%
    filter(!is.na(.data[["value"]])) %>%
    calc_addVariable("space_heating_m2_Uval" = "space_heating * 1e6 / (buildings * uvalue)",
                     factor = 1e6,
                     units = "Space Heating Demand [MJ.K/W]") %>%
    calc_addRatio("water_heating_pop", "water_heating", "pop",
                  factor = 1e3,
                  units = "Water Heating Demand [GJ/cap]") %>%
    calc_addRatio("cooking_pop", "cooking", "pop",
                  factor = 1e3,
                  units = "GJ/cap") %>%

    # equation from McNeil(2007) Future Air conditioning energy consumption
    calc_addVariable("space_cooling_m2_CDD_Uval" = "space_cooling / (buildings * uvalue * CDD * coefCDD) * 1e6",
                     units = "Space Cooling Demand [MJ/((W/K)*f(CDD))]") %>%
    calc_addVariable("appliances_light_elas" = "log(I(appliances_light / pop)) - 0.3*log(gdppop)",
                     units = NA) %>%
    filter(!is.na(.data[["value"]]))


  df <- character.data.frame(df)

  enduseVars <- c("space_heating_m2_Uval",
                  "appliances_light_elas_FACTOR",
                  "water_heating_pop",
                  "cooking_pop",
                  "space_cooling_m2_CDD_Uval")

  # change lambda according to lifestyle scenario assumptions
  lambdaDifferentiated <- lapply(setNames(enduseVars, enduseVars), function(x) lambda)


  #--- Make Projections
  print("Start projections")

  df <- makeProjections(config, df, as.formula("space_heating_m2_Uval ~ 0 + HDD"),
                        scenAssump, lambdaDifferentiated["space_heating_m2_Uval"][[1]],
                        scenAssumpCorrect, scen, convReg = "proportion",
                        outliers = c("RUS", "FIN"),
                        scenAssumpRegion = scenAssumpRegion, lambdaDelta = lambdaDelta)

  df <- makeProjections(config, df, as.formula("appliances_light_elas ~ I(gdppop^(-1/2))"),
                        scenAssump, lambdaDifferentiated["appliances_light_elas_FACTOR"][[1]],
                        scenAssumpCorrect, scen, apply0toNeg = FALSE,
                        transformVariableScen = c("exp(VAR +  0.3*log(gdppop)) *1e3",
                                                  unit = "Appliances and Light Demand [GJ/cap]"),
                        applyScenFactor = TRUE,
                        scenAssumpRegion = scenAssumpRegion, lambdaDelta = lambdaDelta)

  df <- makeProjections(config, df, as.formula("water_heating_pop ~ SSlogis(gdppop, Asym,phi2,phi3)"),
                        scenAssump, lambdaDifferentiated["water_heating_pop"][[1]],
                        scenAssumpCorrect, scen, maxReg = 7,
                        outliers = c("RUS", eurCountries), avoidLowValues = TRUE,
                        scenAssumpRegion = scenAssumpRegion, lambdaDelta = lambdaDelta)

  df <- makeProjections(config, df, as.formula("cooking_pop ~ 1"),
                        scenAssump, lambdaDifferentiated["cooking_pop"][[1]],
                        scenAssumpCorrect, scen, outliers = eurCountries,
                        scenAssumpRegion = scenAssumpRegion, lambdaDelta = lambdaDelta)

  df <- makeProjections(config, df, as.formula("space_cooling_m2_CDD_Uval ~ SSlogis(gdppop, Asym,phi2,phi3)"),
                        scenAssump, lambdaDifferentiated["space_cooling_m2_CDD_Uval"][[1]],
                        scenAssumpCorrect, scen,
                        outliers = c("RUS", "EUR", "OCD", setdiff(eurCountries, c("ESP", "PRT", "GRC", "ITA"))),
                        avoidLowValues = TRUE,
                        scenAssumpRegion = scenAssumpRegion, lambdaDelta = lambdaDelta)

  df <- df %>%
    # define enduse variables from projected variables
    calc_addVariable_(list( # nolint start
      "space_heating"    = c("space_heating_m2_Uval/1e6 * buildings * uvalue", NA),
      "appliances_light" = c("appliances_light_elas* pop/1e3", NA),
      "water_heating"    = c("water_heating_pop  / 1e3 * pop", NA),
      "cooking"          = c("cooking_pop  / 1e3 * pop", NA),
      "space_cooling"    = c("space_cooling_m2_CDD_Uval/1e6 * (buildings*uvalue*CDD*coefCDD)", NA))) %>%
    # nolint end

    # filter unwanted entries
    anti_join(df, by = c("scenario", "period", "region", "variable")) %>%
    rbind(df) %>%

    # make corrections
    mutate(value = ifelse(.data[["variable"]] %in% c("water_heating_pop", "cooking_pop"),
                          .data[["value"]] / 1e3,
                          .data[["value"]]),
           unit  = ifelse(.data[["variable"]] %in% c("water_heating_pop", "cooking_pop"),
                          NA,
                          .data[["unit"]]))


  #--- USEFUL ENERGY TO FINAL ENERGY -------------------------------------------

  # Compute the Useful and Final energy levels at the (end-use, energy carrier) level,
  # and compute the FE level for (end-use).
  # Add the energy type in the variable name

  print("Computing UE and FE at carrier level.")

  df <- disaggregateEnergy(data    = df,
                           eff     = feueEff,
                           shares  = feSharesEC,
                           enduses = enduses)


    # global values
  dfGLO <- df %>%
    calcGlob(c(unique(grep("^(?!.*pop).*\\|.e$", df$variable, value = TRUE, perl = TRUE)),
               "pop", "gdp", "buildings"),
             nameGLO = "GLO") %>%
    rbind(calcGlob(df, c("HDD", "CDD", "gdppop"), weights = "pop")) %>%
    rbind(calcGlob(df, c("rvalue"), weights = "buildings"))

  df <- rbind(df, dfGLO)


  #  Add Variables / Ratios
  df <- calc_addRatio(
    df,
    c("space_cooling_pop|fe", "space_heating_pop|fe", "space_cooling_pop|ue", "space_heating_pop|ue"),
    c("space_cooling|fe", "space_heating|fe", "space_cooling|ue", "space_heating|ue"),
    "pop"
  )

  df <- calc_addRatio(
    df,
    c("appliances_light_pop|fe", "water_heating_pop|fe", "cooking_pop|fe"),
    c("appliances_light|fe", "water_heating|fe", "cooking|fe"),
    "pop"
  )

  df <- calc_addRatio(
    df,
    c("appliances_light_pop|ue"),
    c("appliances_light|ue"),
    "pop"
  )

  df <- calc_addRatio(df, "buildings_pop", "buildings", "pop", units = "m2/cap")
  df <- calcAddProd(df, "gdp", c("gdppop", "pop"), units = "million")

  df <- calc_addSum(df, list(
    "biomod|fe" = grep("biomod\\|fe", getVars(df), value = TRUE),
    "biotrad|fe" = grep("biotrad\\|fe", getVars(df), value = TRUE),
    "coal|fe" = grep("coal\\|fe", getVars(df), value = TRUE),
    "petrol|fe" = grep("petrol\\|fe", getVars(df), value = TRUE),
    "heat|fe" = grep("heat\\|fe", getVars(df), value = TRUE),
    "natgas|fe" = grep("natgas\\|fe", getVars(df), value = TRUE),
    "elec|fe" = grep("elec\\|fe", getVars(df), value = TRUE)
  ))

  df <- calc_addSum(df, list(
    "biomod|ue" = grep("biomod\\|ue", getVars(df), value = TRUE),
    "biotrad|ue" = grep("biotrad\\|ue", getVars(df), value = TRUE),
    "coal|ue" = grep("coal\\|ue", getVars(df), value = TRUE),
    "petrol|ue" = grep("petrol\\|ue", getVars(df), value = TRUE),
    "heat|ue" = grep("heat\\|ue", getVars(df), value = TRUE),
    "natgas|ue" = grep("natgas\\|ue", getVars(df), value = TRUE),
    "elec|ue" = grep("elec\\|ue", getVars(df), value = TRUE)
  ))

  df <- calc_addSum(df, list(
    "space_conditioning|ue" = c("space_heating|ue", "space_cooling|ue"),
    "space_conditioning|fe" = c("space_heating|fe", "space_cooling|fe")
  ))

  df <- calc_addSum(df, list(
    "ue" = c("biomod|ue", "biotrad|ue", "coal|ue", "petrol|ue", "heat|ue", "natgas|ue", "elec|ue"),
    "fe" = c("biomod|fe", "biotrad|fe", "coal|fe", "petrol|fe", "heat|fe", "natgas|fe", "elec|fe")
  ))


  df <- calc_addRatio(df, "Energy Intensity|ue", "ue", "gdp", factor = 1e9, units = "MJ/1000$")

  df <- calc_addRatio(df, "Energy Intensity|fe", "fe", "gdp", factor = 1e9, units = "MJ/1000$")

  df <- calc_addRatio(df, "Energy Intensity_appliances_light|ue", "appliances_light|ue", "gdp",
                      factor = 1e9, units = "MJ/1000$")

  df <- calc_addRatio(df, "Energy Intensity_appliances_light|fe", "appliances_light|fe", "gdp",
                      factor = 1e9, units = "MJ/1000$")

  df <- calc_addRatio(df, "Energy Intensity_space_heating|fe", "space_heating|fe", "gdp",
                      factor = 1e9, units = "MJ/1000$")

  df <- calc_addRatio(df, "Energy Intensity_space_cooling|fe", "space_cooling|fe", "gdp",
                      factor = 1e9, units = "MJ/1000$")

  df <- calc_addRatio(df, "Energy Intensity_cooking|fe", "cooking|fe", "gdp",
                      factor = 1e9, units = "MJ/1000$")

  df <- calc_addRatio(df, "Energy Intensity_water_heating|fe", "water_heating|fe", "gdp",
                      factor = 1e9, units = "MJ/1000$")

  df <- calc_addRatio(
    df, c("petrol_pop|fe", "heat_pop|fe", "natgas_pop|fe", "biomod_pop|fe", "biotrad_pop|fe", "coal_pop|fe"),
    c("petrol|fe", "heat|fe", "natgas|fe", "biomod|fe", "biotrad|fe", "coal|fe"), "pop"
  )
  df <- calc_addRatio(
    df, c("Energy per capita|fe", "Energy per capita|ue"),
    c("fe", "ue"), "pop"
  )

  df <- calc_addRatio(df, c("space_heating|es", "space_cooling|es"),
                      c("space_heating|ue", "space_cooling|ue"),
                      "uvalue",
                      factor = EJ2Wyr
  )
  df <- calc_addRatio(df, c("space_heating|gradient", "space_cooling|gradient"),
                      c("space_heating|es", "space_cooling|es"),
                      "buildings",
                      factor = 1 / Million2Units
  )

  # split history / scenario for all data
  df <- rbind(df %>%
                filter(.data[["period"]] <= endOfHistory) %>%
                mutate(scenario = "history"),
              df %>%
                filter(.data[["period"]] > endOfHistory))


  #--- CORRECTION --------------------------------------------------------------

  # In 2000, only NCD and OCD have data. It biases the GLO figures
  # df = df %>% filter(.data[["period]] != 2000)
  # this is because appliances_light.biotrad,.biomod .etc does only exist in history
  # in combination with completeMissing option from calc_addVariable (and add_Sum),
  # it creates rows with history after 2010
  print("Correcting projections.")

  df <- df %>% filter(
    !(.data[["scenario"]] == "history" & .data[["period"]] > endOfHistory),
    !(grepl("^SSP.*", .data[["scenario"]]) & .data[["period"]] <= endOfHistory)
  )


  #--- Split electric space_heating

  if (isTRUE(hpCorrection)) {

    hp <- do.call(rbind, lapply(
      c("space_heating", "water_heating"),
      splitElec,
      df = df,
      feueEff = feueEff,
      scenAssump = scenAssump
    ))

    # add split electric space heating to results
    df <- rbind(df, hp)

    df <- calc_addSum(df, list(
      "elecHP|fe" = grep("elecHP\\|fe", getVars(df), value = TRUE),
      "elecRH|fe" = grep("elecRH\\|fe", getVars(df), value = TRUE)
    ))

    df <- calc_addSum(df, list(
      "elecHP|ue" = grep("elecHP\\|ue", getVars(df), value = TRUE),
      "elecRH|ue" = grep("elecRH\\|ue", getVars(df), value = TRUE)
    ))
  }


  #--- Add RCP scenario to scenario name if existent

  if (!identical(config[[scen, "rcpScen"]], "noCC")) {
    df <- mutate(df, scenario = ifelse(.data[["scenario"]] != "history",
                                       paste0(.data[["scenario"]], "_rcp", config[[scen, "rcpScen"]]),
                                       .data[["scenario"]]))
  }



  # OUTPUT------------------------------------------------------------------------

  print("Projections finished - saving data.")

  fileName <- paste0("projections_", scen)

  write.csv(df, file.path(outputDir, paste0(fileName, ".csv")), row.names = FALSE)
}



# INTERNAL FUNCTIONS------------------------------------------------------------

#' Compute carrier-specific useful/final energy
#'
#' Carrier-specific values for useful and final energy are calculated using
#' efficiencies for FE->UE conversion for specific carrier-enduse combinations
#' and shares of energy carriers w.r.t. to specific enduses.
#'
#' Energy-type-specific weights are calculated to disaggregate scenario-wise
#' projected enduse-specific data stored in \code{data}.
#'
#' @param data data.frame containing enduse-specific aggregate FE/UE data
#' @param eff data.frame containing final to useful energy conversion efficiencies
#' @param shares data.frame containing FE carrier shares w.r.t. specific enduses
#' @param enduses list of enduses
#'
#' @return data.frame containing (non-)aggregated FE/UE data for enduse-carrier pairs

disaggregateEnergy <- function(data, eff, shares, enduses) {

  # compute the weights for getting FE and UE levels for each energy carrier from the aggregate UE level
  weights <- eff %>%
    inner_join(shares, by = c("scenario", "region", "period", "enduse", "carrier")) %>%
    group_by(across(all_of(c("scenario", "region", "period", "enduse")))) %>%
    mutate(weightFE = .data[["share"]] / sum(.data[["efficiency"]] * .data[["share"]], na.rm = TRUE),
           weightUE = .data[["weightFE"]] * .data[["efficiency"]]) %>%
    ungroup() %>%
    dplyr::select(-"share", -"efficiency")

  # Compute the UE and FE levels for each (end use, energy carrier)
  feue <- weights %>%
    inner_join(data, by = c("scenario", "period", "region", "enduse" = "variable")) %>%
    mutate(fe = .data[["value"]] * .data[["weightFE"]],
           ue = .data[["value"]] * .data[["weightUE"]]) %>%
    dplyr::select(-"weightFE", -"weightUE", -"value", -"unit") %>%
    gather("unit", "value", one_of("fe", "ue"))

  # Compute the aggregate FE level for the whole end use and add the Energy Type (FE or UE) in the variable name
  feAgg <- feue %>%
    filter(.data[["unit"]] == "fe") %>%
    group_by(across(all_of(c("scenario", "region", "period", "enduse", "model", "unit")))) %>%
    reframe(value = sum(.data[["value"]], na.rm = TRUE)) %>%
    ungroup() %>%
    unite(col = "variable", "enduse", "unit", sep = "|") %>%
    as.quitte() %>%
    missingToNA()

  # Add the Energy Type (FE or UE) in the variable name
  feue <- feue %>%
    unite(col = "variable", "enduse", "carrier", sep = ".") %>%
    unite(col = "variable", "variable", "unit", sep = "|") %>%
    as.quitte() %>%
    missingToNA()

  # Add the Energy Type (FE or UE) in the variable name
  data <- data %>%
    mutate(variable = as.character(.data[["variable"]]),
           variable = ifelse(.data[["variable"]] %in% c(enduses, paste0(enduses, "_pop")),
                             paste0(.data[["variable"]], "|ue"),
                             .data[["variable"]]))

  result <- rbind(data, feue, feAgg)

  return(result)
}


#' Aggregate various variables to european regions
#'
#' @param df data to be aggregated
#' @param extVars variables of extensive items
#' @param intVars variables of intensive items
#' @param floorVars variables of floorspace items
#' @param regionmap data.frame that maps countries to regions
#' @return data.frame with aggregated data on specific variables
#'
#' @importFrom dplyr select filter %>% .data
#' @importFrom quitte getColValues aggregate_map calc_addVariable

addEURagg <- function(df, extVars, intVars, floorVars, regionmap) {
  # region mapping
  mapping <- regionmap %>%
    filter(.data[["EDGE_all"]] == "EUR") %>%
    select("EDGE_allEUR", "EDGE_all") %>%
    unique()

  tmp <- df %>%
    filter(.data[["region"]] %in% getColValues(mapping, "EDGE_allEUR"))

  tmpExt <- aggregate_map(data = tmp,
                          mapping = mapping,
                          by = c("region" = "EDGE_allEUR"),
                          subset2agg = extVars)


  tmpInt <- aggregate_map(data = tmp,
                          mapping = mapping,
                          by = c("region" = "EDGE_allEUR"),
                          subset2agg = intVars,
                          weights = "pop")

  tmpFloor <- aggregate_map(data = tmp,
                            mapping = mapping,
                            by = c("region" = "EDGE_allEUR"),
                            subset2agg = floorVars,
                            weights = "buildings")

  if ("rvalue" %in% floorVars) tmpFloor <- tmpFloor %>% calc_addVariable("uvalue" = "1/rvalue")

  data <- rbind(df, tmpExt, tmpInt, tmpFloor)

  return(data)
}

#' Split heat pump and resistive electric fe and ue
#'
#' @param df data frame containing share and efficiency data
#' @param feueEff historical and future FE->UE conversion efficiencies
#' @param enduseChar character, enduse for which electric FE demand should be split
#' @param scenAssump carrier/enduse-specific scenario assumptions
#'
#' @returns data frame containing split fe and ue
#'
#' @importFrom dplyr %>% .data across filter full_join group_by left_join
#'   mutate reframe rename_with select ungroup
#' @importFrom quitte calc_addVariable_
#' @importFrom tidyr gather spread
#'
splitElec <- function(df, feueEff, enduseChar, scenAssump) {
  effRHasym  <- 1.0 # assumed by AL but IDEES finds rather 0.8 - 0.9

  hpEffHist <- 3

  # exponential function approaching Asym, constant before start year
  expAsym <- function(valStart, valAsym, t, tStart = 2020, tau = 50) {
    valStart + (valAsym - valStart) * (1 - exp(-pmax(0, t - tStart) / tau))
  }

  effElec <- feueEff %>%
    filter(.data[["enduse"]] == enduseChar, .data[["carrier"]] == "elec") %>%
    select(-"enduse", -"carrier")

  scenAssumpHP <- expand.grid(region = unique(df[["region"]])) %>%
    mutate(scenario = "history") %>%
    mutate(elecHP_eff = hpEffHist) %>%
    rename_with(~ paste0(enduseChar, ".", .x, "_X_Asym"), "elecHP_eff") %>%
    full_join(select(scenAssump, "region", "scenario",
                     grep(paste0(enduseChar, ".elecHP"), colnames(scenAssump), value = TRUE)),
              by = c("region", "scenario", paste0(enduseChar, ".elecHP_eff_X_Asym")))

  hp <- df %>%
    filter(grepl(paste0(enduseChar, "\\.elec\\|(ue|fe)"), .data[["variable"]]),
           .data[["region"]] != "GLO") %>%
    spread("variable", "value") %>%
    left_join(effElec, by = c("scenario", "region", "period")) %>%
    group_by(across("region")) %>%
    mutate(effRHstart = min(min(.data[["efficiency"]]), effRHasym)) %>%
    ungroup() %>%
    left_join(scenAssumpHP, by = c("scenario", "region")) %>%
    mutate(
      effRH = pmin(expAsym(.data[["effRHstart"]],
                           effRHasym,
                           .data[["period"]],
                           tau = 25),
                   .data[["efficiency"]]),
      effHP = expAsym(hpEffHist,
                      .data[[paste0(enduseChar, ".elecHP_eff_X_Asym")]],
                      .data[["period"]]),
      shareHP = (.data[["efficiency"]] - .data[["effRH"]]) /
        (.data[["effHP"]] - .data[["effRH"]])
    ) %>%
    group_by(across("region")) %>%
    mutate(shareHPstart = .data[["shareHP"]][.data[["period"]] == 2020]) %>%
    ungroup()
  hp <- hp %>%
    mutate(
      shareHP = ifelse(.data[["period"]] > 2020,
                       expAsym(.data[["shareHPstart"]],
                               .data[[paste0(enduseChar, ".elecHP_share_X_Asym")]],
                               .data[["period"]]),
                       .data[["shareHP"]]),
      factor = ifelse(
        .data[["shareHP"]] != 0,
        (.data[["efficiency"]] - .data[["effRH"]]) /
          ((.data[["effHP"]] - .data[["effRH"]]) * .data[["shareHP"]]),
        1
      ),
      effHP = sqrt(.data[["factor"]]) * (.data[["effHP"]] - .data[["effRH"]]) +
        .data[["effRH"]],
      shareHP = sqrt(.data[["factor"]]) * .data[["shareHP"]]
    ) %>%
    gather("variable", "value", -"model", -"scenario", -"period", -"region", -"unit")
  hp <- hp %>%
    calc_addVariable_(
      stats::setNames(
        list(
          paste0("`", enduseChar, ".elec|fe` * shareHP"),
          paste0("`", enduseChar, ".elec|fe` * (1 - shareHP)"),
          paste0("`", enduseChar, ".elecHP|fe` * effHP"),
          paste0("`", enduseChar, ".elecRH|fe` * effRH")
        ),
        c(
          paste0("`", enduseChar, ".elecHP|fe`"),
          paste0("`", enduseChar, ".elecRH|fe`"),
          paste0("`", enduseChar, ".elecHP|ue`"),
          paste0("`", enduseChar, ".elecRH|ue`")
        )
      ),
      only.new = TRUE
    )

  # compute global sum
  hp %>%
    group_by(across(c("model", "scenario", "period", "variable", "unit"))) %>%
    reframe(
      value = sum(.data[["value"]]),
      region = "GLO"
    ) %>%
    rbind(hp)
}
