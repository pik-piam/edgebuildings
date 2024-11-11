#' Projection of final to useful energy efficiencies
#'
#' Calculate final (FE) to useful energy (UE) efficiencies for all combinations
#' of carrier and enduse. Projections are based on a model by De Stercke et al.
#' mainly driven by GDP per capita. It describes a S-shaped curve approaching
#' assumed efficiency levels. The parameters of that curve are derived by a
#' regression with observations of IEA data. The parameters are modified
#' according to scenario assumptions to reflect different narratives. The final
#' projection then follows from a linear interpolation between the results of
#' the unmodified and the modified projection.
#'
#' @references De Stercke, S. (2014). Dynamics of Energy Systems: A Useful
#' Perspective (Interim Report, p. 68). IIASA.
#' http://pure.iiasa.ac.at/id/eprint/11254/1/IR-14-013.pdf
#'
#' @references Scenario assumptions for space_cooling.elec: mostly based on
#' http://www.powerknot.com/how-efficient-is-your-air-conditioning-system.html
#'
#' @note Considering the rise in efficiency will reach a plateau, and the
#' heterogeneity of heat pumps, I take :
#' HP efficiency SSP1: 5, SSP2: 5, SSP3: 3.875, SSP4: a:3.875 b:5 c:5, SSP5: 6
#'
#' @note Scenario assumptions for space_heating.elec
#' I take the same COPs as in space_cooling, and I had an assumption on the
#' penetration of heat pumps in space_heating.elec. I take an efficiency of 1
#' for electric heaters
#' SSP1 = 80% -> 4.2 (1*20% + 80%* 5)
#' SSP2 = 50% -> 3
#' SSP3 = 25% -> 1.71875
#' SSP4a = 25% -> 1.71875
#' SSP4b = 40% -> 2.6
#' SSP4c = 60% -> 3.4
#' SSP5 = 75% -> 4.75
#' for water heaters
#' SSP1 = 80% -> 4.2 (1*20% + 80%* 5)
#' SSP2 = 50% -> 3
#' SSP3 = 25% -> 1.71875
#' SSP4a = 25% -> 1.71875
#' SSP4b = 40% -> 2.6
#' SSP4c = 60% -> 3.4
#' SSP5 = 75% -> 4.75
#'
#' @note For the initial share of heat pumps:
#' According to the IEA https://www.iea.org/tcep/buildings/heating/heatpumps/
#' Heat pumps meet app. 3% of heating needs in buildings
#' From the ETP data at the global level, heating needs sum to 62 EJ (WH + SH)
#' Electriticity for WH and SH is amounts to 7 EJ
#' (0.03*62)/7 = app 25%
#' The initial efficiency is therefore 0.75 + 0.25 * 3
#' I raise the scenario assumptions accordingly concerning the share of HP

#' @note the coefficients displayed on the plots correspond to the coefficients
#' of the function, as it is used by Simon De Stercke (that is why I take the
#' exponential of the negative of the coefficient of SSAsym). The maximum are
#' fairly close to the estimates of Simon, the minimum are not that far, and the
#' curvature parameter is very far. This is not very surprising since he uses
#' GDPppp $1990, and I $2005. Further, the gdppop used here differ from other
#' sources (like World Bank) which is annoying.
#' There are other reasons than gdppop for the differences observed and to
#' explain why the points here are not on a line. The regions are different than
#' in PFUDB. When we disaggregate to the ISO level, we make the assumption that
#' each country inside the group has the same efficiency, which means the same
#' income level, which is of course not the case. And when we reaggregate, we
#' this might make a difference. For appliances light, they mix refrigerators
#' (Low Heat), Light, stationary Power and others. So this is normal that they
#' do not correspond to any of these efficiencies.
#' It would be difficult to use their estimates directly because of the
#' different gdppop values, but this would be possible. I could justify redoing
#' the estimates to calibrate them to my gdppop data.
#' In their framework, there is actually no justification for a deviation from
#' the line. Here, they would only be justified by the composition of the
#' different efficiencies for appliances_light but not for the others. Still, I
#' would prefer not to rely on the estimates for appliances_light, and design on
#' own estimation here.
#'
#' @note The notes above are outdated have not yet been updated nor checked
#' (HT, 03/2024)
#'
#' @param config scenario-wise parameter configuration
#' @param eff data.frame with historic efficiencies
#' @param gdppop data.frame with historic and scenario gdp per capita
#' @param scenAssump region-wise scenario assumptions for regression parameters
#' @param scenAssumpSpeed scenario assumptions on temporal evolution
#' @param regPars regression parameters for enduse-carrier combinations
#'
#' @return data.frame with historic and scenario-projected fe->eu efficiencies
#'
#' @author Antoine Levesque, Hagen Tockhorn
#'
#' @importFrom dplyr mutate filter group_by ungroup any_of matches  %>% .data
#' @importFrom tidyr complete unite separate replace_na pivot_wider spread
#' @importFrom quitte removeColNa aggregate_map getPeriods
#' @importFrom stats as.formula na.omit approx
#' @export

getFEUEefficiencies <- function(config,
                                eff,
                                gdppop,
                                scenAssump,
                                scenAssumpSpeed,
                                regPars) {
  # PARAMETERS -----------------------------------------------------------------

  # scenario
  scen <- row.names(config) %>% unique()

  if (length(scen) > 1) {
    stop("Error in getFEUEefficiencies. Please provide single scenario data.")
  }

  # lower temporal boundary
  periodBegin <- config[scen, "periodBegin"] %>%
    unlist()


  # PRE-PROCESS DATA------------------------------------------------------------

  #--- Data is made compliant with config file

  # gdppop
  gdppop <- gdppop  %>%
    filter(.data[["variable"]] == config[, "gdppopScen"]) %>%
    unique() %>%
    sepVarScen() %>%
    mutate(scenario = scen)


  # scenario parameter assumptions
  scenAssump <- scenAssump %>%
    filter(.data[["scenario"]] == scen)


  # scenario convergence speed assumptions
  scenAssumpSpeed <- scenAssumpSpeed %>%
    filter(.data[["scenario"]] == scen)


  # calculate temporal convergence shares
  lambda <- compLambdaScen(scenAssumpSpeed, startYearVector = periodBegin)



  # PROCESS DATA ---------------------------------------------------------------

  # pre-process gdppop
  gdppop <- gdppop %>%
    mutate(scenario = ifelse(.data[["period"]] <= config[scen, "endOfHistory"],
                             "history",
                             scen)) %>%
    filter(.data[["period"]] %in% getPeriods(lambda)) %>%
    select("region", "period", "scenario", "variable", "value") %>%
    filter(.data[["period"]] >= periodBegin)

  # pre-process efficiencies
  eff <- eff %>%
    select(-"variable") %>%
    unite("variable", "enduse", "carrier", sep = ".") %>%
    select("region", "period", "scenario", "variable", "value") %>%
    mutate(scenario = "history")

  # rbind gdppop and efficiencies
  data <- rbind(eff, gdppop)

  # get variables
  vars <- data %>%
    filter(!(.data[["variable"]] == "gdppop")) %>%
    pull("variable") %>%
    unique()


  # project efficiencies
  pred <- do.call(
    "rbind",
    lapply(vars, function(v) {
      tmp <- makeEfficiencyProjections(data, v, regPars, scenAssump, lambda)
      return(tmp)
    }
    )
  )


  # OUTPUT ---------------------------------------------------------------------

  pred <- pred %>%
    filter(.data[["variable"]] != "gdppop") %>%
    mutate(variable = as.character(.data[["variable"]]),
           scenario = scen) %>%
    separate("variable", c("enduse", "carrier"), sep = "\\.") %>%
    as.data.frame() %>%
    as.quitte() %>%
    missingToNA()

  return(pred)
}


# INTERNAL FUNCTIONS -----------------------------------------------------------

#' Project FE->EU efficiencies for different scenarios
#'
#' Efficiencies of a specific enduse-carrier combination are projected to an S-shaped
#' curve with gdppop as a free parameter.Pre-calculated regression parameters from
#' historic data in mredgebuildings are used for projecting future values.
#' Specific regression parameters can be adjusted ith the \code{scenAssump} parameters.
#'
#' @param df data.frame containing data on gdppop and historic efficiencies
#' @param var enduse-carrier combination as \code{var = <enduse>.<carrier>}
#' @param regPars regression parameters for enduse-carrier combination
#' @param scenAssump scenario assumptions for regression parameters
#' @param lambda temporal convergence shares
#'
#' @return data.frame containing historic and projected efficiencies
#'
#' @importFrom dplyr group_modify filter select mutate left_join group_by across all_of
#' @importFrom tidyr unite spread pivot_longer
#' @importFrom quitte removeColNa
#' @importFrom stats SSasymp
#' @importFrom data.table :=

makeEfficiencyProjections <- function(df, var, regPars, scenAssump, lambda) {

  varX <- paste0(var, "_X_")

  regPars <- regPars %>%
    unite(col = "variable", "enduse", "carrier", sep = ".") %>%
    filter(.data[["variable"]] == var) %>%
    select(-"variable")

  data <- df %>%
    # filter data for variable
    removeColNa() %>%
    filter(.data[["variable"]] %in% c(var, "gdppop")) %>%
    spread("variable", "value") %>%
    removeColNa() %>%

    # join regression parameters
    cbind(regPars) %>%

    # predict values with unchanged parameters
    mutate(pred = SSasymp(.data[["gdppop"]], .data[["Asym"]], .data[["R0"]], .data[["lrc"]]))


  # extract scenario-specific efficiency assumptions
  parsNames <- colnames(scenAssump)[grepl(varX, colnames(scenAssump))]

  # make parameter corrections
  if (length(parsNames) > 0) {
    # get values
    parsVals <- scenAssump %>%
      select(all_of(c("region", "scenario", parsNames)))

    # rename columns to match regression parameter names
    colnames(parsVals) <- gsub(varX, "", colnames(parsVals))

    # replace regression parameters in "data"
    parsNames <- gsub(varX, "", parsNames)
    data <- data %>%
      select(-all_of(parsNames)) %>%
      left_join(parsVals, by = c("region", "scenario"))
  }

  data <- data %>%
    # make prediction with corrected parameters
    mutate(predCorr = SSasymp(.data[["gdppop"]], .data[["Asym"]], .data[["R0"]], .data[["lrc"]])) %>%
    select(-all_of(c("Asym", "R0", "lrc"))) %>%

    # create correction factor to balance-out deviations w.r.t. historic data
    group_by(across(all_of("region"))) %>%
    mutate(factor = .data[[var]] / .data[["pred"]]) %>%

    # regress "factor" for all periods
    group_modify(~ extrapolateMissingPeriods(.x, key = "factor", slopeOfLast = 5)) %>%
    ungroup() %>%

    # correct prediction deviations
    mutate(pred = .data[["pred"]] * .data[["factor"]]) %>%

    # final projection: transition from unmodified to modified
    left_join(lambda, by = c("region", "period", "scenario")) %>%
    mutate(!!var := ifelse(!is.na(.data[[var]]),
                           .data[[var]],
                           .data[["predCorr"]] * .data[["fullconv"]] +
                             .data[["pred"]] * (1 - .data[["fullconv"]]))) %>%

    pivot_longer(cols = c(var), names_to = "variable", values_to = "value")

  # clean dataset
  data <- data %>%
    select(intersect(colnames(df), colnames(data)))

  return(data)
}
