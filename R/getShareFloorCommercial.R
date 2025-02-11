#' Compute share of commercial buildings
#'
#' Estimate share of commercial floor space for all countries by fitting a
#' SSgompertz model w.r.t. GDP per capita to IEA data.
#'
#' @param config scenario-wise parameter configuration
#' @param subtype source c("EDGE", "IEA", "raw_pop")
#' @param gdppop data.frame of gdp per capita
#' @param pop data.frame of population
#' @param floor0 data.frame with IEA floor space data
#' @param regionalmap data.frame with regional mapping
#'
#' @author Antoine Levesque, Hagen Tockhorn, Robin Hasse
#'
#' @importFrom quitte removeColNa as.quitte
#' @importFrom dplyr filter mutate select .data %>%
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom stats SSgompertz
#' @importFrom stats nls
#' @export

getShareFloorCommercial <- function(config,
                                    subtype = "EDGE",
                                    gdppop,
                                    pop,
                                    floor0,
                                    regionalmap) {
  # FUNCTIONS-------------------------------------------------------------------

  aggregate2IEA <- function(data,
                            subset2agg,
                            weights = NULL,
                            aggFun = sum,
                            mapping) {

    aggregate_map(data = data %>%
                    as.quitte() %>%
                    missingToNA(),
                  subset2agg = subset2agg,
                  mapping = mapping,
                  weights = weights,
                  by = c(region = "EDGE_EUR_ETP"),
                  fun = aggFun) %>%
      pivot_wider(names_from = "variable") %>%
      removeColNa()
  }



  # PARAMETER-------------------------------------------------------------------

  # scenario
  scen <- rownames(config) %>% unique()

  billion2million <- 1e3



  # PRE-PROCESS DATA------------------------------------------------------------

  # Load mappings to join ISO3c and the regions of IEA
  mapping <- unique(regionalmap[c("EDGE_EUR_ETP", "IEA_comm")])

  #--- Data is made compliant with config file

  # prepare gdppop
  gdppop <- gdppop %>%
    filter(.data[["scenario"]] == config[scen, "gdppopScen"]) %>%
    unique() %>%
    mutate(scenario = scen) %>%
    mutate(value = as.numeric(.data[["value"]])) %>%
    dplyr::select("region", "period", "scenario", "variable", "value")

  # prepare population
  pop <- pop %>%
    filter(.data[["scenario"]] == config[, "popScen"]) %>%
    unique() %>%
    mutate(scenario = scen) %>%
    mutate(value = as.numeric(.data[["value"]]))



  # CALCULATION-----------------------------------------------------------------

  # ensure that gdppop and pop have the same temporal and spatial coverage
  df <- joinReduceYearsRegion(gdppop, pop)

  # aggregate gdppop and pop to the IEA regions and spread for the regression
  gdppopIEA <- aggregate2IEA(df,
                             subset2agg = "gdppop",
                             weights = "pop",
                             mapping = mapping)

  popIEA    <- aggregate2IEA(pop,
                             subset2agg = "pop",
                             mapping = mapping)


  # Compute the ratio m2com/m2res and add the gdppop values
  # We take the SSP2 gdppop values for 2025
  floor0 <- floor0 %>%
    filter(.data[["period"]] != 2025) %>%
    pivot_wider(names_from = "variable") %>%
    left_join(gdppopIEA, by = c("region", "period"))

  floor <- floor0 %>%
    mutate(share = .data[["Services"]] / .data[["Residential"]],
           unit = NA) %>% #TODO: Should be 1
    select(-"Residential", -"Services")

  # build the output for raw_pop
  floorRawPop <- floor0 %>%
    left_join(popIEA, by = c("region", "period", "scenario")) %>%
    mutate("commCap" = .data[["Services"]] * billion2million / .data[["pop"]]) %>%
    pivot_longer(c("Services", "Residential", "gdppop", "commCap", "pop"),
                 names_to = "variable") %>%
    as.quitte() %>%
    missingToNA()


  # REGRESSION------------------------------------------------------------------

  # Estimate the Gompertz model : phi1 * exp(-phi2 * phi3^gdppop)
  # SSgompertz gives  starting values for  regression (needed in non linear regressions)
  # note: for some reason the European Union is an outlier and it works well without
  floorReg <- floor[-which(floor$region == "European Union"), ]

  regression <- nls(share ~ SSgompertz(gdppop, phi1, phi2, phi3), data = floorReg)

  floorPred <- pivot_wider(gdppop,
                           names_from  = all_of("variable"),
                           values_from = all_of("value"))




  # make the prediction and create the variable column
  floorPred$shareComm <- predict(regression, newdata = floorPred)

  floorPred <- floorPred %>%
    select(-"gdppop") %>%
    pivot_longer("shareComm", names_to = "variable") %>%
    mutate(scenario = scen)


  # OUTPUT----------------------------------------------------------------------

  # TODO: Is there a particular reason why this returns a quitte object for two subtypes and a data frame for the third one?
  return(switch(subtype,
                "EDGE" = floorPred %>%
                  as.data.frame() %>%
                  as.quitte() %>%
                  missingToNA(),
                "IEA" = as.data.frame(floor %>% dplyr::select(-"scenario", -"gdppop")),
                "raw_pop" = as.data.frame(floorRawPop)
  ))
}
