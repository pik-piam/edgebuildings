#' Visualise scenarios
#'
#' This function should be replaced by a mif reporting and mip plotting at some
#' point.
#'
#' @author Robin Hasse
#'
#' @param path character, path to run folder or named vector of paths with
#'   version tags as names
#' @param outputFile path character, path to pdf that is created. If NULL, the
#'   file projection.pdf is created in the first path provided in \code{path}.
#'
#' @importFrom madrat toolGetMapping
#' @importFrom dplyr %>% .data filter group_by across summarise
#' @importFrom quitte replace_column
#' @importFrom tidyr pivot_wider
#' @importFrom ggpubr ggarrange
#' @importFrom RColorBrewer brewer.pal
#' @importFrom utils head
#' @importFrom grDevices pdf dev.off
#' @importFrom ggplot2 theme_bw theme ggplot geom_line aes scale_y_continuous
#'   ggtitle facet_wrap element_blank scale_linetype_manual scale_x_continuous
#'   facet_wrap scale_color_manual geom_col facet_grid scale_fill_manual
#'   element_text geom_tile coord_equal theme_classic
#' @export

visualiseScenarios <- function(path, outputFile = NULL) {


  # READ -----------------------------------------------------------------------

  colorScale <- list(
    Use = c(
      `space_heating`    = "#8B3458",
      `appliances_light` = "#E0CB09",
      `water_heating`    = "#1452F5",
      `cooking`          = "#E00962",
      `space_cooling`    = "#09BCE0"
    ),
    Carrier = c(
      `biomod`  = "#005900",
      `biotrad` = "#7f7f7f",
      `coal`    = "#0c0c0c",
      `elec`    = "#ffb200",
      `elecRH`  = "#f58231",
      `elecHP`  = "#3cb44b",
      `heat`    = "#cc0000",
      `natgas`  = "#999959",
      `petrol`  = "#0000cc"
    )
  )

  uses <- names(colorScale$Use)
  carriers <- names(colorScale$Carrier)

  ## scenarios ====

  data <- switch(
    as.character(length(path)),
    `0` = {
      stop("You need to provide at least one path.")
    },
    `1` = {
      readProjections(path) %>%
        mutate(version = NA)
    },
    do.call(rbind, lapply(names(path), function(v) {
      readProjections(path[[v]]) %>%
        mutate(version = v)
    }))
  )

  data$scenario <- factor(data$scenario,
                          sort(unique(data$scenario)))


  ## mapping ====

  mapping <- toolGetMapping(name  = "regionmappingEDGE.csv",
                            type  = "regional",
                            where = "mredgebuildings")
  eurRegions <- mapping %>%
    filter(.data[["RegionCode"]] == "EUR") %>%
    getElement("RegionCodeEUR")
  
  plottedRegions <- mapping %>%
    select("RegionCode") %>%
    unique() %>%
    pull()



  # FUNCTIONS ------------------------------------------------------------------



  .aggREMIND <- function(df, aggMethod = sum, recover = NULL) {
    recover <- intersect(c(eurRegions, "GLO"), recover)
    mask <- mapping %>%
      select("RegionCode", "RegionCodeEUR_ETP") %>%
      unique()
    out <- df %>%
      filter(.data[["region"]] != "GLO") %>%
      replace_column(mask, region = "RegionCodeEUR_ETP", "RegionCode") %>%
      group_by(across(-all_of("value"))) %>%
      summarise(value = aggMethod(.data[["value"]]), .groups = "drop")
    if (length(recover > 0)) {
      out <- rbind(out, filter(df, .data[["region"]] %in% recover))
    }
    return(out)
  }


  .regions <- function(df) {
    regions <- sort(unique(df[["region"]]))
    regions <- regions[!is.na(regions)]
    c(regions[regions == "GLO"], regions[regions != "GLO"])
  }


  .addBookmark <- function(bookmark, title, page, level) {
    rbind(bookmark,
          data.frame(title = title, page = page, level))
  }


  .getColors <- function(ColorCol) {
    colorScale <- c(brewer.pal(9, "Set1")[c(1:5, 7:9)],
                    brewer.pal(12, "Set3")[c(1, 3:12)],
                    brewer.pal(12, "Paired"))
    colorVals <- unique(ColorCol)
    colorScale <- setNames(colorScale[seq_along(colorVals)], colorVals)
    return(colorScale)
  }


  # LAYOUT ---------------------------------------------------------------------

  ## linetype ====

  linetypeScale <- setNames(seq_along(path), names(path))


  ## plot theme ====

  linePlot <- function(df,
                       title = NULL,
                       subtitle = NULL,
                       xAxisLabel = NULL,
                       yAxisLabel = NULL,
                       linetype = "version",
                       linetypeScale = NULL,
                       facet = NULL,
                       facetNcol = NULL,
                       color = "scenario",
                       colorScale = NULL,
                       x = "period",
                       y = "value") {

    if (is.null(colorScale)) {
      colorScale <- .getColors(df[[color]])
    }


    p <- df %>%
      ggplot() +
      geom_line(aes(.data[[x]], .data[[y]],
                    colour = .data[[color]],
                    linetype = .data[[linetype]]),
                size = .8) +
      ggtitle(title, subtitle) +
      scale_y_continuous(yAxisLabel,
                         expand = c(0, 0, 0.025, 0),
                         limits = c(0, NA)) +
      scale_x_continuous(xAxisLabel) +
      scale_linetype_manual(
        values = linetypeScale,
        guide = if (length(linetypeScale) == 1) "none" else "legend",
        na.value = "solid"
      ) +
      scale_color_manual(values = colorScale) +
      theme_bw() +
      theme(strip.background = element_blank())

    if (!is.null(facet)) {
      p <- p +
        facet_wrap(facet, scales = "free_y", ncol = facetNcol)
    }

    return(p)
  }


  linePlots <- function(df,
                        title = NULL,
                        xAxisLabel = "",
                        yAxisLabel = NULL,
                        linetype = "version",
                        linetypeScale = NULL,
                        facet = NULL,
                        facetNcol = NULL,
                        color = "scenario",
                        colorScale = NULL,
                        x = "period",
                        y = "value") {

    for (r in .regions(df)) {
      print(linePlot(df %>%
                       filter(.data[["region"]] == r),
                     title = title,
                     subtitle = r,
                     xAxisLabel = xAxisLabel,
                     yAxisLabel = yAxisLabel,
                     linetype = linetype,
                     linetypeScale = linetypeScale,
                     facet = facet,
                     facetNcol = facetNcol,
                     color = color,
                     colorScale = colorScale,
                     x = x,
                     y = y))
    }

  }



  # PLOTS ----------------------------------------------------------------------

  # track page number for bookmarks
  i <- 1
  bookmarks <- data.frame()


  # start PDF
  pdfPath <- if (is.null(outputFile)) {
    head(file.path(path, "projections.pdf"), 1)
  } else {
    outputFile
  }

  message("writing PDF: ", pdfPath)
  pdf(pdfPath,
      width = 11.5, height = 8, paper = "a4r",
      title = "EDGE-B results")


  ## file reference table ====

  bookmarks <- .addBookmark(bookmarks, "File Reference table", i, 1)

  path <- unlist(lapply(path, normalizePath))

  pData <- data %>%
    group_by(across(all_of(c("version", "scenario")))) %>%
    summarise(.groups = "drop") %>%
    mutate(path = if (length(path) == 1) {
      path
    } else {
      path[.data[["version"]]]
    })

  p <- pData %>%
    ggplot(aes(.data[["scenario"]], 0)) +
    geom_tile(aes(fill = .data[["scenario"]]),
              color = "white") +
    facet_wrap(c("version", "path"), ncol = 1) +
    theme_classic() +
    scale_fill_manual(values = .getColors(pData[["scenario"]]),
                      guide = "none") +
    coord_equal() +
    theme(axis.line = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_text(angle = 90),
          axis.text.y = element_blank(),
          strip.background = element_blank())
  print(p)

  i <- i + 1


  ## floor space ====

  bookmarks <- .addBookmark(bookmarks, "Floor space", i, 1)
  pData <- data %>%
    filter(.data[["variable"]] == "buildings") %>%
    mutate(value = .data[["value"]] / 1000) %>%  # million m2 -> billion m2
    .aggREMIND(recover = c("DEU", "GLO"))

  linePlots(pData,
            title = "Floor space",
            yAxisLabel = "billion m2",
            linetypeScale = linetypeScale)

  i <- i + length(.regions(pData))


  ## per capita Floorspace ====

  bookmarks <- .addBookmark(bookmarks, "Floor space per capita", i, 1)


  pData <- data %>%
    filter(.data[["variable"]] %in% c("buildings", "pop", "gdp")) %>%
    .aggREMIND(recover = "DEU") %>%
    select(-"unit") %>%
    pivot_wider(names_from = "variable") %>%
    mutate(gdppop = .data[["gdp"]] / .data[["pop"]],
           buildings_pop = .data[["buildings"]] / .data[["pop"]]) # million m2/millioncap = m2/cap


  ### all regions ####

  p <- lapply(list("period", "gdppop"), function(x) {
    linePlot(
      pData,
      title = switch(x, period = paste("Total floor space per capita")),
      xAxisLabel = x,
      yAxisLabel = switch(x, period = "m2/cap"),
      linetypeScale = linetypeScale,
      facet = "scenario",
      color = "region",
      x = x,
      y = "buildings_pop"
    ) + switch(x, gdppop = theme(axis.text.y = element_blank(),
                                 axis.ticks.y = element_blank()))
  })
  print(ggarrange(plotlist = p, ncol = 2, common.legend = TRUE, legend = "right",
                  widths = c(1.2, 1), align = "hv"))

  i <- i + 1


  ### regional ####

  for (r in .regions(pData)) {
    p <- lapply(list("period", "gdppop"), function(x) {
      linePlot(
        filter(pData, .data[["region"]] %in% r),
        title = switch(x, period = paste("Total floor space per capita")),
        subtitle = switch(x, period = r),
        xAxisLabel = x,
        yAxisLabel = switch(x, period = "m2/cap"),
        linetypeScale = linetypeScale,
        color = "scenario",
        x = x,
        y = "buildings_pop"
      ) + switch(x, gdppop = theme(axis.text.y = element_blank(),
                                   axis.ticks.y = element_blank()))
    })
    print(ggarrange(plotlist = p, ncol = 2, common.legend = TRUE, legend = "right",
                    widths = c(1.2, 1), align = "hv"))
  }

  i <- i + length(.regions(pData))


  ## U-value ====

  bookmarks <- .addBookmark(bookmarks, "U-value", i, 1)
  pData <- data %>%
    filter(.data[["variable"]] %in% c("gdppop", "uvalue"),
           .data[["region"]] %in% c(plottedRegions, "DEU", "GLO")) %>%
    pivot_wider(names_from = "variable")


  ### all regions ####

  p <- lapply(list("period", "gdppop"), function(x) {
    linePlot(
      pData,
      title = switch(x, period = paste("U-value")),
      xAxisLabel = x,
      yAxisLabel = switch(x, period = "W/m2/K"),
      linetypeScale = linetypeScale,
      facet = "scenario",
      color = "region",
      x = x,
      y = "uvalue"
    ) + switch(x, gdppop = theme(axis.text.y = element_blank(),
                                 axis.ticks.y = element_blank()))
  })
  print(ggarrange(plotlist = p, ncol = 2, common.legend = TRUE, legend = "right",
                  widths = c(1.2, 1), align = "hv"))

  i <- i + 1


  ## HDD ====

  bookmarks <- .addBookmark(bookmarks, "HDD", i, 1)
  pData <- data %>%
    filter(.data[["variable"]] == "HDD",
           .data[["region"]] %in% c(plottedRegions, "DEU", "GLO")) %>%
    pivot_wider(names_from = "variable")


  ### all regions ####

  print(linePlot(
    pData,
    title = "HDD",
    xAxisLabel = "period",
    yAxisLabel = "K d/yr",
    linetypeScale = linetypeScale,
    facet = "scenario",
    color = "region",
    x = "period",
    y = "HDD"
  ))

  i <- i + 1

  ## CDD ====

  bookmarks <- .addBookmark(bookmarks, "CDD", i, 1)
  pData <- data %>%
    filter(.data[["variable"]] == "CDD",
           .data[["region"]] %in% c(plottedRegions, "DEU", "GLO")) %>%
    pivot_wider(names_from = "variable")


  ### all regions ####

  print(linePlot(
    pData,
    title = "CDD",
    xAxisLabel = "period",
    yAxisLabel = "K d/yr",
    linetypeScale = linetypeScale,
    facet = "scenario",
    color = "region",
    x = "period",
    y = "CDD"
  ))

  i <- i + 1


  ## carrier efficiency and shares ====

  # Only generate these plots if only one path is given
  if (length(linetypeScale) == 1) {

    bookmarks <- .addBookmark(bookmarks, "Carrier projections", i, 1)

    for (projType in c("efficiency", "share")) {

      bookmarks <- .addBookmark(bookmarks, projType, i, 2)


      ### by end use ####

      pData <- data %>%
        filter(grepl(paste0("(", paste(uses, collapse = "|"), ")\\.(",
                            paste(carriers, collapse = "|"), ")\\|", projType),
                     .data[["variable"]])) %>%
        .aggREMIND(recover = c("GLO", "DEU"), aggMethod = mean) %>%
        tidyr::separate_wider_delim("variable", delim = ".", names = c("enduse", "carrier")) %>%
        mutate(carrier = sub(paste0("\\|", projType), "", .data[["carrier"]]))
      linePlots(pData,
                title = paste("Carrier", projType, "by end use"),
                yAxisLabel = "",
                linetype = "scenario",
                linetypeScale = setNames(seq_along(unique(pData$scenario)), unique(pData$scenario)),
                color = "carrier",
                colorScale = colorScale[["Carrier"]],
                facet = "enduse")

      i <- i + length(.regions(pData))

    }
  }

  ## energy demand ====

  bookmarks <- .addBookmark(bookmarks, "Energy demand", i, 1)

  for (enType in c("fe", "ue")) {

    bookmarks <- .addBookmark(bookmarks, toupper(enType), i, 2)

    ### total ####

    bookmarks <- .addBookmark(bookmarks, "Total", i, 3)

    pData <- data %>%
      filter(.data[["variable"]] == enType) %>%
      .aggREMIND(recover = c("GLO", "DEU"))

    linePlots(pData,
              title = paste("Total", toupper(enType), "demand"),
              yAxisLabel = "EJ/yr",
              linetypeScale = linetypeScale)

    i <- i + length(.regions(pData))


    ### by end use ####

    bookmarks <- .addBookmark(bookmarks, "by end use", i, 3)

    pData <- data %>%
      filter(.data[["variable"]] %in% paste(uses, enType, sep = "|")) %>%
      mutate(variable = sub(paste0("^(.*)\\|", enType, "$"), "\\1",
                            .data[["variable"]])) %>%
      .aggREMIND(recover = c("GLO", "DEU"))
    linePlots(pData,
              title = paste(toupper(enType), "demand by end use"),
              yAxisLabel = "EJ/yr",
              linetypeScale = linetypeScale,
              facet = "variable")

    i <- i + length(.regions(pData))


    ### by carrier ####

    bookmarks <- .addBookmark(bookmarks, "by carrier", i, 3)

    pData <- data %>%
      filter(.data[["variable"]] %in% paste(carriers, enType, sep = "|")) %>%
      mutate(variable = sub(paste0("^(.*)\\|", enType, "$"), "\\1",
                            .data[["variable"]])) %>%
      .aggREMIND(recover = c("GLO", "DEU"))
    linePlots(pData,
              title = paste(toupper(enType), "demand by carrier"),
              yAxisLabel = "EJ/yr",
              linetypeScale = linetypeScale,
              facet = "variable")

    i <- i + length(.regions(pData))

  }


  ## per capita energy ====

  bookmarks <- .addBookmark(bookmarks, "Energy demand per capita", i, 1)


  pData <- data %>%
    filter(.data[["variable"]] %in% c("fe", "ue", "pop", "gdp")) %>%
    .aggREMIND(recover = "DEU") %>%
    select(-"unit") %>%
    pivot_wider(names_from = "variable") %>%
    mutate(gdppop = .data[["gdp"]] / .data[["pop"]],
           ue_pop = .data[["ue"]] * 1000 / .data[["pop"]], # EJ/millioncap -> GJ/cap
           fe_pop = .data[["fe"]] * 1000 / .data[["pop"]]) # EJ/millioncap -> GJ/cap

  for (enType in c("fe", "ue")) {

    bookmarks <- .addBookmark(bookmarks, toupper(enType), i, 2)

    bookmarks <- .addBookmark(bookmarks, "Total", i, 3)


    ### all regions ####

    p <- lapply(list("period", "gdppop"), function(x) {
      linePlot(
        pData,
        title = switch(x, period = paste("Total", toupper(enType), "demand per capita")),
        xAxisLabel = x,
        yAxisLabel = switch(x, period = "GJ/yr/cap"),
        linetypeScale = linetypeScale,
        facet = "scenario",
        color = "region",
        x = x,
        y = paste0(enType, "_pop")
      ) + switch(x, gdppop = theme(axis.text.y = element_blank(),
                                   axis.ticks.y = element_blank()))
    })
    print(ggarrange(plotlist = p, ncol = 2, common.legend = TRUE, legend = "right",
                    widths = c(1.2, 1), align = "hv"))

    i <- i + 1


    ### regional ####

    for (r in .regions(pData)) {
      p <- lapply(list("period", "gdppop"), function(x) {
        linePlot(
          filter(pData, .data[["region"]] %in% r),
          title = switch(x, period = paste("Total", toupper(enType), "demand per capita")),
          subtitle = switch(x, period = r),
          xAxisLabel = x,
          yAxisLabel = switch(x, period = "GJ/yr/cap"),
          linetypeScale = linetypeScale,
          color = "scenario",
          x = x,
          y = paste0(enType, "_pop")
        ) + switch(x, gdppop = theme(axis.text.y = element_blank(),
                                     axis.ticks.y = element_blank()))
      })
      print(ggarrange(plotlist = p, ncol = 2, common.legend = TRUE, legend = "right",
                      widths = c(1.2, 1), align = "hv"))
    }

    i <- i + length(.regions(pData))


    ### by enduse ###

    bookmarks <- .addBookmark(bookmarks, "by enduse", i, 3)

    pDataUse <- data %>%
      # Filter for relevant variables
      filter(.data[["variable"]] %in% c("gdp", "pop", paste(uses, enType, sep = "|"))) %>%
      select(-"unit") %>%
      .aggREMIND(recover = "DEU") %>%
      pivot_wider(names_from = "variable") %>%
      mutate(gdppop = .data[["gdp"]] / .data[["pop"]],
             across(.cols = matches("^[^|]+\\|[^|]+$"),
                    .fns = ~ .x * 1e3 / .data[["pop"]],  # EJ/yr/cap -> GJ/yr/cap
                    .names = "{.col}_pop"))

    for (eu in uses) {
      p <- lapply(list("period", "gdppop"), function(x) {
        linePlot(
          pDataUse,
          title = switch(x, period = paste(eu, toupper(enType), "demand per capita")),
          xAxisLabel = x,
          yAxisLabel = switch(x, period = "GJ/yr/cap"),
          color = "region",
          facet = "scenario",
          x = x,
          y = paste0(eu, "|", enType, "_pop")
        ) + switch(x, gdppop = theme(axis.text.y = element_blank(),
                                     axis.ticks.y = element_blank()))
      })
      print(ggarrange(plotlist = p, ncol = 2, common.legend = TRUE, legend = "right",
                      widths = c(1.2, 1), align = "hv"))

    }

    i <- i + length(uses)


    ### by carrier ###

    bookmarks <- .addBookmark(bookmarks, "by carrier", i, 3)

    pDataCarrier <- data %>%
      # Filter for relevant variables
      filter(.data[["variable"]] %in% c("gdp", "pop", paste(carriers, enType, sep = "|"))) %>%
      select(-"unit") %>%
      .aggREMIND(recover = "DEU") %>%
      pivot_wider(names_from = "variable") %>%
      mutate(gdppop = .data[["gdp"]] / .data[["pop"]],
             across(.cols = matches("^[^|]+\\|[^|]+$"),
                    .fns = ~ .x * 1e3 / .data[["pop"]],  # EJ/yr/cap -> GJ/yr/cap
                    .names = "{.col}_pop"))

    for (c in carriers) {
      p <- lapply(list("period", "gdppop"), function(x) {
        linePlot(
          pDataCarrier,
          title = switch(x, period = paste(c, toupper(enType), "demand per capita")),
          xAxisLabel = x,
          yAxisLabel = switch(x, period = "GJ/yr/cap"),
          color = "region",
          facet = "scenario",
          x = x,
          y = paste0(c, "|", enType, "_pop")
        ) + switch(x, gdppop = theme(axis.text.y = element_blank(),
                                     axis.ticks.y = element_blank()))
      })
      print(ggarrange(plotlist = p, ncol = 2, common.legend = TRUE, legend = "right",
                      widths = c(1.2, 1), align = "hv"))

    }

    i <- i + length(carriers)

  }


  ## macro ====

  bookmarks <- .addBookmark(bookmarks, "Macro drivers", i, 1)

  pData <- pData %>%
    select(-"fe", -"ue", -"fe_pop", -"ue_pop") %>%
    pivot_longer(c("gdp", "pop", "gdppop"), names_to = "variable")
  linePlots(pData,
            "Macro drivers",
            linetypeScale = linetypeScale,
            facet = "variable",
            facetNcol = 1)

  i <- i + length(.regions(pData))


  ## energy mixes ====

  bookmarks <- .addBookmark(bookmarks, "Energy mixes", i, 1)

  for (enType in c("fe", "ue")) {
    for (perCapita in c(FALSE, TRUE)) {

      bookmarks <- .addBookmark(bookmarks, paste(toupper(enType), if (isTRUE(perCapita)) "per capita"), i, 2)

      pData <- data %>%
        filter(.data[["variable"]] %in% c(paste(c(uses, carriers), enType, sep = "|"), "pop"),
               .data[["period"]] %% 5 == 0) %>%
        mutate(variable = sub(paste0("^(.*)\\|", enType, "$"), "\\1",
                              .data[["variable"]])) %>%
        .aggREMIND(recover = c("GLO", "DEU"))

      if (isTRUE(perCapita)) {
        allVars <- c(uses, carriers)
        pData <- pData %>%
          calc_addRatio(paste(allVars, "pop", sep = "_"), allVars, "pop", factor = 1000, only.new = TRUE) %>%
          mutate(variable = sub("_pop", "", .data[["variable"]]))
      }

      # avoid NA as facet title
      if (all(is.na(pData$version))) {
        pData$version <- ""
      }

      for (r in .regions(pData)) {
        p <- lapply(c("Carrier", "Use"), function(by) {
          pData <- pData %>%
            filter(.data[["region"]] == r,
                   .data[["variable"]] %in% switch(
                     by,
                     Carrier = carriers,
                     Use = uses
                   ))
          pData %>%
            ggplot() +
            geom_col(aes(.data[["period"]], .data[["value"]],
                         fill = .data[["variable"]])) +
            facet_grid(.data[["scenario"]] ~ .data[["version"]]) +
            scale_y_continuous(switch(by, Carrier = if (isTRUE(perCapita)) "GJ/yr/cap" else "EJ/yr"),
                               expand = c(0, 0, 0.05, 0)) +
            scale_x_continuous(NULL, expand = c(0, 0)) +
            scale_fill_manual(values = colorScale[[by]], name = by) +
            ggtitle(switch(by, Carrier = paste(paste(toupper(enType), "demand"),
                                               if (isTRUE(perCapita)) "per capita")),
                    switch(by, Carrier = r)) +
            theme_bw() +
            theme(panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  strip.background = element_blank()) +
            switch(by,
                   Use = theme(axis.text.y = element_blank(),
                               axis.ticks.y = element_blank()))
        })
        print(ggarrange(plotlist = p, ncol = 2, widths = c(1.1, 1), align = "hv"))
      }

      i <- i + length(.regions(pData))

    }

  }


  # end PDF
  dev.off()

  message("PDF written: ", pdfPath)


  # WRITE BOOKMARKS ------------------------------------------------------------

  try(xmpdf::set_bookmarks(bookmarks = bookmarks, input = pdfPath))

}
