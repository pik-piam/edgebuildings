# Model for the projection of global energy demand in the buildings sector

R package **edgebuildings**, version **0.4.4**

[![CRAN status](https://www.r-pkg.org/badges/version/edgebuildings)](https://cran.r-project.org/package=edgebuildings) [![R build status](https://github.com/ricardarosemann/edgebuildings/workflows/check/badge.svg)](https://github.com/ricardarosemann/edgebuildings/actions) [![codecov](https://codecov.io/gh/ricardarosemann/edgebuildings/branch/master/graph/badge.svg)](https://app.codecov.io/gh/ricardarosemann/edgebuildings) [![r-universe](https://pik-piam.r-universe.dev/badges/edgebuildings)](https://pik-piam.r-universe.dev/builds)

## Purpose and Functionality


  The Energy Demand GEnerator projects energy demand for buildings both at the
  useful and final energy level. It covers the global demand and five energy services:
  space heating, space cooling, appliances and lighting (treated together) water heating
  and cooking. In a nutshell, it computes the useful energy intensity of each service
  in different regions, taking into account climate conditions as well as floor space
  demand. It then projects energy demand based on the assumption that, as developing
  countries grow economically, they will catch up with the energy service demand of
  developed countries. Further assumptions are made for the level beyond
  the current level of economic development (saturation or lower growth).


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("edgebuildings")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Tutorial

The package comes with a vignette describing the basic functionality of the package and how to use it. You can load it with the following command (the package needs to be installed):

```r
vignette("edgebuildings") # EDGE Buildings
```

## Questions / Problems

In case of questions / problems please contact Robin Hasse <robin.hasse@pik-potsdam.de>.

## Citation

To cite package **edgebuildings** in publications use:

Levesque A, Hasse R, Tockhorn H, Rosemann R, Führlich P (2025). "edgebuildings: Model for the projection of global energy demand in the buildings sector - Version 0.4.4."

A BibTeX entry for LaTeX users is

 ```latex
@Misc{,
  title = {edgebuildings: Model for the projection of global energy demand in the buildings sector - Version 0.4.4},
  author = {Antoine Levesque and Robin Hasse and Hagen Tockhorn and Ricarda Rosemann and Pascal Führlich},
  date = {2025-03-05},
  year = {2025},
}
```
