rm(list=ls())
renv::init()

packages <- c("interplot", "stringr",  "stargazer", "ggplot2", "lavaan", "xtable", "lmtest", "sandwich", "multiwayvcov", "data.table", "gridExtra", "memisc") ### add here packages you need to load
install.packages(packages)

### After initialization of the project you can install.packages and then run the following line to save the package
renv::snapshot()






