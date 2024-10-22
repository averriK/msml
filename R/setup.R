library(data.table)
setDTthreads(threads=0) # nolint
library(readxl)
library(reticulate)

library(stringdist)
library(caret) |> suppressPackageStartupMessages()
library(caretEnsemble)
library(xgboost)
library(RColorBrewer)
library(doParallel)
library(buildPlot)
library(highcharter)

library(seewave)
library(TTR)
library(pracma)
library(gmsp)
