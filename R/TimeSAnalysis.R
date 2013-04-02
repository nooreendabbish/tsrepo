source('ReadNaRescale.R')
source('GraphMaker.R')
source('FeatureFinder.R')
source('FeaturesMaker.R')
source('DoKSTest.R')
library(lattice)

GetEllipsis <- function (...){
  output.list <- as.list(substitute(list(...)))[-1L]
  return(output.list)
}

#'Takes a list of paths to ts data and generates analysis files
#'@param names this is a list of: the group names, the y-axis label
#'@param save.dir this is the path to the folder where graphs will go
#'@param ... list of paths to ts data
#'@description this is the mother wrapper for this package
TimeSAnalysis <- function (names, save.dir, thresholds, ...){
  
  input.list <- GetEllipsis(...)
  
  ts.cols.paired <- list()
  for (k in seq(1, length(input.list), by=2)){

    ts.cols.paired[[k]] <- ReadNaRescale(as.character(input.list[[k]]), 
                                         as.character(input.list[[k+1]]))

  }
  
  
  feat.list <- FeatureFinder(ts.cols.paired, thresholds[1], thresholds[2])
  
  GraphMaker1(ts.cols.paired, names, save.dir)
  ksout <- DoKSTest(ts.cols.paired)
  
  pcaout <- GraphMaker2(feat.list, names, save.dir)
  
  list(ks.results = ksout,
       features.pca.results = pcaout)
}
