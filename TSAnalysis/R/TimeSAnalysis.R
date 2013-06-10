TimeSAnalysis <-
function (names, save.dir, thresholds, ...){
  
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
