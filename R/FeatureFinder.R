#'This function exacts the features from a list of lists of ts data in columns
#'
#'
FeatureFinder <- function(ts.cols.paired, a.thresh, r.thresh){

  num.sets <- (length(ts.cols.paired))
  feat.out <- list(list(),length= num.sets)
  for (k in 1:num.sets){
      for (j in 1:2){
      #print(a.thresh)
      data.holder <- ts.cols.paired[[k]][[j]][,-1]
      feat.out[[k]][[j]] <- FeaturesMaker(data.holder,
                                         a.thresh, r.thresh)
    }
  }
  
  return(feat.out)
  
}