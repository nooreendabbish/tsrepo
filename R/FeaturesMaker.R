####FeatureBinder####
#'@description Parses the list of lists of ts data and sends it 
#'to feature maker
#'@param ts.cols.paired
#'@param a.thresh the absolute threshold
#'@param r.thresh fraction 
#'
FeatureBinder <- function(ts.cols.paired, a.thresh, r.thresh){
  
  num.sets <- (length(ts.cols.paired))
  feat.out <- list(list(),length= num.sets)
  for (k in 1:num.sets){
    for (j in 1:2){
      print(a.thresh)
      data.holder <- ts.cols.paired[[k]][[j]][,-1]
      feat.out[[k]][[j]] <- FeaturesMaker(data.holder,
                                          a.thresh, r.thresh)
    }
  }
  
  return(feat.out)
  
}

####Tiny functions ####
TimeToThresh <- function(x, thresh){
  
  default <- length(x)+1
  first.stab <- which(x>thresh)
  if (length(first.stab) == 0){
    first.stab <- default}
  return(first.stab[1])
}

##Time to Rel #
#'@description accepts a ts, relative threshold (fraction of max),
#'and it turns the index value when the ts goes above the relative threshold. 
#'
#' Note that this function is meaningless of many types of ts data.
#'  
TimeToRel <- function(x, rel.frac){
  
  thresh <- (rel.frac*max(x))
  return(TimeToThresh(x, thresh))
}

##TimeToDec ##
#'@description takes a time series and threshold value.
#'returns dirst index after the max at which time series
#'values are below a critical threshold. 
TimeToDec <- function(ts, a.thresh){
  tmax <- which.max(ts)
  print(tmax)
  t.decay <- which (ts[tmax:length(ts)]<a.thresh)
  if(is.na(t.decay[1])) tdecay <-length(ts)+2
  return(t.decay[1])
}


##TimeDecRel ####
#'@description takes a ts and relative threshold(fraction)
#'and returns the time point after the max where the ts has
#'decreased below the threshold.
TimeDecRel <- function(ts, r.thresh){
  thresh <- r.thresh*(max(ts))
  t.max <- which.max(ts)
  t.decay <- which (ts[t.max:length(ts)]<r.thresh)
  if(is.na(t.decay[1])) t.decay <- length(ts)+2
  return(t.decay)
  print(t.decay)
}


#### FeaturesMaker Header ####
#'Given a matrix with time series data arranged in columns,
#'this function produces a matrix of 'features' in each series.
#'Features include min, means, medians, etc.
#'@export 
#'@return output.matrix matrix quantifying features (by col) and
#'each ts by row
#'@param matrix.input contains time series (in ALL columns)
#'@seealso TimeSeriesAnalysis-package
FeaturesMaker <- function (matrix.in, a.thresh, r.thresh){
  
  ##initialize and define.
  number.features <- 9
#  matrix.in <- matrix.input[,2:ncol(matrix.input)]
  rows.in <- nrow(matrix.in)
  cols.in <- ncol(matrix.in)
  output.matrix <- matrix(, nrow = cols.in, ncol = number.features)
  colnames(output.matrix) <- list("Mins", #1
                                  "Means",#2
                                  "Medians",#3
                                  "Mean", #4
                                  "StartValue",#) #5
                                  "t.absThr", #) #6
                                  "t.relThr", #) #7
                                  "t.absDec", #) #8
                                  "t.relDec")
  ##Extract features                        
  output.matrix[,1] <- apply(matrix.in, 2, min)
  output.matrix[,2] <- apply(matrix.in, 2, mean)
  output.matrix[,3] <- apply(matrix.in, 2, median)
  output.matrix[,4] <- apply(matrix.in, 2, max)
  output.matrix[,5] <- as.numeric((matrix.in[1, ]))
  output.matrix[,6] <- apply(matrix.in, 2, 
                             function(x) TimeToThresh(x, a.thresh))
  output.matrix[,7] <- apply(matrix.in, 2, 
                             function(x) TimeToRel(x, r.thresh))
  print(output.matrix[,7])
  decaytime.temp <- apply(matrix.in, 2, 
                           function(x) TimeToDec(x, r.thresh))
  
  
  output.matrix[,8] <- decaytime.temp - output.matrix[,6]
  decayrel.temp <- apply(matrix.in, 2, 
                         function(x) TimeDecRel(x, r.thresh))
  output.matrix[,9] <- decayrel.temp - output.matrix[,7]
  return(output.matrix)


}