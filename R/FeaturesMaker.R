TimeToThresh <- function(x, thresh){
  
  default <- length(x)+1
  first.stab <- which(x>thresh)
  if (length(first.stab) == 0){
    first.stab <- default}
  return(first.stab)
}

TimeToRel <- function(x, rel.frac){
  
  thresh <- (rel.frac*max(x))
  first.stab <- which(x>thresh)
  
  return(first.stab)
}
##### FeaturesMatrix Header ####
#'Given a matrix with time series data arranged in columns,
#'this function produces a matrix of 'features' in each series.
#'Features include min, means, medians, etc.
#'@export 
#'@return output.matrix matrix quantifying features (by col) and
#'each ts by row
#'@param matrix.input contains time series (in ALL columns)
#'@seealso TimeSeriesAnalysis-package
FeaturesMaker <- function (matrix.input, a.thresh, r.thresh){
  
  ##initialize and define.
  number.features <- 7
  matrix.in <- matrix.input[,2:ncol(matrix.input)]
  rows.in <- nrow(matrix.in)
  cols.in <- ncol(matrix.in)
  output.matrix <- matrix(, nrow = cols.in, ncol = number.features)
  colnames(output.matrix) <- list("Mins", #1
                                  "Means",#2
                                  "Medians",#3
                                  "Mean", #4
                                  "StartValue", #) #, #5
                                  "t.absThr") #6
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
  #decaytime.temp <- apply(matrix.in, 2, 
  #                          function(x) TimeToDec(x, r.thresh))
  #output.matrix[,8] <- decaytime.temp - output.matrix[,6]
  #decayrel.temp <- apply(matrix.in, 2, TimeDecRel(x, r.thresh))
  #output.matrix[,9] <- 
  return(output.matrix)


}