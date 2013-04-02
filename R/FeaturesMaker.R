tAbsoluteThresh <- function (series.in, threshold)
{
  default <- length(series.in) + 1
  time.keep <- default 
  i <-0
  for (i in 1:length(series.in)){
    if (series.in[i] > threshold & (time.keep==default)){time.keep <- i}
  }
  return(time.keep)
}

#'Given a matrix with time series data arranged in columns,
#'this function produces a matrix of 'features' in each series.
#'Features include min, means, medians, etc.
#'@export 
#'@return output.matrix matrix quantifying features (by col) and
#'each ts by row
#'@param matrix.input contains time series (in ALL columns)
#'@seealso TimeSeriesAnalysis-package
FeaturesMaker <- function (matrix.input, a.thresh, r.thresh){
  
  number.features <- 6
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
                          
  output.matrix[,1] <- apply(matrix.in, 2, min)
  output.matrix[,2] <- apply(matrix.in, 2, mean)
  output.matrix[,3] <- apply(matrix.in, 2, median)
  output.matrix[,4] <- apply(matrix.in, 2, max)
  output.matrix[,5] <- as.numeric((matrix.in[1, ]))
  print(a.thresh)
  output.matrix[,6] <- apply(matrix.in, 2, 
                             function(x) tAbsoluteThresh(x, a.thresh))
  return(output.matrix)


}