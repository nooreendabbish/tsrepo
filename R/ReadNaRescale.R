#'Reads the data in from filenames, replaces all na with 0,
#'rescales the data.
#'@param filename.a passed from TimeSAnalysis
#'@param filename.b passed from TimeSAnalysis
#'@description uses read.csv...
#'@return list of 2 rescaled, na-free, ts data matrices
ReadNaRescale <- function (filename.a, filename.b){
  
  c1 <- read.csv(filename.a)
  c2 <- read.csv(filename.b)
  
  
  c1[is.na(c1)] <- 0
  c2[is.na(c2)] <- 0 
  
  
  c1.rescaled <- Rescaler(c1, c2)
  c2.rescaled <- Rescaler(c2, c1)
  
  ts.cols <- list (c1.rescaled, c2.rescaled)
  return(ts.cols)
}

#'Takes two groups of ts data and rescales the first by the max of both.
#'@param in.1 first time series matrix, first column should be timestamps.
#'@param in.2 second time series matrix, first column should be timestamps.
#'@description divides the first df or matrix by the global max
#'@return function returns the rescaled first matrix
Rescaler <- function(in.1, in.2){ 
  
  mat.a <- in.1[,-1]
  mat.b <- in.2[,-1]
  
  max.a <- max(mat.a)
  max.b <- max(mat.b)
  
  global <- max(max.a,max.b)
  
  return(cbind(in.1[,1], (mat.a/global)))
}