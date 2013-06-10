ReadNaRescale <-
function (filename.a, filename.b){
  
  c1 <- read.csv(filename.a)
  c2 <- read.csv(filename.b)
  
  
  c1[is.na(c1)] <- 0
  c2[is.na(c2)] <- 0 
  
  
  c1.rescaled <- Rescaler(c1, c2)
  c2.rescaled <- Rescaler(c2, c1)
  
  ts.cols <- list (c1.rescaled, c2.rescaled)
  return(ts.cols)
}
