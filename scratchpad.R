names.in <- c("CD41 G1", "CD41 G2", "Intensity")
out <- "./output/"
thrsh <- c(.2,.2)
file.1 <- "./data/NG12 G1 CD41.csv"
file.2 <- "./data/NG12 G2 CD41.csv"

tsa.output <- TimeSAnalysis(names.in, out, thrsh, 
                            "./data/NG12 G1 CD41.csv", "./data/NG12 G2 CD41.csv" )
