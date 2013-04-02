library(lattice)

SummaryPlot <- function (times, series.a, series.b, 
                         label.a, label.b, ylabel, filename,dev.boolean = TRUE){

  means.1 <- apply(series.a, 1, mean)
  sd.1 <- apply(series.a, 1, sd)
  se.1 <- sd.1/(sqrt(ncol(series.a)))
  medians.1 <- apply(series.a, 1, median)
  
  means.2 <- apply(series.b, 1, mean)
  sd.2 <- apply(series.b, 1, sd)
  se.2 <- sd.2/(sqrt(ncol(series.b)))
  medians.2 <- apply(series.b, 1, median)
  
  
  if (dev.boolean) pdf(filename, height = 8.5, width = 11)
  plot(x = times, y = means.1+se.1, typ = 'l', col = 1 , ylab = ylabel,
       xlab = "Time")
  lines(x = times, y = means.1, typ = 'l', col = 1, lwd = 3 )
  lines(x = times, y = means.1-se.1, typ = 'l', col =1)
  lines(x = times, y = medians.1, typ = 'l', col =1, lty = 2, lwd =3)
  
  
  lines(x = times, y = means.2+se.2, typ = 'l', col = 2)
  lines(x = times, y = means.2, typ = 'l', col = 2, lwd = 3)
  lines(x = times, y = means.2-se.2, typ = 'l', col = 2)
  lines(x = times, y = medians.2, typ = 'l', col =2, lty = 2, lwd = 3)
  legend ('topright', legend = c(paste(label.a, "Mean"),
                                 paste(label.a,"Median"),
                                 paste(label.b, "Mean"),
                                 paste(label.b,"Median")), 
          col = c(1,1,2,2), lty = c(1,2,1,2), lwd = c(3,3,3,3))
  if (dev.boolean) dev.off()
}
 
Grapher1 <- function(series.a, series.b, filename, num.row = 7,
                     num.col = 11, dev.boolean = TRUE){
  
  if (dev.boolean) pdf(filename)
  par(mar = c(0,0,0,0), oma = c(1,1,1,1), mfrow = c(num.row, num.col))

  apply(series.a, 2, function(x) plot(x, ylim = c(0,1),
                                      yaxt = 'n', xaxt = 'n') )
  apply(series.b, 2, function(x) plot(x, ylim = c(0,1), col = 'red',
                                            yaxt = 'n', xaxt = 'n')) 
  if (dev.boolean) dev.off()
}

#'@description This function creates the graphical output to file.
#'It will generate a summary plot showing mean, se, median curves
#'@param ts time series data
#'@param label.groups list of labels/groupnames, last entry is the y-units
#'@export 
GraphMaker1 <- function  (ts, label.groups, save.path){

  ts.col.all <- list()
  for (k in seq(1, length(ts), by = 2)){
    ts.col.all <- ts[[k]]
    size.1 <- ncol(ts.col.all[[1]])-1
    size.2 <- ncol(ts.col.all[[2]])-1
    #Mean/Median/SE series plot
    for (i in seq(1, 1, by=2)){
    time.points <- ts.col.all[[i]][,1]
    series.1 <- ts.col.all[[i]]
    series.1 <-series.1[,-1]
    series.2 <- ts.col.all[[(i+1)]]#[,2:size.2]
    series.2 <- series.2[,-1]
    
    filename <- paste0(save.path,'summaryplot',label.groups[i],
                       label.groups[i+1],'.pdf')
    SummaryPlot(time.points, series.1, series.2, label.groups[i], label.groups[i+1], 
                label.groups[-1], filename)
  
    filename <- paste0(save.path, 'tsmatrixplot', label.groups[i],
                     label.groups[i+1],'.pdf')
    Grapher1(series.1, series.2, filename)
  
  }
  }
}

#' @description This function takes a list of matrices that contain ts feature data. 
#' It generates a scatterplotmatrix (using splom) and density kernel plots
#' for each feature. It also does PCA on the data and generates Elbow and Biplots.
#' It save the pca info to file as well.
#' @param feat a list of paired lists of matrices/dataframes.
#' @param label.groups groupnames in pairs followed by y-axis label
#' @param save.path points to folder where outut should go.
GraphMaker2 <- function(feat, label.groups, save.path){

  for(n in seq(1, length(feat), by=2)){
    features.a <- as.matrix(feat[[n]][[1]])
    features.b <- as.matrix(feat[[n]][[2]])
    rows.a <- nrow(features.a)
    rows.b <- nrow(features.b)
    cols <- ncol(features.a)
    groupids <- c(rep(1, times = rows.a),
                  rep(2, times = rows.b))
    features <- matrix(, nrow = rows.a + rows.b, ncol = cols)
    features <- rbind(features.a, features.b)
    
    filename <- paste0(save.path,'spm',label.groups[n],
                       label.groups[n+1],'.pdf')
    pdf(filename, height = 11, width = 11)
    splom(features, pch = groupids+19, col = groupids)
    dev.off()
    
    filename <- paste0(save.path,'kerneldens',label.groups[n],
                       label.groups[n+1],'.pdf')
    pdf(filename, height = 8.5, width = 11 )
    par(oma = c(1,1,1,1), mar = c(2,2,2,2), 
    mfrow = c(floor(sqrt(cols)), ceiling(sqrt(cols))))
for (k in 1:cols){
  plot(density(features[1:rows.a,k]), main = colnames(features)[k])
#       xlab = label.groups[-1])
  lines(density(features[(rows.a+1):(rows.a+rows.b),k]), col = 2)
}
dev.off()

    
  cPCA <- prcomp(features[,1:cols],
             scale = TRUE, center = TRUE)
  prefix <- paste0(save.path,'pca',label.groups[n], label.groups[n+1])
  elbow.name <- paste0(prefix,'Elbow','.pdf')
  pdf(elbow.name, height = 8.5, width = 11)
  par(mfrow = c(1,2))
  plot(cPCA, typ = 'l', main = paste(label.groups[n], 
                                     "and", label.groups[n+1],"PCA"))
  dev.off()

  biplot.name <- paste0(prefix,'.pdf')
  pdf(biplot.name, height = 8.5, width = 11)
  biplot(cPCA, xlabs = features[,cols+1] )
  dev.off()

  save(cPCA, file = prefix)

  }
}
