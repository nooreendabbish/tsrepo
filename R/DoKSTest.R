#' @description This function accept a list of two ts matrices or data frames and
#' it returns a ks.test at a given timepoint.
#' @param input.list
#' @param time.x
#' @export
DoKSTest <- function(input.list, time.x = 120){
  ks.return <- list(length = length(input.list))
  for(k in 1:length(input.list)){
  time.points <- input.list[[k]][[1]][,1]
  time.look <- which(time.points > 120)
  #print(time.look[1])
  #print(input.list[[k]][[2]][time.look[1],])
  ks.return[[k]] <- ks.test(input.list[[k]][[1]][time.look[1],],
                       as.numeric(input.list[[k]][[2]][time.look[1],]))
  }
  return(ks.return)
}