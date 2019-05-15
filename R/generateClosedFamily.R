#' generateClosedFamily
#' 
#' This function generates random set family closed under union. Set system is represented with a list of sorted vectors of natural numbers.
#' @param p maximal number of elements in minimal set
#' @param n number of minimal sets
#' @export
#' @importFrom stats runif
#' @importFrom utils combn
#' 
generateClosedFamily <- function(p = NULL, n = NULL){
  if(is.null(p)){
    p <- floor(runif(1, min = 3, max = 15))
  }
  int <- 1:p
  if(is.null(n)){
    n <- floor(runif(1, min = 4, max = 10))
  }
  sets <- list()
  for(i in 1:n){
    number <- sample(p, 1)
    set <- sort(sample(p, number))
    if(Position(function(x) identical(x, set), sets, nomatch = 0) == 0){
      sets[[length(sets)+1]] <- set
    }
  }
  
  iter1 <- length(sets)
  iter2 <- 0
  while(!(iter1==iter2)){
    iter1 <- length(sets)
    temp <- combn(sets, 2)
    for(k in (1:(length(temp)/2))*2-1){
      temp1 <- union(temp[[k]], temp[[k+1]])
      temp1 <- sort(temp1)
      if(Position(function(x) identical(x, temp1), sets, nomatch = 0) == 0){
        sets[[length(sets)+1]] <- temp1
      }
    }
    iter2 <- length(sets)
  }
  sets
}