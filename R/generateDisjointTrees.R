#' generateDisjointTrees
#' The function generates adjacency matrix of two trees representing sets subfamilies. The first subfamily consists of sets containing element `elem`, the second consists of remaining sets.
#' @param sets The set system
#' @param elem The element splitting the set family. Default is the most common element 
#' @export
generateDisjointTrees <- function(sets, elem = NULL){
  stopifnot(class(sets) == "list")
  if(is.null(elem)){
    elem <- findMaxElems(sets)[1]
  }
  print(paste("Chosen element: ", elem, sep = ""))
  setsX <- {
    sets2 <- list()
    l <- 1
    for(i in sets){
      if(elem %in% i){
        sets2[[l]] <- i
        l <- l + 1
      }
    }
    sets2
  }
  setsNoX <- {
    sets2 <- list()
    l <- 1
    for(i in sets){
      if(!(elem %in% i)){
        sets2[[l]] <- i
        l <- l + 1
      }
    }
    sets2
  }
  if(length(setsNoX) != 0){
  m1 <- generateInclusionTreeMatrix(setsX)
  m2 <- generateInclusionTreeMatrix(setsNoX)
  m3 <- matrix(nrow = nrow(m1) + nrow(m2), ncol = ncol(m1) + ncol(m2))
  m3[1:nrow(m1), 1:ncol(m1)] <- m1
  m3[(nrow(m1)+1):nrow(m3), 1:ncol(m1)] <- 0
  m3[(nrow(m1)+1):nrow(m3), (ncol(m1)+1):ncol(m3)] <- m2
  m3[1:nrow(m1), (ncol(m1)+1):ncol(m3)] <- 0
  for(i in 1:nrow(m3)){
    for(j in 1:nrow(m3)){
      m3[i, j] <- max(m3[i, j], m3[j, i])
    }
  }
  z <- c(generateNamesVector(setsX), generateNamesVector(setsNoX))
  rownames(m3) <- z
  colnames(m3) <- z
  colors <- c(rep(3, times = nrow(m1)), rep(2, times = nrow(m2)))
  list(m3, colors)
  }else{
    m1 <- generateInclusionTreeMatrix(setsX)
    colors <- rep(3, times = nrow(m1))
    list(m1, colors)
  }
}
