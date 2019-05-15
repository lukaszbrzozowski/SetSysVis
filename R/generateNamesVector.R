#' generateNamesVector
#' function generates vector of names (labels) for the set system.
#' @param sets The set system
#' @param breakpoint how many numbers shall be displayed in one line
#' @export
generateNamesVector <- function(sets, breakpoint = 3){
  stopifnot(class(sets) == "list")
  names <- list()
  t <- 1
  names <- lapply(sets, function(i){
    name <- "{"
    if(length(i) > 1){
      p <- 1
      for(j in 1:(length(i)-1)){
        name <- paste(name, as.character(i[j]), ", ", sep = "")
        if(p %% breakpoint == 0){
          name <- paste(name, "\n", sep = "")
        }
        p <- p + 1
      }
      name <- paste(name, as.character(i[length(i)]), sep = "")
      name <- paste(name, "}", sep = "")
      names[[t]] <- name
      t <- t + 1
    }else{
      name <- paste("{", as.character(i), "}", sep = "")
      names[[t]] <- name
      t <- t +1
    }
    i <- name
  })
  names
}