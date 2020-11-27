split_LR <- function(sc){
  scnew <- split(sc, sc$p_throws)
  names(scnew) <- c("p_throws: L",
                    "p_throws: R")
  scnew
}
