###############################################################################
#
#       Test for square comparison of spatial point patterns 
#
###############################################################################

test.squares <- function(null = dat1,
                         alt = dat2,
                         method = "friedman",
                         window = window,
                         nr_sq = 10){
                if (!is(null, "list")) {
                  stop("Input (null) is of wrong class.")
                }
                if (!is(alt, "list")) {
                  stop("Input (alt) is of wrong class.")
                }
                if(length(null) != length(alt)) {
                  stop("Number of elements is different in data sets.")
                }
                if (!is(method, "character")) {
                  stop("Input (method) is of wrong class.")
                }
                if (!is(method, "character")) {
                  if (!(method %in% c("friedman",
                                      "wilcox"))) {
                    stop("Input (method) ill-specified.")
                  }
                } 
                if (!is(window, "owin")) {
                  stop("Input (window) is of wrong class.")
                }
                if (!is(nr_sq, "numeric")) {
                  stop("Input (nr_sq) is of wrong class.")
                }
                a <- b <- result <- numeric()
                for(i in 1:length(null)){
                  a <- rbind(a, 
                             quadratcount(ppp(null[[i]]$X, 
                                              null[[i]]$Y,
                                              window = window),
                                          nx = nr_sq,
                                          ny = nr_sq)
                  )
                  b <- rbind(b, 
                             quadratcount(ppp(alt[[i]]$X, 
                                              alt[[i]]$Y,
                                              window = window),
                                          nx = nr_sq, 
                                          ny = nr_sq)
                  )
                }
                for(i in 1:ncol(a)){
                  if(method == "friedman"){
                    result[i] <-  friedman.test(cbind(a[,i],
                                                      b[,i]))$p.value
                  }
                  if(method == "wilcox"){
                    result[i] <-  wilcox.exact(a[,i],
                                               b[,i], 
                                               paired = TRUE,
                                               alternative = "greater",
                                               exact = FALSE)$p.value
                  }   
                }
                #result <- p.adjust(result, "fdr")
                return(result)
}
