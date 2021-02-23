###############################################################################
#
#       Test for spatial autocorrelation employing Moran.I and Mantel test
#   
###############################################################################

test.saptcorr <- function(dataX,
                          dataY,
                          dataG,
                          method){
                if (!is(dataX, "numeric")) {
                  stop("Input (dataX) is of wrong class.")
                }
                if (!is(dataY, "numeric")) {
                  stop("Input (dataY) is of wrong class.")
                }
                if (!is(dataG, "character")) {
                  stop("Input (dataG) is of wrong class.")
                }
                if(length(dataX) != length(dataY)) {
                  stop("Number of elements is different in data sets.")
                }
                if(length(dataX) != length(dataG)) {
                  stop("Number of elements is different in data sets.")
                }
                if (!is(method, "character")) {
                  stop("Input (method) is of wrong class.")
                }
                if (!is(method, "character")) {
                  if (!(method %in% c("Moran.I",
                                      "Mantel"))) {
                    stop("Input (method) ill-specified.")
                  }
                }  
                my.data1 <- data.frame(X = dataX,
                                       Y = dataY, 
                                       Gene = as.numeric(factor(dataG))
                )
                # matrix of all distances...
                my.dists <- as.matrix(dist(cbind(my.data1$X,
                                                 my.data1$Y))
                )
                # ...which is inversed...
                my.dists.inv1 <- 1/my.dists
                # ...and their diagonals set to "0"
                diag(my.dists.inv1) <- 0
                my.dists.inv1[is.infinite(my.dists.inv1)] <- 0
                # Moran.I test function
                Moran.I <- function (x, 
                                     weight, 
                                     scaled = FALSE,
                                     na.rm = FALSE, 
                                     alternative = "two.sided"){
                  if (dim(weight)[1] != dim(weight)[2]) 
                    stop("'weight' must be a square matrix")
                  n <- length(x)
                  if (dim(weight)[1] != n) 
                    stop("'weight' must have as many rows as observations in 'x'")
                  ei <- -1/(n - 1)
                  nas <- is.na(x)
                  if (any(nas)) {
                    if (na.rm) {
                      x <- x[!nas]
                      n <- length(x)
                      weight <- weight[!nas, !nas]
                    }
                    else {
                      warning("'x' has missing values: maybe you 
                                    wanted to set na.rm = TRUE?")
                      return(list(observed = NA, 
                                  expected = ei,
                                  sd = NA, 
                                  p.value = NA))
                    }
                  }
                  ROWSUM <- rowSums(weight)
                  ROWSUM[ROWSUM == 0] <- 1
                  weight <- weight/ROWSUM
                  s <- sum(weight)
                  m <- mean(x)
                  y <- x - m
                  cv <- sum(weight * y %o% y)
                  v <- sum(y^2)
                  obs <- (n/s) * (cv/v)
                  if (scaled) {
                    i.max <- (n/s) * 
                      (sd(rowSums(weight) * y)/sqrt(v/(n - 1)))
                    obs <- obs/i.max
                  }
                  S1 <- 0.5 * sum((weight + t(weight))^2)
                  S2 <- sum((apply(weight, 1, sum) + apply(weight, 2, sum))^2)
                  s.sq <- s^2
                  k <- (sum(y^4)/n)/(v/n)^2
                  sdi <- sqrt((n * ((n^2 - 3 * n + 3) * S1 - n * S2 + 3 * s.sq) - 
                                 k * (n * (n - 1) * S1 - 2 * n * S2 + 6 * s.sq))/
                                ((n - 1) * (n - 2) * (n - 3) * s.sq) - 1/((n - 1)^2))
                  alternative <- match.arg(alternative, c("two.sided", 
                                                          "less", 
                                                          "greater")
                  )
                  pv <- pnorm(obs,
                              mean = ei,
                              sd = sdi
                  )
                  if (alternative == "two.sided") 
                    pv <- if (obs <= ei) 2 * pv
                  else 2 * (1 - pv)
                  if (alternative == "greater") pv <- 1 - pv
                  list(observed = obs,
                       expected = ei,
                       sd = sdi,
                       p.value = pv)
                }
                # Mantel test function
                mantel.test <- function (m1,
                                         m2,
                                         nperm = 999, 
                                         graph = FALSE, 
                                         alternative = "two.sided", ...){
                  alternative <- match.arg(alternative, c("two.sided", 
                                                          "less", 
                                                          "greater"))
                  n <- nrow(m1)
                  realz <- mant.zstat(m1, m2)
                  nullstats <- replicate(nperm, 
                                         mant.zstat(m1, perm.rowscols(m2,n))
                  )
                  pval <- switch(alternative, 
                                 two.sided = 2 * min(sum(nullstats >= realz), 
                                                     sum(nullstats <= realz)),
                                 less = sum(nullstats <= realz), 
                                 greater = sum(nullstats >= realz))
                  pval <- (pval + 1)/(nperm + 1)
                  if (alternative == "two.sided" && pval > 1) 
                    pval <- 1
                  if (graph) {
                    plot(density(nullstats), type = "l", ...)
                    abline(v = realz)
                  }
                  list(z.stat = realz, 
                       p = pval,
                       alternative = alternative)
                }
                
                # points with similar values 
                # (astrocyte with similar marker) close to each other
                if(method=="Moran.I"){
                  result <- Moran.I(x = my.data1$Gene,
                                    weight = my.dists.inv1
                  )
                }
                if(method=="Mantel"){
                  result <- mantel.test(m1 = as.matrix(dist(my.data1$Gene)),
                                        m2 = my.dists
                  )
                }
                return(result)
}
