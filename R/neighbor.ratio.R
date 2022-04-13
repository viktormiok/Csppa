###############################################################################
#
#       Over all percentage of the cells in the neighbourhood within the diameter
#
###############################################################################

neighbor.ratio <- function(dataX,
                           dataY,
                           dataG,
                           diam,
                           per_marker=TRUE){
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
                if (!is(diam, "numeric")) {
                  stop("Input (diam) is of wrong class.")
                }
                if (length(diam) != 1) {
                  stop("Input (diam) is of wrong length.")
                }
                if (is.na(diam)) {
                  stop("Input (diam) is not a positive integer.")
                }
                if (diam < 0) {
                  stop("Input (diam) is not a positive integer.")
                }
                if (!is(per_marker, "logical")) {
                  stop("Input (per_marker) is of wrong class.")
                }
                geo.dist = dist(cbind(dataX, dataY))
                geo.dist = as.matrix(geo.dist)
                geo.dist[geo.dist > diam] = 0
                
                if(per_marker){
                  out = matrix(0, nrow(geo.dist), 3)
                  for(i in 1:nrow(geo.dist)){
                    s = length(dataG[geo.dist[i,] != 0])
                    out[i,1] = sum(dataG[geo.dist[i,] != 0] == 'gfap_only')/s
                    out[i,2] = sum(dataG[geo.dist[i,] != 0] == 'aldh_only')/s
                    out[i,3] = sum(dataG[geo.dist[i,] != 0] == 'both')/s 
                  }
                  colnames(out) = c('gfap_only', 'aldh_only', 'both')
                  return(out)
                } else{
                  res = numeric()
                  for(i in 1:nrow(geo.dist)){
                    tab = table(dataG[geo.dist[i,] != 0])
                    res = c(res, as.numeric(tab/sum(tab)))
                  }
                  return(res)
                }
}