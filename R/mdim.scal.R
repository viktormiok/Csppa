###############################################################################
#
#       Multidimensional scaling based on random forest proximity
#
###############################################################################


mdim.scal <- function(dataX,
                      dataY,
                      dataG){
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
            # fit forest and extract proximity measure
            spiral_rf <- randomForest(x = cbind(dataX, dataX),
                                      y = as.factor(dataG),
                                      ntree = 10000,
                                      proximity = TRUE,
                                      oob.prox = TRUE,
            )
            
            # convert proximities into distances
            proximity_to_dist <- function(proximity) {
              1 - proximity
            }
            spiral_dist <- proximity_to_dist(spiral_rf$proximity)
            
            spiral_mds <- as.data.frame(cmdscale(spiral_dist))
            spiral_mds$class <- as.factor(dataG)
            # plot the result, sweet
            p <- ggplot(data = spiral_mds, aes(x = V1,
                                               y = V2, 
                                               colour = class)) +
              geom_point() +
              labs(x = "1st dimension",
                   y = "2nd dimension",
                   title = "Multidimensional scaling based 
                                      on random forest proximity"
              )
            return(p)
}
