###############################################################################
#
#       Random forest classification
#
###############################################################################


randF.class <- function(dataX,
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
              spiral_data <- data.frame(cbind(dataX, dataY), 
                                        y = factor(dataG)
              )
              colnames(spiral_data) <- c("x1", "x2", "y")
              # set features that should be inspected within the regions
              features <- c("x1", "x2")
              spiral_task <- TaskClassif$new(id = "spirals",
                                             backend = spiral_data,
                                             target = "y"
              )
              plot_learner_prediction(
                lrn("classif.rpart",
                    predict_type = "prob"),
                spiral_task
              )
              
              plot_learner_prediction(
                lrn("classif.ranger", 
                    predict_type = "prob"),
                spiral_task
              )
}
