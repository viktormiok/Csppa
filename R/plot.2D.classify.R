###############################################################################
#
#   Function for a general classification method with two features to visualize
#   
###############################################################################

plot.2D.classify <- function(to_classify_labels,
                             to_classify_data, 
                             classify_method,
                             X1,
                             X2,
                             lengthX1 = 100,
                             lengthX2 = 100,
                             title = "",
                             plot_class_rate = TRUE) {
                  if (!is(to_classify_labels, "character")) {
                    stop("Input (to_classify_labels) is of wrong class.")
                  }
                  if (!is(classify_method, "function")) {
                    stop("Input (classify_method) is of wrong class.")
                  }
                  if (!is(X1, "character")) {
                    stop("Input (X1) is of wrong class.")
                  }
                  if (!is(X2, "character")) {
                    stop("Input (X2) is of wrong class.")
                  }
                  if (!is(lengthX1, "numeric")) {
                    stop("Input (lengthX1) is of wrong class.")
                  }
                  if (length(lengthX1) != 1) {
                    stop("Input (lengthX1) is of wrong length.")
                  }
                  if (is.na(lengthX1)) {
                    stop("Input (lengthX1) is not a positive integer.")
                  }
                  if (lengthX1 < 0) {
                    stop("Input (lengthX1) is not a positive integer.")
                  }
                  if (!is(lengthX2, "numeric")) {
                    stop("Input (lengthX2) is of wrong class.")
                  }
                  if (length(lengthX2) != 1) {
                    stop("Input (lengthX2) is of wrong length.")
                  }
                  if (is.na(lengthX2)) {
                    stop("Input (lengthX2) is not a positive integer.")
                  }
                  if (lengthX2 < 0) {
                    stop("Input (lengthX2) is not a positive integer.")
                  }
                  if (!is(title, "character")) {
                    stop("Input (to_classify_labels) is of wrong class.")
                  }
                  if (!is(plot_class_rate, "logical")) {
                    stop("Input (plot_class_rate) is of wrong class.")
                  }
                  gridX1 <- seq( 
                    min(to_classify_data[, X1]), 
                    max(to_classify_data[, X1]), 
                    length.out = lengthX1
                  )
                  gridX2 <- seq(
                    min(to_classify_data[, X2]), 
                    max(to_classify_data[, X2]), 
                    length.out = lengthX2
                  )
                  # compute grid coordinates with cartesian product
                  grid_data <- expand.grid(gridX1, gridX2) 
                  names(grid_data) <- c(X1, X2)
                  # assign grid cells to classes based on classification rule:
                  grid_result <- classify_method( 
                    to_classify_data = grid_data
                  )
                  grid_data$prediction <- grid_result$prediction
                  # assign data to be classified based on classification rule &
                  # check these "predictions"
                  to_check_result <- classify_method(
                    to_classify_data = to_classify_data[, c(X1, X2)] )
                  to_classify_data$class <- to_classify_labels
                  to_classify_data$correct <- 
                    (to_check_result$prediction == to_classify_labels)
                  
                  ggplot() + 
                    geom_raster(data = grid_data,
                                aes_string(x = X1,
                                           y = X2,
                                           fill = "prediction"), 
                                alpha = .8) + 
                    geom_point(data = to_classify_data,
                               aes_string(x = X1,
                                          y = X2,
                                          shape = "class",
                                          color = "correct"), 
                               alpha = .8) +  
                    scale_color_manual(values = c("TRUE" = "darkgray", 
                                                  "FALSE" = "white"),
                                       guide = FALSE ) +
                    labs(fill = "Class",
                         shape = "Class",
                         title = parse(text = if(plot_class_rate) paste0(
                           title, '~ ": " ~',
                           100 * round(mean(to_classify_data$correct), 4),
                           '~ "% correctly classified"')
                           else title )) +
                    theme(plot.title = element_text(size = 16)) 
}
