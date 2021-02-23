###############################################################################
#
#   Function for simple k-NN with two features to visualize class boundaries
#   
###############################################################################

# function for simple k-NN with two features to visualize class boundaries 
# X1 and X2 are the names of the two features to use.
plot.2d.knn <- function(train_labels,
                       train_data,
                       k,
                       X1,
                       X2,
                       distance = distance.euclidean,
                       standardize = FALSE,
                       to_classify_labels = train_labels,
                       to_classify_data = train_data,
                       lengthX1 = 100, 
                       lengthX2 = 100,
                       plot_class_rate = TRUE) {
            if (!is(train_labels, "character")) {
              stop("Input (train_labels) is of wrong class.")
            }
            if (!is(train_data, "data.frame")) {
              stop("Input (train_data) is of wrong class.")
            }
            if(length(train_labels) != nrow(train_data)) {
              stop("Number of elements is different in data sets.")
            }
            if (!is(k, "numeric")) {
              stop("Input (k) is of wrong class.")
            }
            if (length(k) != 1) {
              stop("Input (k) is of wrong length.")
            }
            if (is.na(k)) {
              stop("Input (k) is not a positive integer.")
            }
            if (k < 0) {
              stop("Input (k) is not a positive integer.")
            }
            if (!is(X1, "character")) {
              stop("Input (X1) is of wrong class.")
            }
            if (!is(X2, "character")) {
              stop("Input (X2) is of wrong class.")
            }
            if(nrow(train_data) != nrow(to_classify_data)) {
              stop("Number of elements is different in data sets.")
            }
            if(ncol(train_data) != ncol(to_classify_data)) {
              stop("Number of elements is different in data sets.")
            }
            if (!is(distance, "function")) {
              stop("Input (distance) is of wrong class.")
            }
            if (!is(standardize, "logical")) {
              stop("Input (standardize) is of wrong class.")
            }
            if (!is(k, "numeric")) {
              stop("Input (k) is of wrong class.")
            }
            if (length(k) != 1) {
              stop("Input (k) is of wrong length.")
            }
            if (is.na(k)) {
              stop("Input (k) is not a positive integer.")
            }
            if (k < 0) {
              stop("Input (k) is not a positive integer.")
            }
            if (!is(to_classify_labels, "character")) {
              stop("Input (to_classify_labels) is of wrong class.")
            }
            if(length(to_classify_labels) != nrow(to_classify_data)) {
              stop("Number of elements is different in data sets.")
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
            if (!is(plot_class_rate, "logical")) {
              stop("Input (plot_class_rate) is of wrong class.")
            }
            plot.2d.classify(to_classify_labels = to_classify_labels,
                             to_classify_data = to_classify_data,
                             classify_method <- function(to_classify_data){
                               get.knn(train_labels = train_labels,
                                       train_data = train_data[, c(X1, X2)], 
                                       to_classify_data = to_classify_data, 
                                       k = k, 
                                       distance = distance, 
                                       standardize = standardize)},
                             X1,
                             X2,
                             lengthX1 = lengthX1,
                             lengthX2 = lengthX2,
                             title = paste0("k == ", k),
                             plot_class_rate = plot_class_rate)
}

