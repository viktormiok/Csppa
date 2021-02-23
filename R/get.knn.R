###############################################################################
#
#       Data classification with k-nearest neighbors (KNN) algorithm
#   
###############################################################################
distance.euclidean <- function(a, b) { 
  sqrt(sum((a - b)^2))
}

get.knn <- function(train_labels,
                    train_data,
                    to_classify_data, 
                    k, 
                    distance = distance.euclidean, 
                    standardize = FALSE) {
          if (!is(train_labels, "character")) {
            stop("Input (train_labels) is of wrong class.")
          }
          if(ncol(train_data) != ncol(to_classify_data)) {
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
          if (!is(standardize, "logical")) {
            stop("Input (standardize) is of wrong class.")
          }
          # TODO: for a real implementation, we would need to add 
          #       input checks here to
          #       make our implicit assumptions about the inputs explicit:
          # train_labels: vector with no NAs, 
          #               same length as rows of train_data
          # train_data: data.frame or matrix without NAs, 
          #             same columns as to_classify_data
          # to_classify_data: data.frame or matrix without NAs
          # k: single positive integer, at most number of rows of train_data
          # distance: a function taking two arguments 
          #           (hard to check beyond that...)
          num_preds <- nrow(to_classify_data)
          pred <- rep(as.character(NA),
                      num_preds
          )
          # transform data to speedup computation
          train_data <- data.matrix(train_data) 
          to_classify_data <- data.matrix(to_classify_data)
          # standardize the feature vectors if desired
          if (standardize == TRUE) { 
            train_data <- scale(train_data) 
            to_classify_data <- scale(
              to_classify_data,
              center = attr(train_data, "scaled:center"), 
              scale = attr(train_data, "scaled:scale")
            ) 
          }
          for (i in seq_len(num_preds)) {
            # compute distance to all training data points 
            distance_to_train <- apply(
              train_data,
              MARGIN = 1,
              FUN = function(x) distance(x, to_classify_data[i, ])
            )
            # extract row indices of k nearest ones
            nearest_neighbors <- order(distance_to_train)[1:k]
            # compute frequencies of classes in neighborhood 
            class_frequency <- table(train_labels[nearest_neighbors]) 
            most_frequent_classes <- 
              names(class_frequency)[class_frequency == max(class_frequency)]
            # random tie breaking if more than 1 class has maximal frequency.
            # (causes the occasionally weird, fuzzy class 
            #  boundaries in the figs. below) 
            pred[i] <- sample(most_frequent_classes, 1)
          }
          list(prediction = pred,
               levels = levels(train_labels)
          ) 
}
