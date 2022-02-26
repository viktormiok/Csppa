###############################################################################
#
#       function which from whole data extract data set of interest
#
###############################################################################
get.data <- function(data,
                     marker, 
                     diet,
                     mouse,
                     arc, 
                     cal_arc){
          # data: Data frame. Data set of spatial point pattern
          # marker: Character. Indicate which marker to include in data set
          # diet: Numeric. Indicate  which diet to include in data set
          # mouse: Numeric. Indicate which mouse to include in data set
          # arc: Numeric. Indicate which arc to include in data set
          # cal_arc: Logical. 
          
          if (!is(data, "data.frame")) {
            stop("Input (data) is of wrong class.")
          }
          if (!is(marker, "character")) {
            stop("Input (marker) is of wrong class.")
          }
          if (!is(diet, "numeric")) {
            stop("Input (diet) is of wrong class.")
          }
          if (!is(mouse, "numeric")) {
            stop("Input (mouse) is of wrong class.")
          }
          if (!is(arc, "numeric")) {
            stop("Input (arc) is of wrong class.")
          }
          if (!is(cal_arc, "logical")) {
            stop("Input (cal_arc) is of wrong class.")
          }
          result = list()
          if(cal_arc == TRUE){
            for(j in arc){
              result[[paste(marker, 
                            diet, 
                            j, 
                            sep='_')]] <- data[(data[,1] == diet) &
                                               (data[,2] == marker) &
                                               (data[,3] %in% mouse) &
                                               (data[,4] %in% j),] 
            }
          } else {
            for(i in diet){
              result[[paste(marker,
                            i, 
                            sep='_')]] <- data[(data[,1] == i) &
                                               (data[,2] == marker) &
                                               (data[,3] %in% mouse) &
                                               (data[,4] %in% arc),] 
            }
          }
          return(result)
}

