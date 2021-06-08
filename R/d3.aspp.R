###############################################################################
#
#       Plots for 3D kernerl density visualization spatial point patterns
#
###############################################################################

d3.aspp <- function(data,
                    data1=NULL,
                    type_plot="3D_plot",
                    window,
                    sigma=15,
                    theta=40,
                    phi=35,
                    col="lightblue",
                    title=" "){
  
          if (!is(data, "data.frame")) {
            stop("Input (data) is of wrong class.")
          }
          if (!is.null(data1)) {
            if(!is(data, "data.frame")){
              stop("Input (data1) is of wrong class.")
            }
            else{ if(ncol(data) != ncol(data1)) {
              stop("Number of columns is different in data sets.")
            }
            }
          } 
          if (!is(type_plot, "character")) {
            stop("Input (type_plot) is of wrong class.")
          }
          if (is(type_plot, "character")) {
            if (!(type_plot %in% c("3D_plot",
                                   "3D_plot_interactive"))) {
              stop("Input (type_plot) ill-specified.")
            }
          } 
          if (!is(sigma, "numeric")) {
            stop("Input (sigma) is of wrong class.")
          }
          if (length(sigma) != 1) {
            stop("Input (sigma) is of wrong length.")
          }
          if (is.na(sigma)) {
            stop("Input (sigma) is not a positive integer.")
          }
          if (sigma < 0) {
            stop("Input (sigma) is not a positive integer.")
          }
          if (!is(theta, "numeric")) {
            stop("Input (theta) is of wrong class.")
          }
          if (length(theta) != 1) {
            stop("Input (theta) is of wrong length.")
          }
          if (is.na(theta)) {
            stop("Input (theta) is not a positive integer.")
          }
          if (theta < 0) {
            stop("Input (theta) is not a positive integer.")
          }
          if (!is(phi, "numeric")) {
            stop("Input (phi) is of wrong class.")
          }
          if (length(phi) != 1) {
            stop("Input (phi) is of wrong length.")
          }
          if (is.na(phi)) {
            stop("Input (phi) is not a positive integer.")
          }
          if (phi < 0) {
            stop("Input (phi) is not a positive integer.")
          }
          if (!is(col, "character")) {
            stop("Input (col) is of wrong class.")
          }
          if(type_plot == "3D_plot"){
            k1 <- density(ppp(data$X,
                              data$Y,
                              window=window),
                          sigma=sigma
            ) 
            if(!is.null(data1)){
              k2 <- density(ppp(data1$X,
                                data1$Y,
                                window=window),
                            sigma=sigma
              ) 
              persp(k2 - k1, 
                    col="lightblue",
                    theta=theta,
                    phi=phi,
                    main=title
              )
            } else{
              persp(k1, 
                    col="lightcyan",
                    theta=theta,
                    phi=phi,
                    main=title,
                    shade=0.0001,
                    axes=TRUE,
                    box=TRUE
              ) 
            }
          }
          if(type_plot == "3D_plot_interactive"){
            # Compute kde2d
            kd <- with(data, 
                       MASS::kde2d(x=X, 
                                   y=Y,
                                   n=50)
            )
            # Plot with plotly
            p <- plot_ly(x=kd$x, 
                         y=kd$y,
                         z=kd$z) %>% add_surface()
          }
}
