#Creator: Viktorian Miok
#Day of creation: 05.01.2022
#Description: This file contains functions of interest for the package tigaRcycle

#'d3.aspp
#'@description This function for plotting of 3D kernel density visualization spatial point patterns.
#'@param data vector of x and y coordinates of data points.
#'@param data1 vector of x and y coordinates of data points.
#'@param type_plot define the type of the plot either "3D_plot" or "3D_plot_interactive".
#'@param window window of observation, an object of class "owin".
#'@param sigma parameter for density function.
#'@param theta angles defining the viewing direction. theta gives the azimuthal direction and phi.
#'@param phi the colatitude.
#'@param col the color(s) of the surface facets. Transparent colors are ignored. This is recycled to the (nx-1)(ny-1) facets.
#'@param title character vector representing the title of the plot.
#'@examples
#'@author Viktorian Miok
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

#'dens.aspp
#'@description This function for visualization and testing of spatial point patterns.
#'@param data Vector of x and y coordinates of data point.
#'@param data1 Vector of x and y coordinates of data point.
#'@param test_null the null model object to plot.
#'@param test_alt the alternative model object to plot.
#'@param method method employed for testing "friedman" or "wilcoxon".
#'@param nr_sq number of squares for the polygon horizontal axis.
#'@param z_lim plotting limits.
#'@param type_plot type of the comparison "subtraction_square_plot", "subtraction_dens_plot" and "test_plot".
#'@param title character indicate title of the plot.
#'@param window window of observation, an object of class "owin".
#'@param cex.axis specify the size of the tick label numbers/text with a numeric value of length 1.
#'@param cex.lab specify the size of the axis label text with a numeric value of length 1.
#'@param col The colors to be used for the background.
#'@param num_size number indicating the size of the labels.
#'@param num_size_sig number indicating the size of the labels.
#'@param ribbon logical flag indicating whether to display a ribbon showing the color map. Default is TRUE for new plots and FALSE for added plots.
#'@param contour create a contour plot, or add contour lines to an existing plot.
#'@param bias bias >1 puts more colors at high values.
#'@param add_star_sig logical flag indicating whether to add or not star in the square.
#'@param test_overall logical flag indicating whether to test or not.
#'@param add_square_counts logical flag indicating whether to add number of cells in the square
#'@examples
#'@author Viktorian Miok
dens.aspp <- function(data, 
                      data1=NULL,
                      test_null=NULL,
                      test_alt=NULL,
                      method="friedman",
                      nr_sq=10,
                      z_lim=NULL,
                      type_plot="test_plot", 
                      title=" ", 
                      window, 
                      cex.axis=1,
                      cex.lab=1,
                      col=c("blue", "white", "red"),
                      num_size=1.2,
                      num_size_sig=1.5,
                      ribbon=FALSE,
                      contour=TRUE,
                      bias=NULL,
                      add_star_sig=FALSE,
                      test_overall=TRUE,
                      add_square_counts=TRUE){
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
  if ((!is.null(test_null)) & (!is.null(test_alt))) {
    if(!is(test_null, "list")){
      stop("Input (test_null) is of wrong class.")
    }
    if(!is(test_alt, "list")){
      stop("Input (test_alt) is of wrong class.")
    }
    else{ if(length(test_null) != length(test_alt)) {
      stop("Number of elements is different in data sets.")
    }
    }
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
  if (!is(nr_sq, "numeric")) {
    stop("Input (nr_sq) is of wrong class.")
  }
  if (length(nr_sq) != 1) {
    stop("Input (nr_sq) is of wrong length.")
  }
  if (is.na(nr_sq)) {
    stop("Input (nr_sq) is not a positive integer.")
  }
  if (nr_sq < 0) {
    stop("Input (nr_sq) is not a positive integer.")
  }
  if(!is.null(z_lim)){
    if (!is(z_lim, "numeric")) {
      stop("Input (z_lim) is of wrong class.")
    }
    if (length(z_lim) != 2) {
      stop("Input (z_lim) is of wrong length.")
    }
  }
  if (!is(type_plot, "character")) {
    stop("Input (type_plot) is of wrong class.")
  }
  if (is(type_plot, "character")) {
    if (!(type_plot %in% c("subtraction_square_plot",
                           "subtraction_dens_plot",
                           "test_plot"))) {
      stop("Input (type_plot) ill-specified.")
    }
  } 
  if (!is(title, "character")) {
    stop("Input (title) is of wrong class.")
  }
  if (!is(window, "owin")) {
    stop("Input (window) is of wrong class.")
  }
  if (!is(cex.axis, "numeric")) {
    stop("Input (cex.axis) is of wrong class.")
  }
  if (length(cex.axis) != 1) {
    stop("Input (cex.axis) is of wrong length.")
  }
  if (is.na(cex.axis)) {
    stop("Input (cex.axis) is not a positive integer.")
  }
  if (cex.axis < 0) {
    stop("Input (cex.axis) is not a positive integer.")
  }
  if (!is(cex.lab, "numeric")) {
    stop("Input (cex.lab) is of wrong class.")
  }
  if (length(cex.lab) != 1) {
    stop("Input (cex.lab) is of wrong length.")
  }
  if (is.na(cex.lab)) {
    stop("Input (cex.lab) is not a positive integer.")
  }
  if (cex.lab < 0) {
    stop("Input (cex.lab) is not a positive integer.")
  }
  if (!is(col, "character")) {
    stop("Input (col) is of wrong class.")
  }
  if (!is(num_size, "numeric")) {
    stop("Input (num_size) is of wrong class.")
  }
  if (length(num_size) != 1) {
    stop("Input (num_size) is of wrong length.")
  }
  if (is.na(num_size)) {
    stop("Input (num_size) is not a positive integer.")
  }
  if (num_size < 0) {
    stop("Input (num_size) is not a positive integer.")
  }
  if (!is(num_size_sig, "numeric")) {
    stop("Input (num_size_sig) is of wrong class.")
  }
  if (length(num_size_sig) != 1) {
    stop("Input (num_size_sig) is of wrong length.")
  }
  if (is.na(num_size_sig)) {
    stop("Input (num_size_sig) is not a positive integer.")
  }
  if (num_size_sig < 0) {
    stop("Input (num_size_sig) is not a positive integer.")
  }
  if (!is(ribbon, "logical")) {
    stop("Input (ribbon) is of wrong class.")
  }
  if (!is(contour, "logical")) {
    stop("Input (contour) is of wrong class.")
  }
  if(!is.null(bias)){
    if (!is(bias, "numeric")) {
      stop("Input (bias) is of wrong class.")
    }
    if (length(bias) != 1) {
      stop("Input (bias) is of wrong length.")
    }
    if (is.na(bias)) {
      stop("Input (bias) is not a positive integer.")
    }
    if (bias < 0) {
      stop("Input (bias) is not a positive integer.")
    }
  }
  if (!is(add_star_sig, "logical")) {
    stop("Input (add_star_sig) is of wrong class.")
  }
  if (!is(test_overall, "logical")) {
    stop("Input (test_overall) is of wrong class.")
  }
  if (!is(add_square_counts, "logical")) {
    stop("Input (test_overall) is of wrong class.")
  }
  
  if(type_plot == "subtraction_square_plot"){
    t1 <- ppp(data$X,
              data$Y,
              window=window
    )
    Q1 <- quadratcount(t1, 
                       nx=nr_sq,
                       ny=nr_sq
    )
    if(!is.null(data1)){
      t2 <- ppp(data1$X,
                data1$Y,
                window=window
      )
      Q2 <- quadratcount(t2, 
                         nx=nr_sq,
                         ny=nr_sq
      )
      if(is.null(z_lim)){
        plot(intensity(Q2-Q1, image=TRUE), 
             main=title, 
             las=1,
             zlim=c(-max(abs(intensity(Q2-Q1, image=TRUE)$v), na.rm=T),
                    max(abs(intensity(Q2-Q1, image=TRUE)$v), na.rm=T)),
             axes=TRUE,
             box=FALSE,
             ribbon=ribbon,
             cex.axis=cex.axis,
             cex.lab=cex.lab,
             col=colorRampPalette(col, 
                                  bias=ifelse(is.null(bias),
                                              1,
                                              bias))(n=255)
        )
      } else{ plot(intensity(Q2-Q1, image=TRUE), 
                   main=title, 
                   las=1, 
                   zlim=z_lim, 
                   axes=TRUE,
                   box=FALSE,
                   ribbon=ribbon,
                   cex.axis=cex.axis,
                   cex.lab=cex.lab,
                   col=colorRampPalette(col, 
                                        bias=ifelse(is.null(bias),
                                                    1,
                                                    bias))(n=255)
      )
      }
    } else { if(is.null(z_lim)){
      plot(intensity(Q1, image=TRUE), 
           main=title, 
           las=1, 
           axes=TRUE,
           box=FALSE,
           ribbon=ribbon,
           cex.axis=cex.axis,
           cex.lab=cex.lab,
           col=colorRampPalette(col, 
                                bias=ifelse(is.null(bias), 
                                            1,
                                            bias))(n=255)
      )
    } else{plot(intensity(Q1, image=TRUE), 
                main=title, 
                las=1, 
                zlim=z_lim, 
                axes=TRUE,
                box=FALSE,
                ribbon=ribbon,
                cex.axis=cex.axis,
                cex.lab=cex.lab,
                col=colorRampPalette(col, 
                                     bias=ifelse(is.null(bias),
                                                 1,
                                                 bias))(n=255)
    )
    }
    }  
    mtext(expression(paste("Distance ", "(", mu, "m)")),
          side=1,
          line=2,
          cex=cex.lab
    )
    mtext(expression(paste("Distance ", "(", mu, "m)")),
          side=2, 
          line=1,
          cex=cex.lab
    )
    if(ribbon){
      mtext("Kernel Density",
            side=4, 
            line=1,
            cex=cex.lab
      )
    }
  }
  if(type_plot == "subtraction_dens_plot"){
    t1 <- ppp(data$X,
              data$Y,
              window=window
    )
    t2 <- ppp(data1$X,
              data1$Y,
              window=window
    )
    k1 <- density(t1,
                  sigma=15
    ) 
    k2 <- density(t2,
                  sigma=15
    ) 
    if(is.null(z_lim)){
      p <- plot(k2 - k1, 
                las=1, 
                main=title,
                zlim=c(-max(abs((k2-k1)$v), na.rm=T)
                       ,max(abs((k2-k1)$v), na.rm=T)),
                axes=TRUE,
                box=FALSE,
                ribbon=ribbon,
                col=colorRampPalette(col, 
                                     bias=ifelse(is.null(bias),
                                                 1,
                                                 bias))(n=255),
                cex.axis=cex.axis,
                cex.lab=cex.lab
      ) 
    } else{
      p <- plot(k2 - k1, 
                las=1, 
                main=title,
                zlim=z_lim,
                axes=TRUE,
                box=FALSE,
                ribbon=ribbon,
                col=colorRampPalette(col, 
                                     bias=ifelse(is.null(bias),
                                                 1,
                                                 bias))(n=255),
                cex.axis=cex.axis,
                cex.lab=cex.lab
      ) 
    }
    if(contour)
      contour(k2 - k1, 
              add=TRUE
      )
    mtext(expression(paste("Distance ", "(", mu, "m)")),
          side=1, 
          line=3, 
          cex=cex.lab
    )
    mtext(expression(paste("Distance ", "(", mu, "m)")),
          side=2,
          line=2,
          cex=cex.lab
    )
    if(ribbon){
      mtext("Kernel Density", 
            side=4,
            line=1,
            cex=cex.lab
      )
    }
  } 
  if(type_plot == "test_plot"){
    # density plot with contour and number of points per square
    t1 <- ppp(data$X,
              data$Y,
              window=window
    )
    k1 <- density(t1,
                  sigma=15
    ) 
    if(is.null(z_lim)){
      p <- plot(k1, 
                las=1, 
                main=title,
                axes=TRUE,
                box=FALSE,
                ribbon=ribbon,
                col=colorRampPalette(col)(n=255),
                cex.axis=cex.axis,
                cex.lab=cex.lab
      ) 
    } else{
      p <- plot(k1, 
                las=1, 
                main=title,
                zlim=z_lim,
                axes=TRUE,
                box=FALSE,
                ribbon=ribbon,
                col=colorRampPalette(col)(n=255),
                cex.axis=cex.axis,
                cex.lab=cex.lab
      ) 
    }
    if(contour){
      contour(k1, 
              add=TRUE
      )
    }
    Q1 <- quadratcount(t1, 
                       nx=nr_sq,
                       ny=nr_sq
    )
    if(!is.null(test_null)){
      result <- test.squares(null=test_null, 
                             alt=test_alt, 
                             nr_sq=nr_sq,
                             window=window,
                             method="friedman"
      )
      result[is.nan(result)] <- 1
      if(add_star_sig){
        for(i in 1:length(Q1)) {
          if(result[i] < 0.05) Q1[i] <- paste(Q1[i],
                                              "*",
                                              sep="")
        } 
      }
      p <- plot(Q1,
                add=TRUE,
                col=ifelse(result < 0.05, "green3", 'white'), # lawngreen
                cex=ifelse(result < 0.05, num_size_sig, num_size)
      )
      if(test_overall){
        out <- diff.aspp(null=test_null, 
                         alternative=test_alt, 
                         nr_sq=nr_sq,
                         window=window,
                         method="friedman",
                         testonly=TRUE
        )
      }
    } else {
      if(add_square_counts){
        p <- plot(Q1,
                  add=TRUE,
                  col="white", 
                  cex=num_size,
                  cex.axis=cex.axis,
                  cex.lab=cex.lab
        )
      }
    }
    mtext(expression(paste("Distance ", "(", mu, "m)")),
          side=1,
          line=3,
          cex=cex.lab
    )
    mtext(expression(paste("Distance ", "(", mu, "m)")),
          side=2,
          line=2,
          cex=cex.lab
    )
    if(ribbon){
      mtext("Kernel Density",
            side=4,
            line=1,
            cex=cex.lab
      )
    }
    
  }
}

#'diff.aspp
#'@description This function for testing the overall and local difference in spatial point patterns.
#'@param null the null model object to plot.
#'@param alternative the alternative model object to plot.
#'@param method method employed for non-parametric testing, "friedman" or "wilcoxon".
#'@param nr_sq number of squares for the polygon horizontal axis.
#'@param window  window of observation, an object of class "owin".
#'@param title  character indicate title of the plot.
#'@param ribbon logical flag indicating whether to display a ribbon showing the color map. Default is TRUE for new plots and FALSE for added plots.
#'@param cex.axis specify the size of the tick label numbers/text with a numeric value of length 1.
#'@param cex.lab specify the size of the axis label text with a numeric value of length 1.
#'@param col the colors to be used for the background.
#'@param testonly logical flag indicating whether to test or not.
#'@examples
#'@author Viktorian Miok
diff.aspp <- function(null,
                      alternative,
                      method="friedman",
                      nr_sq=10, 
                      window,
                      title="",
                      ribbon=TRUE,
                      cex.axis=1,
                      cex.lab=1,
                      col=c("blue", "white", "red"),
                      testonly=FALSE){
  if (!is(null, "list")) {
    stop("Input (null) is of wrong class.")
  }
  if (!is(alternative, "list")) {
    stop("Input (alternative) is of wrong class.")
  }
  if(length(null) != length(alternative)) {
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
  if (!is(nr_sq, "numeric")) {
    stop("Input (nr_sq) is of wrong class.")
  }
  if (length(nr_sq) != 1) {
    stop("Input (nr_sq) is of wrong length.")
  }
  if (is.na(nr_sq)) {
    stop("Input (nr_sq) is not a positive integer.")
  }
  if (nr_sq < 0) {
    stop("Input (nr_sq) is not a positive integer.")
  }
  if (!is(title, "character")) {
    stop("Input (title) is of wrong class.")
  }
  if (!is(window, "owin")) {
    stop("Input (window) is of wrong class.")
  }
  if (!is(ribbon, "logical")) {
    stop("Input (ribbon) is of wrong class.")
  }
  if (!is(cex.axis, "numeric")) {
    stop("Input (cex.axis) is of wrong class.")
  }
  if (length(cex.axis) != 1) {
    stop("Input (cex.axis) is of wrong length.")
  }
  if (is.na(cex.axis)) {
    stop("Input (cex.axis) is not a positive integer.")
  }
  if (cex.axis < 0) {
    stop("Input (cex.axis) is not a positive integer.")
  }
  if (!is(cex.lab, "numeric")) {
    stop("Input (cex.lab) is of wrong class.")
  }
  if (length(cex.lab) != 1) {
    stop("Input (cex.lab) is of wrong length.")
  }
  if (is.na(cex.lab)) {
    stop("Input (cex.lab) is not a positive integer.")
  }
  if (cex.lab < 0) {
    stop("Input (cex.lab) is not a positive integer.")
  }
  if (!is(col, "character")) {
    stop("Input (col) is of wrong class.")
  }
  if (!is(testonly, "logical")) {
    stop("Input (testonly) is of wrong class.")
  }
  
  a <- b <- result <- numeric()
  for(i in 1:length(null)){
    a <- rbind(a,
               quadratcount(ppp(null[[i]]$X,
                                null[[i]]$Y, 
                                window=window), 
                            nx=nr_sq, 
                            ny=nr_sq)
    )
    b <- rbind(b,
               quadratcount(ppp(alternative[[i]]$X, 
                                alternative[[i]]$Y, 
                                window=window),
                            nx=nr_sq,
                            ny=nr_sq)
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
                                 paired=TRUE,
                                 alternative="greater", 
                                 exact=FALSE)$p.value
    }   
  }
  null_all <- do.call("rbind", null)
  alt_all <- do.call("rbind", alternative)
  
  t1 <- ppp(null_all$X, 
            null_all$Y,
            window=window
  )
  t2 <- ppp(alt_all$X,
            alt_all$Y, 
            window=window
  )
  k2 <- density(t2,
                sigma=15
  ) 
  if(!testonly){
    p <- plot(k2, 
              las=1,
              main=title,
              axes=TRUE,
              box=FALSE,
              ribbon=ribbon,
              cex.axis=cex.axis,
              cex.lab=cex.lab,
              col=colorRampPalette(col)(n=255)
    )
    contour(k2, 
            add=TRUE
    )
  }
  Q1 <- quadratcount(t1,
                     nx=nr_sq,
                     ny=nr_sq
  )
  Q2 <- quadrat.test(t2,
                     nx=nr_sq,
                     ny=nr_sq
  )
  Q2$expected <- Q1
  #padj <- p.adjust(result, "fdr")
  result[is.nan(result)] <- 1
  Q2$residuals <- result #padj
  if(!testonly){
    p <- plot(Q2,
              add=TRUE,
              col=ifelse(result < 0.05, "lawngreen", 'white'),
              cex=ifelse(result < 0.05, 1.3, 1.1)
    )
    mtext(expression(paste("Distance ", "(", mu, "m)")),
          side=1,
          line=2
    )
    mtext(expression(paste("Distance ", "(", mu, "m)")),
          side=2, 
          line=2
    )
    if(ribbon){
      mtext("Kernel Density",
            side=4,
            line=1,
            cex=cex.lab
      )
    }
  }
  treated=as.numeric(Q2$observed)
  untreated=as.numeric(Q2$expected)
  if(method == "friedman"){
    frid=friedman.test(cbind(treated, untreated))
    text(300, 450, 
         paste("Friedman rank sum test:","\n p-value =", as.character(frid$p.value)),
         cex=1
    )
    if(!testonly){
      return(list(l1=frid,
                  l1=p))
    } else{
      return(frid)
    }
  }
  if(method == "wilcox"){
    wilc=wilcox.exact(treated,
                      untreated,
                      paired=TRUE,
                      alternative="greater",
                      exact=FALSE
    )
    text(300, 450, 
         paste("Friedman rank sum test:","\n p-value =", as.character(frid$p.value)),
         cex=1
    )
    if(!testonly){
      return(list(l1=wilc,
                  l1=p))
    } else{
      return(wilc)
    }
  }
}  

#'get.data
#'@description This function which from whole data set extract data elements of interest.
#'@param data data frame. Data set of spatial point pattern.
#'@param marker character. Indicate which marker to include in data set.
#'@param diet numeric. Indicate  which diet to include in data set.
#'@param mouse numeric. Indicate which mouse to include in data set.
#'@param arc numeric. Indicate which arc to include in data set.
#'@param cal_arc logical flag indicating whether to average over the ARCs. 
#'@examples
#'@author Viktorian Miok
get.data <- function(data,
                     marker, 
                     diet,
                     mouse,
                     arc, 
                     cal_arc){
  ###############################################################################
  #
  #       function which from whole data extract data set of interest
  #
  ###############################################################################
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
  result <- list()
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

#'distance.euclidean
#'@description This is a function for calculating euclidean distance.
#'@param a Matrix of first set of locations where each row gives the coordinates of a particular point.
#'@param b Matrix of second set of locations where each row gives the coordinates of a particular point. If this is missing a is used.
#'@examples
#'@author Viktorian Miok
distance.euclidean <- function(a, b) { 
  sqrt(sum((a - b)^2))
}

#'get.knn
#'@description Fast k-nearest neighbor searching algorithms including a kd-tree, cover-tree and the algorithm implemented in class package.
#'@param train_labels a character.
#'@param train_data a character. 
#'@param to_classify_data a character. 
#'@param k the maximum number of nearest neighbors to search. The default value is set to 10.
#'@param distance function for calculating the distance between the cells. 
#'@param standardize logical whether or not to standardize.
#'@examples
#'@author Viktorian Miok
get.knn <- function(train_labels,
                    train_data,
                    to_classify_data, 
                    k, 
                    distance=distance.euclidean, 
                    standardize=FALSE) {
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
    to_classify_data <- scale(to_classify_data,
                              center=attr(train_data, 
                                          "scaled:center"), 
                              scale=attr(train_data, 
                                         "scaled:scale")
    ) 
  }
  for (i in seq_len(num_preds)) {
    # compute distance to all training data points 
    distance_to_train <- apply(train_data,
                               MARGIN=1,
                               FUN=function(x) distance(x, to_classify_data[i, ])
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
  list(prediction=pred,
       levels=levels(train_labels)
  ) 
}

#'gg.aspp
#'@description This function for plotting for initial cellular spatial point pattern visualization.
#'@param data a vector of x and y coordinates of data point.
#'@param type_plot a character that indicate the type of the plot. On of the following: "hexagonal_heatmap", "scatter_plot", "polygon", "density_contour".
#'@param legend.text a character or expression vector of length ≥ 1 to appear in the legend.
#'@param legend.title a character string or length-one expression giving a title to be placed at the top of the legend. 
#'@param axis.text a character for both x and y axis texts.
#'@param axis.title a character for both x and y axis title.
#'@param plot.title a character that represent the title of the plot. 
#'@param xlim a observations not in this range will be dropped completely and not passed to any other layers. If a NA value is substituted for one of the limits that limit is automatically calculated.
#'@param ylim a Observations not in this range will be dropped completely and not passed to any other layers. If a NA value is substituted for one of the limits that limit is automatically calculated.
#'@examples
#'@author Viktorian Miok
gg.aspp <- function(data,
                    type_plot='scatter_plot',
                    title=" ",
                    legend.text=15,
                    legend.title=15,
                    axis.text=15,
                    axis.title=15,
                    plot.title=15,
                    xlim=c(0, 500), 
                    ylim=c(0, 500)){
  if (!is(data, "data.frame")) {
    stop("Input (data) is of wrong class.")
  }
  if (!is(type_plot, "character")) {
    stop("Input (type_plot) is of wrong class.")
  }
  if (is(type_plot, "character")) {
    if (!(type_plot %in% c("hexagonal_heatmap",
                           "scatter_plot",
                           "polygon",
                           "density_contour"))) {
      stop("Input (type_plot) ill-specified.")
    }
  } 
  if (!is(title, "character")) {
    stop("Input (title) is of wrong class.")
  }
  if (!is(legend.text, "numeric")) {
    stop("Input (legend.text) is of wrong class.")
  }
  if (length(legend.text) != 1) {
    stop("Input (legend.text) is of wrong length.")
  }
  if (is.na(legend.text)) {
    stop("Input (legend.text) is not a positive integer.")
  }
  if (legend.text < 0) {
    stop("Input (legend.text) is not a positive integer.")
  }
  if (!is(legend.title, "numeric")) {
    stop("Input (legend.title) is of wrong class.")
  }
  if (length(legend.title) != 1) {
    stop("Input (legend.title) is of wrong length.")
  }
  if (is.na(legend.title)) {
    stop("Input (legend.title) is not a positive integer.")
  }
  if (legend.title < 0) {
    stop("Input (legend.title) is not a positive integer.")
  }
  if (!is(axis.text, "numeric")) {
    stop("Input (axis.text) is of wrong class.")
  }
  if (length(axis.text) != 1) {
    stop("Input (axis.text) is of wrong length.")
  }
  if (is.na(axis.text)) {
    stop("Input (axis.text) is not a positive integer.")
  }
  if (axis.text < 0) {
    stop("Input (axis.text) is not a positive integer.")
  }
  if (!is(axis.title, "numeric")) {
    stop("Input (axis.title) is of wrong class.")
  }
  if (length(axis.title) != 1) {
    stop("Input (axis.title) is of wrong length.")
  }
  if (is.na(axis.title)) {
    stop("Input (axis.title) is not a positive integer.")
  }
  if (axis.title < 0) {
    stop("Input (axis.title) is not a positive integer.")
  }
  if (!is(plot.title, "numeric")) {
    stop("Input (plot.title) is of wrong class.")
  }
  if (length(plot.title) != 1) {
    stop("Input (plot.title) is of wrong length.")
  }
  if (is.na(plot.title)) {
    stop("Input (plot.title) is not a positive integer.")
  }
  if (plot.title < 0) {
    stop("Input (plot.title) is not a positive integer.")
  }
  if (!is(xlim, "numeric")) {
    stop("Input (xlim) is of wrong class.")
  }
  if (length(xlim) != 2) {
    stop("Input (xlim) is of wrong length.")
  }
  if (!is(ylim, "numeric")) {
    stop("Input (ylim) is of wrong class.")
  }
  if (length(ylim) != 2) {
    stop("Input (ylim) is of wrong length.")
  }
  
  if(type_plot == "hexagonal_heatmap"){
    # Hexagonal heatmap of 2d bin counts
    p <- ggplot(data, aes(x=X, y=Y) ) + 
      geom_hex(bins=70) + 
      scale_fill_continuous(type="viridis") + 
      theme_bw() +
      ggtitle(title) + 
      theme(legend.text=element_text(size=legend.text),
            legend.title=element_text(size=legend.title),
            axis.text=element_text(size=axis.text),
            axis.title=element_text(size=axis.title),
            plot.title=element_text(size=plot.title)) + 
      xlim(xlim) +
      ylim(ylim)
    return(p)
  }
  if(type_plot == 'scatter_plot'){
    # scatter plot
    p <- ggplot(data, aes(x=X, y=Y)) + 
      stat_density2d(geom="tile",
                     aes(fill=..density..),
                     contour=FALSE) + 
      geom_point(colour="white") +
      ggtitle(title) +
      theme(legend.text=element_text(size=legend.text),
            legend.title=element_text(size=legend.title),
            axis.text=element_text(size=axis.text),
            axis.title=element_text(size=axis.title),
            plot.title=element_text(size=plot.title)) + 
      xlim(xlim) +
      ylim(ylim)
    return(p)
  }
  if(type_plot == "polygon"){
    p <- ggplot(data, aes(x=X, y=Y)) + 
      geom_point() +
      stat_density_2d(aes(fill=..level..), 
                      alpha=0.1, 
                      geom="polygon") +
      theme(legend.text=element_text(size=legend.text),
            legend.title=element_text(size=legend.title),
            axis.text=element_text(size=axis.text),
            axis.title=element_text(size=axis.title),
            plot.title=element_text(size=plot.title))
    
    dat_lims <- lapply(data, function(v) c(min(v), max(v)))
    plot_lims <- ggplot_build(p)$panel$ranges[[1]][c("x.range",
                                                     "y.range")]
    
    p + scale_x_continuous(limits=dat_lims$x * 1.1) + 
      scale_y_continuous(limits=dat_lims$y * 1.1) +
      coord_cartesian(xlim=plot_lims$x.range,
                      ylim=plot_lims$y.range)
    return(p)
  }
  if(type_plot == "density_contour"){
    #density plot with contours
    p <- ggplot(data, aes(x=X, y=Y)) +
      stat_density_2d(aes(fill=..density..), 
                      geom="raster", 
                      contour=FALSE) +
      scale_fill_distiller(palette=4, 
                           direction=1) + 
      geom_density_2d(color="white") + 
      ggtitle(title) + 
      theme(legend.text=element_text(size=legend.text),
            legend.title=element_text(size=legend.title),
            axis.text=element_text(size=axis.text),
            axis.title=element_text(size=axis.title),
            plot.title=element_text(size=plot.title)) +
      xlim(xlim) +
      ylim(ylim)
    return(p)
  } 
}



#'mdim.scal
#'@description This function for multidimensional scaling based on random forest proximity.
#'@param dataX vector of elements that represent x-axis values of the point patterns.
#'@param dataY vector of elements that represent y-axis values of the point patterns.
#'@param dataG vector of elements that represent group values of the point patterns.
#'@examples
#'@author Viktorian Miok
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
  spiral_rf <- randomForest(x=cbind(dataX, dataY),
                            y=as.factor(dataG),
                            ntree=10000,
                            proximity=TRUE,
                            oob.prox=TRUE,
  )
  
  # convert proximities into distances
  proximity_to_dist <- function(proximity) {
    1 - proximity
  }
  spiral_dist <- proximity_to_dist(spiral_rf$proximity)
  
  spiral_mds <- as.data.frame(cmdscale(spiral_dist))
  spiral_mds$class <- as.factor(dataG)
  # plot the result, sweet
  p <- ggplot(data=spiral_mds, aes(x=V1,
                                   y=V2, 
                                   colour=class)) +
    geom_point() +
    labs(x="1st dimension",
         y="2nd dimension",
         title="Multidimensional scaling based 
                                      on random forest proximity"
    )
  return(p)
}

#'neighbor.ratio
#'@description This function calculate over all percentage of the cells in the neighborhood within the diameter.
#'@param dataX vector of elements that represent x-axis values of the point patterns.
#'@param dataY vector of elements that represent y-axis values of the point patterns.
#'@param dataG vector of elements that represent group values of the point patterns.
#'@param diam number represent the diameter.
#'@param per_marker a logical.
#'@examples
#'@author Viktorian Miok
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
  geo.dist <- dist(cbind(dataX, dataY))
  geo.dist <- as.matrix(geo.dist)
  geo.dist[geo.dist > diam] <- 0
  
  if(per_marker){
    out <- matrix(0, nrow(geo.dist), 3)
    for(i in 1:nrow(geo.dist)){
      s <- length(dataG[geo.dist[i,] != 0])
      out[i,1] <- sum(dataG[geo.dist[i,] != 0] == 'gfap_only')/s
      out[i,2] <- sum(dataG[geo.dist[i,] != 0] == 'aldh_only')/s
      out[i,3] <- sum(dataG[geo.dist[i,] != 0] == 'both')/s 
    }
    colnames(out) <- c('gfap_only', 'aldh_only', 'both')
    return(out)
  } else{
    res <- numeric()
    for(i in 1:nrow(geo.dist)){
      tab <- table(dataG[geo.dist[i,] != 0])
      res <- c(res, as.numeric(tab/sum(tab)))
    }
    return(res)
  }
}

#'plot.2d.classify
#'@description Function for a general classification method with two features to visualize.
#'@param to_classify_labels a character.
#'@param to_classify_data a character.
#'@param classify_method a character 
#'@param X1 a character.
#'@param X2 a character.
#'@param lengthX1 a numeric indicating the number for x-axis.
#'@param lengthX2 a numeric indicating the number for y-axis.
#'@param title a character indicating title of the plot.
#'@param plot_class_rate a logical whether to plot or not class rate.
#'@examples
#'@author Viktorian Miok
plot.2d.classify <- function(to_classify_labels,
                             to_classify_data, 
                             classify_method,
                             X1,
                             X2,
                             lengthX1=100,
                             lengthX2=100,
                             title="",
                             plot_class_rate=TRUE) {
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
  gridX1 <- seq(min(to_classify_data[, X1]), 
                max(to_classify_data[, X1]), 
                length.out=lengthX1
  )
  gridX2 <- seq(min(to_classify_data[, X2]), 
                max(to_classify_data[, X2]), 
                length.out=lengthX2
  )
  # compute grid coordinates with cartesian product
  grid_data <- expand.grid(gridX1, gridX2) 
  names(grid_data) <- c(X1, X2)
  # assign grid cells to classes based on classification rule:
  grid_result <- classify_method( 
    to_classify_data=grid_data
  )
  grid_data$prediction <- grid_result$prediction
  # assign data to be classified based on classification rule &
  # check these "predictions"
  to_check_result <- classify_method(
    to_classify_data=to_classify_data[, c(X1, X2)] )
  to_classify_data$class <- to_classify_labels
  to_classify_data$correct <- 
    (to_check_result$prediction == to_classify_labels)
  
  ggplot() + 
    geom_raster(data=grid_data,
                aes_string(x=X1,
                           y=X2,
                           fill="prediction"), 
                alpha=.8) + 
    geom_point(data=to_classify_data,
               aes_string(x=X1,
                          y=X2,
                          shape="class",
                          color="correct"), 
               alpha=.8) +  
    scale_color_manual(values=c("TRUE"="darkgray", 
                                "FALSE"="white"),
                       guide=FALSE ) +
    labs(fill="Class",
         shape="Class",
         title=parse(text=if(plot_class_rate) paste0(
           title, '~ ": " ~',
           100 * round(mean(to_classify_data$correct), 4),
           '~ "% correctly classified"')
           else title )) +
    theme(plot.title=element_text(size=16)) 
}


#'plot.2d.knn
#'@description Function for simple k-NN with two features to visualize class boundaries.
#'@param train_labels a character.
#'@param train_data a character.
#'@param k the maximum number of nearest neighbors to search. The default value is set to 10.
#'@param X1 a charter.
#'@param X2 a charter.
#'@param distance function for calculating the distance between the cells.
#'@param standardize logical whether or not to standardize.
#'@param to_classify_labels a character.
#'@param to_classify_data a character.
#'@param lengthX1 a numeric indicating the number for x-axis.
#'@param lengthX2 a numeric indicating the number for y-axis.
#'@param plot_class_rate a logical whether to plot or not class rate.
#'@examples
#'@author Viktorian Miok
plot.2d.knn <- function(train_labels,
                        train_data,
                        k,
                        X1,
                        X2,
                        distance=distance.euclidean,
                        standardize=FALSE,
                        to_classify_labels=train_labels,
                        to_classify_data=train_data,
                        lengthX1=100, 
                        lengthX2=100,
                        plot_class_rate=TRUE) {
  # function for simple k-NN with two features to visualize class boundaries 
  # X1 and X2 are the names of the two features to use
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
  plot.2d.classify(to_classify_labels=to_classify_labels,
                   to_classify_data=to_classify_data,
                   classify_method <- function(to_classify_data){
                     get.knn(train_labels=train_labels,
                             train_data=train_data[, c(X1, X2)], 
                             to_classify_data=to_classify_data, 
                             k=k, 
                             distance=distance, 
                             standardize=standardize
                     )},
                   X1,
                   X2,
                   lengthX1=lengthX1,
                   lengthX2=lengthX2,
                   title=paste0("k == ", k),
                   plot_class_rate=plot_class_rate
  )
}


#'randF.class
#'@description This function for plotting of random forest classification.
#'@param dataX vector of elements that represent x-axis values of the point patterns.
#'@param dataY vector of elements that represent y-axis values of the point patterns.
#'@param dataG vector of elements that represent group values of the point patterns.
#'@examples
#'@author Viktorian Miok
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
                            y=factor(dataG)
  )
  colnames(spiral_data) <- c("x1", "x2", "y")
  # set features that should be inspected within the regions
  features <- c("x1", "x2")
  spiral_task <- TaskClassif$new(id="spirals",
                                 backend=spiral_data,
                                 target="y"
  )
  plot_learner_prediction(
    lrn("classif.rpart",
        predict_type="prob"),
    spiral_task
  )
  
  plot_learner_prediction(
    lrn("classif.ranger", 
        predict_type="prob"),
    spiral_task
  )
}


#'spp.viz
#'@description This function for visualization and testing of spatial point patterns - all combined.
#'@param data Vector of x and y coordinates of data point.
#'@param data1 Vector of x and y coordinates of data point.
#'@param test_null the null model object to plot.
#'@param test_alt the alternative model object to plot.
#'@param method method employed for testing "friedman" or "wilcoxon".
#'@param nr_sq number of squares for the polygon horizontal axis.
#'@param z_lim plotting limits.
#'@param type_plot type of the plot to be generated "hexagonal_heatmap", "scatter_plot", "polygon", "density_contour","statspat_square_plot", "statspat_plot_subtract", "statspat_plot"
#'@param title character indicate title of the plot.
#'@param window window of observation, an object of class "owin".
#'@param legend.text a character or expression vector of length ≥ 1 to appear in the legend.
#'@param legend.title a character string or length-one expression giving a title to be placed at the top of the legend. 
#'@param axis.text a character for both x and y axis texts.
#'@param axis.title a character for both x and y axis title.
#'@param plot.title a character that represent the title of the plot. 
#'@param cex.axis specify the size of the tick label numbers/text with a numeric value of length 1.
#'@param cex.lab specify the size of the axis label text with a numeric value of length 1.
#'@param col The colors to be used for the background.
#'@param xlim a observations not in this range will be dropped completely and not passed to any other layers. If a NA value is substituted for one of the limits that limit is automatically calculated.
#'@param ylim a observations not in this range will be dropped completely and not passed to any other layers. If a NA value is substituted for one of the limits that limit is automatically calculated.
#'@param num_size number indicating the size of the labels.
#'@param num_size_sig number indicating the size of the labels.
#'@param ribbon logical flag indicating whether to display a ribbon showing the colour map. Default is TRUE for new plots and FALSE for added plots.
#'@param contour create a contour plot, or add contour lines to an existing plot.
#'@param bias bias >1 puts more colors at high values.
#'@param add_star_sig logical flag indicating whether to add or not star in the square.
#'@param test_overall logical flag indicating whether to test or not.
#'@examples
#'@author Viktorian Miok
spp.viz <- function(data, 
                    data1=NULL,
                    test_null=NULL,
                    test_alt=NULL,
                    method="friedman",
                    nr_sq=10,
                    z_lim=NULL,
                    type_plot="statspat_plot", 
                    title=" ", 
                    window, 
                    legend.text=15,
                    legend.title=15,
                    axis.text=15,
                    axis.title=15,
                    plot.title=15,
                    cex.axis=1,
                    cex.lab=1,
                    col=c("blue", "white", "red"),
                    xlim, 
                    ylim,
                    num_size=1.2,
                    num_size_sig=1.5,
                    ribbon=FALSE,
                    contour=TRUE,
                    bias=NULL,
                    add_star_sig=FALSE,
                    test_overall=TRUE){
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
  if (!is(method, "character")) {
    stop("Input (method) is of wrong class.")
  }
  if (!is(method, "character")) {
    if (!(method %in% c("friedman",
                        "wilcox"))) {
      stop("Input (method) ill-specified.")
    }
  } 
  if (!is(type_plot, "character")) {
    stop("Input (type_plot) is of wrong class.")
  }
  if (is(type_plot, "character")) {
    if (!(type_plot %in% c("hexagonal_heatmap",
                           "scatter_plot",
                           "polygon",
                           "density_contour",
                           "statspat_square_plot",
                           "statspat_plot_subtract",
                           "statspat_plot",
                           "3D_plot",
                           "3D_plot_interactive"))) {
      stop("Input (type_plot) ill-specified.")
    }
  } 
  if (!is(title, "character")) {
    stop("Input (title) is of wrong class.")
  }
  if (!is(window, "owin")) {
    stop("Input (window) is of wrong class.")
  }
  if(type_plot %in% c("hexagonal_heatmap",
                      "scatter_plot",
                      "polygon",
                      "density_contour")){
    if (!is(xlim, "numeric")) {
      stop("Input (xlim) is of wrong class.")
    }
    if (length(xlim) != 2) {
      stop("Input (xlim) is of wrong length.")
    }
    if (!is(ylim, "numeric")) {
      stop("Input (ylim) is of wrong class.")
    }
    if (length(ylim) != 2) {
      stop("Input (ylim) is of wrong length.")
    }
  }
  if (!is(nr_sq, "numeric")) {
    stop("Input (nr_sq) is of wrong class.")
  }
  if (length(nr_sq) != 1) {
    stop("Input (nr_sq) is of wrong length.")
  }
  if (is.na(nr_sq)) {
    stop("Input (nr_sq) is not a positive integer.")
  }
  if (nr_sq < 0) {
    stop("Input (nr_sq) is not a positive integer.")
  }
  if (!is(legend.text, "numeric")) {
    stop("Input (legend.text) is of wrong class.")
  }
  if (length(legend.text) != 1) {
    stop("Input (legend.text) is of wrong length.")
  }
  if (is.na(legend.text)) {
    stop("Input (legend.text) is not a positive integer.")
  }
  if (legend.text < 0) {
    stop("Input (legend.text) is not a positive integer.")
  }
  if (!is(legend.title, "numeric")) {
    stop("Input (legend.title) is of wrong class.")
  }
  if (length(legend.title) != 1) {
    stop("Input (legend.title) is of wrong length.")
  }
  if (is.na(legend.title)) {
    stop("Input (legend.title) is not a positive integer.")
  }
  if (legend.title < 0) {
    stop("Input (legend.title) is not a positive integer.")
  }
  if (!is(axis.text, "numeric")) {
    stop("Input (axis.text) is of wrong class.")
  }
  if (length(axis.text) != 1) {
    stop("Input (axis.text) is of wrong length.")
  }
  if (is.na(axis.text)) {
    stop("Input (axis.text) is not a positive integer.")
  }
  if (axis.text < 0) {
    stop("Input (axis.text) is not a positive integer.")
  }
  if (!is(axis.title, "numeric")) {
    stop("Input (axis.title) is of wrong class.")
  }
  if (length(axis.title) != 1) {
    stop("Input (axis.title) is of wrong length.")
  }
  if (is.na(axis.title)) {
    stop("Input (axis.title) is not a positive integer.")
  }
  if (axis.title < 0) {
    stop("Input (axis.title) is not a positive integer.")
  }
  if (!is(plot.title, "numeric")) {
    stop("Input (plot.title) is of wrong class.")
  }
  if (length(plot.title) != 1) {
    stop("Input (plot.title) is of wrong length.")
  }
  if (is.na(plot.title)) {
    stop("Input (plot.title) is not a positive integer.")
  }
  if (plot.title < 0) {
    stop("Input (plot.title) is not a positive integer.")
  }
  
  if(type_plot == "hexagonal_heatmap"){
    # Hexagonal heatmap of 2d bin counts
    p <- ggplot(data, aes(x=X, y=Y) ) + 
      geom_hex(bins=70) + 
      scale_fill_continuous(type="viridis") + 
      theme_bw() +
      ggtitle(title) + 
      theme(legend.text=element_text(size=legend.text),
            legend.title=element_text(size=legend.title),
            axis.text=element_text(size=axis.text),
            axis.title=element_text(size=axis.title),
            plot.title=element_text(size=plot.title)) + 
      xlim(xlim) +
      ylim(ylim)
    return(p)
  }
  if(type_plot == 'scatter_plot'){
    # scatter plot
    p <- ggplot(data, aes(x=X, y=Y)) + 
      stat_density2d(geom="tile",
                     aes(fill=..density..),
                     contour=FALSE) + 
      geom_point(colour="white") +
      ggtitle(title) +
      theme(legend.text=element_text(size=legend.text),
            legend.title=element_text(size=legend.title),
            axis.text=element_text(size=axis.text),
            axis.title=element_text(size=axis.title),
            plot.title=element_text(size=plot.title)) + 
      xlim(xlim) +
      ylim(ylim)
    return(p)
  }
  if(type_plot == "polygon"){
    p <- ggplot(data, aes(x=X, y=Y)) + 
      geom_point() +
      stat_density_2d(aes(fill=..level..), 
                      alpha=0.1, 
                      geom="polygon") +
      theme(legend.text=element_text(size=legend.text),
            legend.title=element_text(size=legend.title),
            axis.text=element_text(size=axis.text),
            axis.title=element_text(size=axis.title),
            plot.title=element_text(size=plot.title)) +
      xlim(xlim) +
      ylim(ylim)
    
    dat_lims <- lapply(data, 
                       function(v) c(min(v), max(v))
    )
    plot_lims <- ggplot_build(p)$panel$ranges[[1]][c("x.range",
                                                     "y.range")]
    
    p + scale_x_continuous(limits=dat_lims$x * 1.1) + 
      scale_y_continuous(limits=dat_lims$y * 1.1) +
      coord_cartesian(xlim=plot_lims$x.range,
                      ylim=plot_lims$y.range)
    return(p)
  }
  if(type_plot == "density_contour"){
    #density plot with contours
    p <- ggplot(data, aes(x=X, y=Y)) +
      stat_density_2d(aes(fill=..density..), 
                      geom="raster", 
                      contour=FALSE) +
      scale_fill_distiller(palette=4, 
                           direction=1) + 
      geom_density_2d(color="white") + 
      ggtitle(title) + 
      theme(legend.text=element_text(size=legend.text),
            legend.title=element_text(size=legend.title),
            axis.text=element_text(size=axis.text),
            axis.title=element_text(size=axis.title),
            plot.title=element_text(size=plot.title)) +
      xlim(xlim) +
      ylim(ylim)
    return(p)
  } 
  if(type_plot == "statspat_square_plot"){
    t1 <- ppp(data$X,
              data$Y,
              window=window
    )
    Q1 <- quadratcount(t1, 
                       nx=nr_sq,
                       ny=nr_sq
    )
    if(!is.null(data1)){
      t2 <- ppp(data1$X,
                data1$Y,
                window=window
      )
      Q2 <- quadratcount(t2, 
                         nx=nr_sq,
                         ny=nr_sq
      )
      if(is.null(z_lim)){
        plot(intensity(Q1-Q2, image=TRUE), 
             main=title, 
             las=1,
             axes=TRUE,
             box=FALSE,
             ribbon=ribbon,
             cex.axis=cex.axis,
             cex.lab=cex.lab,
             col=colorRampPalette(col, 
                                  bias=ifelse(is.null(bias),
                                              1,
                                              bias))(n=255)
        )
      } else{ plot(intensity(Q1-Q2, image=TRUE), 
                   main=title, 
                   las=1, 
                   zlim=z_lim, 
                   axes=TRUE,
                   box=FALSE,
                   ribbon=ribbon,
                   cex.axis=cex.axis,
                   cex.lab=cex.lab,
                   col=colorRampPalette(col, 
                                        bias=ifelse(is.null(bias),
                                                    1,
                                                    bias))(n=255)
      )
      }
    } else { if(is.null(z_lim)){
      plot(intensity(Q1, image=TRUE), 
           main=title, 
           las=1, 
           axes=TRUE,
           box=FALSE,
           ribbon=ribbon,
           cex.axis=cex.axis,
           cex.lab=cex.lab,
           col=colorRampPalette(col, 
                                bias=ifelse(is.null(bias), 
                                            1,
                                            bias))(n=255)
      )
    } else{plot(intensity(Q1, image=TRUE), 
                main=title, 
                las=1, 
                zlim=z_lim, 
                axes=TRUE,
                box=FALSE,
                ribbon=ribbon,
                cex.axis=cex.axis,
                cex.lab=cex.lab,
                col=colorRampPalette(col, 
                                     bias=ifelse(is.null(bias),
                                                 1,
                                                 bias))(n=255)
    )
    }
    }  
    mtext(expression(paste("Distance ", "(", mu, "m)")),
          side=1,
          line=3,
          cex=cex.lab
    )
    mtext(expression(paste("Distance ", "(", mu, "m)")),
          side=2, 
          line=2,
          cex=cex.lab
    )
    if(ribbon){
      mtext("Kernel Density", 
            side=4, 
            line=1, 
            cex=cex.lab
      )
    }
  }
  if(type_plot == "statspat_plot_subtract"){
    t1 <- ppp(data$X,
              data$Y,
              window=window
    )
    t2 <- ppp(data1$X,
              data1$Y,
              window=window
    )
    k1 <- density(t1,
                  sigma=15
    ) 
    k2 <- density(t2,
                  sigma=15
    ) 
    if(is.null(z_lim)){
      p <- plot(k2 - k1, 
                las=1, 
                main=title,
                axes=TRUE,
                box=FALSE,
                ribbon=ribbon,
                col=colorRampPalette(col, 
                                     bias=ifelse(is.null(bias),
                                                 1,
                                                 bias))(n=255),
                cex.axis=cex.axis,
                cex.lab=cex.lab
      ) 
    } else{
      p <- plot(k2 - k1, 
                las=1, 
                main=title,
                zlim=z_lim,
                axes=TRUE,
                box=FALSE,
                ribbon=ribbon,
                col=colorRampPalette(col, 
                                     bias=ifelse(is.null(bias),
                                                 1,
                                                 bias))(n=255),
                cex.axis=cex.axis,
                cex.lab=cex.lab
      ) 
    }
    if(contour)
      contour(k2 - k1, 
              add=TRUE
      )
    mtext(expression(paste("Distance ", "(", mu, "m)")),
          side=1, 
          line=3, 
          cex=cex.lab
    )
    mtext(expression(paste("Distance ", "(", mu, "m)")),
          side=2,
          line=2,
          cex=cex.lab
    )
    if(ribbon){
      mtext("Kernel Density", 
            side=4,
            line=1,
            cex=cex.lab
      )
    }
  } 
  if(type_plot == "statspat_plot"){
    # density plot with contour and number of points per square
    t1 <- ppp(data$X,
              data$Y,
              window=window
    )
    k1 <- density(t1,
                  sigma=15
    ) 
    if(is.null(z_lim)){
      p <- plot(k1, 
                las=1, 
                main=title,
                axes=TRUE,
                box=FALSE,
                ribbon=ribbon,
                col=colorRampPalette(col)(n=255),
                cex.axis=cex.axis,
                cex.lab=cex.lab
      ) 
    } else{
      p <- plot(k1, 
                las=1, 
                main=title,
                zlim=z_lim,
                axes=TRUE,
                box=FALSE,
                ribbon=ribbon,
                col=colorRampPalette(col)(n=255),
                cex.axis=cex.axis,
                cex.lab=cex.lab
      ) 
    }
    if(contour){
      contour(k1, 
              add=TRUE
      )
    }
    Q1 <- quadratcount(t1, 
                       nx=nr_sq,
                       ny=nr_sq
    )
    if(!is.null(test_null)){
      result <- test.squares(null=test_null, 
                             alt=test_alt, 
                             nr_sq=nr_sq,
                             window=window,
                             method="friedman"
      )
      result[is.nan(result)] <- 1
      if(add_star_sig){
        for(i in 1:length(Q1)) {
          if(result[i] < 0.05) Q1[i] <- paste(Q1[i],
                                              "*",
                                              sep="")
        } 
      }
      p <- plot(Q1,
                add=TRUE,
                col=ifelse(result < 0.05, "green3", 'white'), # lawngreen
                cex=ifelse(result < 0.05, num_size_sig, num_size)
      )
      if(test_overall){
        out <- diff.aspp(null=test_null, 
                         alternative=test_alt, 
                         nr_sq=nr_sq,
                         window=window,
                         method="friedman",
                         testonly=TRUE
        )
      }
    } else {
      p <- plot(Q1,
                add=TRUE,
                col="white", 
                cex=num_size,
                cex.axis=cex.axis,
                cex.lab=cex.lab
      )
    }
    mtext(expression(paste("Distance ", "(", mu, "m)")),
          side=1,
          line=3,
          cex=cex.lab
    )
    mtext(expression(paste("Distance ", "(", mu, "m)")),
          side=2,
          line=2,
          cex=cex.lab
    )
    if(ribbon){
      mtext("Kernel Density", 
            side=4,
            line=1,
            cex=cex.lab
      )
    }
  }
  if(type_plot == "3D_plot"){
    k1 <- density(ppp(data$X,
                      data$Y,
                      window=window),
                  sigma=15
    ) 
    persp(k1, 
          col="lightblue",
          theta=40,
          phi=35,
          main=title
    )
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
#'test.saptcorr
#'@description This function for spatial auto-correlation employing Moran.I and Mantel test.
#'@param dataX vector of elements that represent x-axis values of the point patterns.
#'@param dataY vector of elements that represent y-axis values of the point patterns.
#'@param dataG vector of elements that represent group values of the point patterns.
#'@param method character representing the statistical method employed, either Mentel or Moran.I
#'@examples
#'@author Viktorian Miok
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
  my.data1 <- data.frame(X=dataX,
                         Y=dataY, 
                         Gene=as.numeric(factor(dataG))
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
                       scaled=FALSE,
                       na.rm=FALSE, 
                       alternative="two.sided"){
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
                                    wanted to set na.rm=TRUE?")
        return(list(observed=NA, 
                    expected=ei,
                    sd=NA, 
                    p.value=NA))
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
    alternative <- match.arg(alternative,
                             c("two.sided", "less", "greater")
    )
    pv <- pnorm(obs,
                mean=ei,
                sd=sdi
    )
    if (alternative == "two.sided") 
      pv <- if (obs <= ei) 2 * pv
    else 2 * (1 - pv)
    if (alternative == "greater") pv <- 1 - pv
    list(observed=obs,
         expected=ei,
         sd=sdi,
         p.value=pv)
  }
  # Mantel test function
  mantel.test <- function (m1,
                           m2,
                           nperm=999, 
                           graph=FALSE, 
                           alternative="two.sided", ...){
    alternative <- match.arg(alternative, c("two.sided", 
                                            "less", 
                                            "greater"))
    n <- nrow(m1)
    realz <- mant.zstat(m1, m2)
    nullstats <- replicate(nperm, 
                           mant.zstat(m1, perm.rowscols(m2,n))
    )
    pval <- switch(alternative, 
                   two.sided=2 * min(sum(nullstats >= realz), 
                                     sum(nullstats <= realz)),
                   less=sum(nullstats <= realz), 
                   greater=sum(nullstats >= realz))
    pval <- (pval + 1)/(nperm + 1)
    if (alternative == "two.sided" && pval > 1) 
      pval <- 1
    if (graph) {
      plot(density(nullstats), type="l", ...)
      abline(v=realz)
    }
    list(z.stat=realz, 
         p=pval,
         alternative=alternative)
  }
  
  # points with similar values 
  # (astrocyte with similar marker) close to each other
  if(method=="Moran.I"){
    result <- Moran.I(x=my.data1$Gene,
                      weight=my.dists.inv1
    )
  }
  if(method=="Mantel"){
    result <- mantel.test(m1=as.matrix(dist(my.data1$Gene)),
                          m2=my.dists
    )
  }
  return(result)
}


#'test.squares
#'@description This function for testing of square comparison of cellular spatial point patterns.
#'@param null the null model object to plot.
#'@param alt the alternative model object to plot.
#'@param method method employed for testing "friedman" or "wilcoxon".
#'@param window window of observation, an object of class "owin".
#'@param nr_sq number of squares for the poligon horizontal axis.
#'@examples
#'@author Viktorian Miok
test.squares <- function(null=dat1,
                         alt=dat2,
                         method="friedman",
                         window=window,
                         nr_sq=10){
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
                                window=window),
                            nx=nr_sq,
                            ny=nr_sq)
    )
    b <- rbind(b, 
               quadratcount(ppp(alt[[i]]$X, 
                                alt[[i]]$Y,
                                window=window),
                            nx=nr_sq, 
                            ny=nr_sq)
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
                                 paired=TRUE,
                                 alternative="greater",
                                 exact=FALSE)$p.value
    }   
  }
  #result <- p.adjust(result, "fdr")
  return(result)
}

