
###############################################################################
#
#       Plots for visualization and testing of spatial point patterns
#
###############################################################################

dens.aspp <- function(data, 
                      data1 = NULL,
                      test_null = NULL,
                      test_alt = NULL,
                      method = "friedman",
                      nr_sq = 10,
                      z_lim = NULL,
                      type_plot = "test_plot", 
                      title = " ", 
                      window, 
                      cex.axis = 1,
                      cex.lab = 1,
                      col = c("blue", "white", "red"),
                      num_size = 1.2,
                      num_size_sig = 1.5,
                      ribbon = FALSE,
                      contour = TRUE,
                      bias = NULL,
                      add_star_sig = FALSE,
                      test_overall = TRUE,
                      add_square_counts = TRUE){
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
                        window = window
              )
              Q1 <- quadratcount(t1, 
                                 nx = nr_sq,
                                 ny = nr_sq
              )
              if(!is.null(data1)){
                t2 <- ppp(data1$X,
                          data1$Y,
                          window = window
                )
                Q2 <- quadratcount(t2, 
                                   nx = nr_sq,
                                   ny = nr_sq
                )
                if(is.null(z_lim)){
                  plot(intensity(Q2-Q1, image = TRUE), 
                       main = title, 
                       las = 1,
                       zlim = c(-max(abs(intensity(Q2-Q1, image = TRUE)$v), na.rm=T),
                                max(abs(intensity(Q2-Q1, image = TRUE)$v), na.rm=T)),
                       axes = TRUE,
                       box = FALSE,
                       ribbon = ribbon,
                       cex.axis = cex.axis,
                       cex.lab = cex.lab,
                       col = colorRampPalette(col, 
                                              bias = ifelse(is.null(bias),
                                                            1,
                                                            bias))(n = 255)
                  )
                } else{ plot(intensity(Q2-Q1, image = TRUE), 
                             main = title, 
                             las = 1, 
                             zlim = z_lim, 
                             axes = TRUE,
                             box = FALSE,
                             ribbon = ribbon,
                             cex.axis = cex.axis,
                             cex.lab = cex.lab,
                             col = colorRampPalette(col, 
                                                    bias = ifelse(is.null(bias),
                                                                  1,
                                                                  bias))(n = 255)
                )
                }
              } else { if(is.null(z_lim)){
                plot(intensity(Q1, image = TRUE), 
                     main = title, 
                     las = 1, 
                     axes = TRUE,
                     box = FALSE,
                     ribbon = ribbon,
                     cex.axis = cex.axis,
                     cex.lab = cex.lab,
                     col = colorRampPalette(col, 
                                            bias = ifelse(is.null(bias), 
                                                          1,
                                                          bias))(n = 255)
                )
              } else{plot(intensity(Q1, image = TRUE), 
                          main = title, 
                          las = 1, 
                          zlim = z_lim, 
                          axes = TRUE,
                          box = FALSE,
                          ribbon = ribbon,
                          cex.axis = cex.axis,
                          cex.lab = cex.lab,
                          col = colorRampPalette(col, 
                                                 bias = ifelse(is.null(bias),
                                                               1,
                                                               bias))(n = 255)
              )
              }
              }  
              mtext(expression(paste("Distance ", "(", mu, "m)")),
                    side = 1,
                    line = 2,
                    cex = cex.lab
              )
              mtext(expression(paste("Distance ", "(", mu, "m)")),
                    side = 2, 
                    line = 1,
                    cex = cex.lab
              )
              if(ribbon){
                mtext("Kernel Density", side = 4, line = 1, cex = cex.lab)
              }
            }
            if(type_plot == "subtraction_dens_plot"){
              t1 <- ppp(data$X,
                        data$Y,
                        window = window
              )
              t2 <- ppp(data1$X,
                        data1$Y,
                        window = window
              )
              k1 <- density(t1,
                            sigma = 15
              ) 
              k2 <- density(t2,
                            sigma = 15
              ) 
              if(is.null(z_lim)){
                p <- plot(k2 - k1, 
                          las = 1, 
                          main = title,
                          zlim = c(-max(abs((k2-k1)$v), na.rm = T),max(abs((k2-k1)$v), na.rm = T)),
                          axes = TRUE,
                          box = FALSE,
                          ribbon = ribbon,
                          col = colorRampPalette(col, 
                                                 bias = ifelse(is.null(bias),
                                                               1,
                                                               bias))(n = 255),
                          cex.axis = cex.axis,
                          cex.lab = cex.lab
                ) 
              } else{
                p <- plot(k2 - k1, 
                          las = 1, 
                          main = title,
                          zlim = z_lim,
                          axes = TRUE,
                          box = FALSE,
                          ribbon = ribbon,
                          col = colorRampPalette(col, 
                                                 bias = ifelse(is.null(bias),
                                                               1,
                                                               bias))(n = 255),
                          cex.axis = cex.axis,
                          cex.lab = cex.lab
                ) 
              }
              if(contour)
                contour(k2 - k1, 
                        add = TRUE
                )
              mtext(expression(paste("Distance ", "(", mu, "m)")),
                    side = 1, 
                    line = 3, 
                    cex = cex.lab
              )
              mtext(expression(paste("Distance ", "(", mu, "m)")),
                    side = 2,
                    line = 2,
                    cex = cex.lab
              )
              if(ribbon){
                mtext("Kernel Density", side = 4, line = 1, cex = cex.lab)
              }
            } 
            if(type_plot == "test_plot"){
              # density plot with contour and number of points per square
              t1 <- ppp(data$X,
                        data$Y,
                        window = window
              )
              k1 <- density(t1,
                            sigma = 15
              ) 
              if(is.null(z_lim)){
                p <- plot(k1, 
                          las = 1, 
                          main = title,
                          axes = TRUE,
                          box = FALSE,
                          ribbon = ribbon,
                          col = colorRampPalette(col)(n = 255),
                          cex.axis = cex.axis,
                          cex.lab = cex.lab
                ) 
              } else{
                p <- plot(k1, 
                          las = 1, 
                          main = title,
                          zlim = z_lim,
                          axes = TRUE,
                          box = FALSE,
                          ribbon = ribbon,
                          col = colorRampPalette(col)(n = 255),
                          cex.axis = cex.axis,
                          cex.lab = cex.lab
                ) 
              }
              if(contour){
                contour(k1, 
                        add = TRUE
                )
              }
              Q1 <- quadratcount(t1, 
                                 nx = nr_sq,
                                 ny = nr_sq
              )
              if(!is.null(test_null)){
                result <- test.squares(null = test_null, 
                                       alt = test_alt, 
                                       nr_sq = nr_sq,
                                       window = window,
                                       method = "friedman"
                )
                result[is.nan(result)] <- 1
                if(add_star_sig){
                  for(i in 1:length(Q1)) {
                    if(result[i] < 0.05) Q1[i] <- paste(Q1[i],
                                                        "*",
                                                        sep = "")
                  } 
                }
                p <- plot(Q1,
                          add = TRUE,
                          col = ifelse(result < 0.05, "green3", 'white'), # lawngreen
                          cex = ifelse(result < 0.05, num_size_sig, num_size)
                )
                if(test_overall){
                  out <- diff.aspp(null = test_null, 
                                   alternative = test_alt, 
                                   nr_sq = nr_sq,
                                   window = window,
                                   method = "friedman",
                                   testonly = TRUE
                  )
                }
              } else {
                if(add_square_counts){
                  p <- plot(Q1,
                            add = TRUE,
                            col = "white", 
                            cex = num_size,
                            cex.axis = cex.axis,
                            cex.lab = cex.lab
                  )
                }
              }
              mtext(expression(paste("Distance ", "(", mu, "m)")),
                    side = 1,
                    line = 3,
                    cex = cex.lab
              )
              mtext(expression(paste("Distance ", "(", mu, "m)")),
                    side = 2,
                    line = 2,
                    cex = cex.lab
              )
              if(ribbon){
                mtext("Kernel Density", side = 4, line = 1, cex = cex.lab)
              }
              
            }
}
