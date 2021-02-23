#########################################################################################
#
#       Plots for visualization and testing of spatial point patterns - all combined
#
#########################################################################################

spp.viz <- function(data, 
                    data1 = NULL,
                    test_null = NULL,
                    test_alt = NULL,
                    method = "friedman",
                    nr_sq = 10,
                    z_lim = NULL,
                    type_plot = "statspat_plot", 
                    title = " ", 
                    window, 
                    legend.text = 15,
                    legend.title = 15,
                    axis.text = 15,
                    axis.title = 15,
                    plot.title = 15,
                    cex.axis = 1,
                    cex.lab = 1,
                    col = c("blue", "white", "red"),
                    xlim, 
                    ylim,
                    num_size = 1.2,
                    num_size_sig = 1.5,
                    ribbon = FALSE,
                    contour = TRUE,
                    bias = NULL,
                    add_star_sig = FALSE,
                    test_overall = TRUE){
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
          
          if(type_plot=="hexagonal_heatmap"){
            # Hexagonal heatmap of 2d bin counts
            p <- ggplot(data, aes(x = X, y = Y) ) + 
              geom_hex(bins = 70) + 
              scale_fill_continuous(type = "viridis") + 
              theme_bw() +
              ggtitle(title) + 
              theme(legend.text = element_text(size = legend.text),
                    legend.title = element_text(size = legend.title),
                    axis.text = element_text(size = axis.text),
                    axis.title = element_text(size = axis.title),
                    plot.title = element_text(size = plot.title)) + 
              xlim(xlim) +
              ylim(ylim)
            return(p)
          }
          if(type_plot=='scatter_plot'){
            # scatter plot
            p <- ggplot(data, aes(x = X, y = Y)) + 
              stat_density2d(geom = "tile",
                             aes(fill = ..density..),
                             contour = FALSE) + 
              geom_point(colour = "white") +
              ggtitle(title) +
              theme(legend.text = element_text(size = legend.text),
                    legend.title = element_text(size = legend.title),
                    axis.text = element_text(size = axis.text),
                    axis.title = element_text(size = axis.title),
                    plot.title = element_text(size = plot.title)) + 
              xlim(xlim) +
              ylim(ylim)
            return(p)
          }
          if(type_plot == "polygon"){
            p <- ggplot(data, aes(x = X, y = Y)) + 
              geom_point() +
              stat_density_2d(aes(fill = ..level..), 
                              alpha = 0.1, 
                              geom = "polygon") +
              theme(legend.text = element_text(size = legend.text),
                    legend.title = element_text(size = legend.title),
                    axis.text = element_text(size = axis.text),
                    axis.title = element_text(size = axis.title),
                    plot.title = element_text(size = plot.title)) +
              xlim(xlim) +
              ylim(ylim)
            
            dat_lims <- lapply(data, function(v) c(min(v), max(v)))
            plot_lims <- ggplot_build(p)$panel$ranges[[1]][c("x.range",
                                                             "y.range")]
            
            p + scale_x_continuous(limits = dat_lims$x * 1.1) + 
              scale_y_continuous(limits = dat_lims$y * 1.1) +
              coord_cartesian(xlim = plot_lims$x.range,
                              ylim = plot_lims$y.range)
            return(p)
          }
          if(type_plot == "density_contour"){
            #density plot with contours
            p <- ggplot(data, aes(x = X, y = Y)) +
              stat_density_2d(aes(fill = ..density..), 
                              geom = "raster", 
                              contour = FALSE) +
              scale_fill_distiller(palette = 4, 
                                   direction = 1) + 
              geom_density_2d(color = "white") + 
              ggtitle(title) + 
              theme(legend.text = element_text(size = legend.text),
                    legend.title = element_text(size = legend.title),
                    axis.text = element_text(size = axis.text),
                    axis.title = element_text(size = axis.title),
                    plot.title = element_text(size = plot.title)) +
              xlim(xlim) +
              ylim(ylim)
            return(p)
          } 
          if(type_plot == "statspat_square_plot"){
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
                plot(intensity(Q1-Q2, image = TRUE), 
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
              } else{ plot(intensity(Q1-Q2, image = TRUE), 
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
          if(type_plot == "statspat_plot_subtract"){
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
          if(type_plot == "statspat_plot"){
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
              p <- plot(Q1,
                        add = TRUE,
                        col = "white", 
                        cex = num_size,
                        cex.axis = cex.axis,
                        cex.lab = cex.lab
              )
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
          if(type_plot == "3D_plot"){
            k1 <- density(ppp(data$X,
                              data$Y,
                              window = window),
                          sigma = 15
            ) 
            persp(k1, 
                  col = "lightblue",
                  theta = 40,
                  phi = 35,
                  main = title
            )
          }
          if(type_plot == "3D_plot_interactive"){
            # Compute kde2d
            kd <- with(data, 
                       MASS::kde2d(x = X, 
                                   y = Y,
                                   n = 50)
            )
            # Plot with plotly
            p <- plot_ly(x = kd$x, 
                         y = kd$y,
                         z = kd$z) %>% add_surface()
          }
}