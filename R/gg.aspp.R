###############################################################################
#
#       Plots for initial astrocytes spatial point pattern visualization
#
###############################################################################
gg.aspp <- function(data,
                    type_plot = 'scatter_plot',
                    title = " ",
                    legend.text = 15,
                    legend.title = 15,
                    axis.text = 15,
                    axis.title = 15,
                    plot.title = 15,
                    xlim = c(0, 500), 
                    ylim = c(0, 500)){
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
          if(type_plot == 'scatter_plot'){
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
                    plot.title = element_text(size = plot.title))
            
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
}
