% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/roxycode.R
\name{dens.aspp}
\alias{dens.aspp}
\title{dens.aspp}
\usage{
dens.aspp(
  data,
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
  add_square_counts = TRUE
)
}
\arguments{
\item{data}{Vector of x and y coordinates of data point.}

\item{data1}{Vector of x and y coordinates of data point.}

\item{test_null}{the null model object to plot.}

\item{test_alt}{the alternative model object to plot.}

\item{method}{method employed for testing "friedman" or "wilcoxon".}

\item{nr_sq}{number of squares for the polygon horizontal axis.}

\item{z_lim}{plotting limits.}

\item{type_plot}{type of the comparison "subtraction_square_plot", "subtraction_dens_plot" and "test_plot".}

\item{title}{character indicate title of the plot.}

\item{window}{window of observation, an object of class "owin".}

\item{cex.axis}{specify the size of the tick label numbers/text with a numeric value of length 1.}

\item{cex.lab}{specify the size of the axis label text with a numeric value of length 1.}

\item{col}{The colors to be used for the background.}

\item{num_size}{number indicating the size of the labels.}

\item{num_size_sig}{number indicating the size of the labels.}

\item{ribbon}{logical flag indicating whether to display a ribbon showing the color map. Default is TRUE for new plots and FALSE for added plots.}

\item{contour}{create a contour plot, or add contour lines to an existing plot.}

\item{bias}{bias >1 puts more colors at high values.}

\item{add_star_sig}{logical flag indicating whether to add or not star in the square.}

\item{test_overall}{logical flag indicating whether to test or not.}

\item{add_square_counts}{logical flag indicating whether to add number of cells in the square}
}
\description{
This function for visualization and testing of spatial point patterns.
}
\author{
Viktorian Miok
}
