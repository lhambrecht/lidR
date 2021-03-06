% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/algorithm-dec.R
\name{highest}
\alias{highest}
\title{Point Cloud Decimation Algorithm}
\usage{
highest(res = 1)
}
\arguments{
\item{res}{numeric. The resolution of the grid used to filter the point cloud}
}
\description{
This function is made to be used in \link{decimate_points}. It implements an algorithm that
creates a grid with a given resolution and filters the point cloud by selecting the highest point
within each cell.
}
\examples{
LASfile <- system.file("extdata", "Megaplot.laz", package="lidR")
las = readLAS(LASfile, select = "xyz")

# Select the highest point within each cell of an overlayed grid
thinned = decimate_points(las, highest(4))
plot(thinned)
}
\seealso{
Other point cloud decimation algorithms: 
\code{\link{homogenize}()},
\code{\link{random}()}
}
\concept{point cloud decimation algorithms}
