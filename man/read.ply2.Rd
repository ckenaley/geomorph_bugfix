% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/3d.R
\name{read.ply2}
\alias{read.ply2}
\title{Read mesh data (vertices and faces) from .ply files}
\usage{
read.ply2(file, ShowSpecimen = FALSE)
}
\arguments{
\item{file}{character, a path to an ASCII .ply file}

\item{ShowSpecimen}{logical,should the ply file should be displayed}
}
\value{
A list with the following components:

#' \itemize{
\item "x","y", and "z", the x, y, and z coordinats of the mesh vertices
\item "mesh", \code{mesh3d} object whose name describes the number of vertices and triangles.
\item "zmean", the mean z position for each triangle (used for face coloring)
\item "facecolor", a face color according to \code{colour_ramp} palette "RdBu"
}
}
\description{
Read .ply files to be used for digitizing landmark coordinates.
}
\details{
Function reads three-dimensional surface data in the form of a single .ply file (Polygon File Format; ASCII format only, from 3D scanners or from .ply rendered from STL files produced by surface renderings of computed tomography data. The function opens the ply file and plots the mesh in a local  \code{shiny} app if \code{ShowSpecimen=TRUE}.
}
\examples{

#load a .ply file, taken from https://people.sc.fsu.edu/~jburkardt/data/ply/ply.html

ply.f<- system.file("extdata","hammerhead.ply",package = "geomorphcompanion")


spec <- read.ply2(file=ply.f,ShowSpecimen = FALSE)
}
\seealso{
\code{geomorph::read.ply}
}
