
#' @title Read mesh data (vertices and faces) from .ply files
#'
#' @description Read .ply files to be used for digitizing landmark coordinates.
#'
#'
#' @param file character, a path to an ASCII .ply file
#' @param ShowSpecimen logical,should the ply file should be displayed
#'
#' @details Function reads three-dimensional surface data in the form of a single .ply file (Polygon File Format; ASCII format only, from 3D scanners or from .ply rendered from STL files produced by surface renderings of computed tomography data. The function opens the ply file and plots the mesh in a local  \code{shiny} app if \code{ShowSpecimen=TRUE}.
#'
#' @return
#'
#' A list with the following components:
#'
#' #' \itemize{
#' \item "x","y", and "z", the x, y, and z coordinats of the mesh vertices
#' \item "meas", \code{mesh3d} object whose name describes the number of vertices and triangles.
#' \item "zmean", the mean z position for each triangle (used for face coloring)
#' \item "facecolor", a face color according to \code{colour_ramp} palette "RdBu"
#' }
#'
#'
#' @seealso \code{geomorph::read.ply}
#'
#' @export
#' @import shiny scales plotly utils
#'
#' @examples
#'
#' #load a .ply file, taken from https://people.sc.fsu.edu/~jburkardt/data/ply/ply.html
#'
#'ply.f<- system.file("extdata","hammerhead.ply",package = "geomorphcompanion")
#'
#'
#'spec <- read.ply2(file=ply.f,ShowSpecimen = FALSE)

read.ply2 <- function(file,ShowSpecimen = FALSE) {
  mesh <- geomorph::read.ply(file, ShowSpecimen = FALSE)
  # see getS3method("shade3d", "mesh3d") for details on how to plot
  # plot point cloud
  x <- mesh$vb["xpts", ]
  y <- mesh$vb["ypts", ]
  z <- mesh$vb["zpts", ]
  m <- matrix(c(x, y, z), ncol = 3, dimnames = list(NULL, c("x", "y", "z")))
  # now figure out the colormap
  zmean <- apply(t(mesh$it), MARGIN = 1, function(row) {
    mean(m[row, 3])
  })
  facecolor = colour_ramp(brewer_pal(palette = "RdBu")(9))(rescale(x = zmean))

  if (ShowSpecimen) {
    p <-  plot_ly(
      x = x,
      y = y,
      z = z,
      i = mesh$it[1, ] - 1,
      j = mesh$it[2, ] - 1,
      k = mesh$it[3, ] - 1,
      facecolor = facecolor,
      type = "mesh3d",
      alpha = 0.01,
    )

    print(p)
  }
  return(list(
    x = x,
    y = y,
    z = z,
    mesh = mesh,
    zmean = zmean,
    facecolor = facecolor,
    spec.name=basename(file)
  ))
}



#' @title Digitize 3D landmarks on mesh3d object
#'
#' @description A \code{shiny}-based interactive function to digitize three-dimensional (3D) landmarks. Input for the function is output of vertex coordinates defining a mesh3d object as obtained from \code{read.ply2}.
#'
#' @param spec a list, produced from \code{read.ply2}
#' @param out.dir character, the file path describing where to save results
#'
#' @details Function for digitizing fixed three-dimensional landmarks. Produces a \code{shiny} app interactive session. Permits panning, zooming, and rotation. Results are saved as a .csv file once the user clicks on the "Save" button.
#'
#'
#' @return
#'
#' A csv file with x,y,z coordinates named according to the \code{spec.name} output of \code{read.ply2}
#'
#'
#' @seealso \code{geomorph::digitize.fixed}
#'
#' @export
#'
#' @examples
#'
#'\dontrun{
#' #load a .ply file, taken from https://people.sc.fsu.edu/~jburkardt/data/ply/ply.html
#'
#'ply.f<- system.file("extdata","hammerhead.ply",package = "geomorphcompanion")
#'
#'spec <- read.ply2(ply=ply.f,ShowSpecimen = F)
#'
#'digit.fixed2(spec=spec,out.dir = tempdir())
#'
#'#commence digitizing
#'
#'f <- list.files(tempdir(),pattern=".csv",full.names=TRUE)
#' read_csv(f)
#'}

digit.fixed2 <- function(spec = spec,out.dir=NULL) {
  ui <- fluidPage(
    plotlyOutput('myPlot'),
    tableOutput("table"),
    verbatimTextOutput("info"),
    actionButton("save", "Save")
  )

  server <- function(input, output, session) {
    # keep track of which cars have been hovered on
    dt <- reactiveVal()

    # On hover, the key field of the event data contains the car name
    # Add that name to the set of all "selected" cars


    observeEvent(event_data("plotly_click"), {
      d <- unlist(event_data("plotly_click")[, c(3:5)])
      d_old_new <- rbind(dt(), d)
      dt(unique(d_old_new))
    })


    # clear the set of data when a double-click occurs
    observeEvent(event_data("plotly_doubleclick"), {
      dt(NULL)
    })


    # if the point is selected, paint it red
    #cols <- ifelse(row.names(mtcars) %in% data(), "red", "black")

    output$myPlot = renderPlotly({
      plot_ly(
        x = spec$x,
        y = spec$y,
        z = spec$z,
        i = spec$mesh$it[1, ] - 1,
        j = spec$mesh$it[2, ] - 1,
        k = spec$mesh$it[3, ] - 1,
        facecolor = spec$facecolor,
        type = "mesh3d",
        alpha = 0.01,
      )
    })


    # output$table <- renderTable({data()
    # })

    output$info <- renderPrint({
      dt()
    })

    observeEvent(input$save, {
      write.csv(dt(), paste0(out.dir,"/",spec$spec.name,".csv"), row.names = FALSE)
    })

  }

  shinyApp(ui, server)
}


