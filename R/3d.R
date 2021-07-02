
#' @title Read mesh data (vertices and faces) from .ply files
#'
#' @description Read .ply files to be used for digitizing landmark coordinates.
#'
#'
#' @param file character, a path to an ASCII .ply file
#' @param ShowSpecimen logical,should the ply file should be displayed
#'
#'
#' @details Function reads three-dimensional surface data in the form of a single .ply file (Polygon File Format; ASCII format only, from 3D scanners or from .ply rendered from STL files produced by surface renderings of computed tomography data. The function opens the ply file and plots the mesh in a local  \code{shiny} app if \code{ShowSpecimen=TRUE}.
#'
#' @return
#'
#' A list with the following components:
#'
#' #' \itemize{
#' \item "x","y", and "z", the x, y, and z coordinats of the mesh vertices
#' \item "mesh", \code{mesh3d} object whose name describes the number of vertices and triangles.
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
#' @import shiny tidyverse plotly utils
#' @examples
#'
#'\dontrun{
#' #load a .ply file, taken from https://people.sc.fsu.edu/~jburkardt/data/ply/ply.html
#'
#'ply.f<- system.file("extdata","hammerhead.ply",package = "geomorphcompanion")
#'
#'spec <- read.ply2(file=ply.f,ShowSpecimen = F)
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
    sliderInput("marksize", "Point size:",
                min = 1, max = 50,
                value = 10),
    plotlyOutput('myPlot'),
    verbatimTextOutput("info"),
    actionButton("save", "Save"),
    actionButton("del", "Delete Last"),
    tags$script(
      HTML(
        "$(function(){
      $(document).keyup(function(e) {
      if (e.which == 65) {
        $('#button').click()
      }
      });
      })"
      )
    ),
    actionButton("button", "Accept Point"),
    textOutput("text")
  )

  server <- function(input, output, session) {

    dt <- reactiveValues(df = tibble(NULL), df2 = tibble(NULL))



    output$myPlot = renderPlotly({


      plot_ly(type = 'scatter3d', mode = 'markers')%>%
        add_trace(
        x = spec$x,
        y = spec$y,
        z = spec$z,
        marker=list(
        alpha = 1,
        size = 10,
        color="black"),
        name="points"
      ) %>% add_trace(
        type = 'mesh3d',
        x = spec$x,
        y = spec$y,
        z = spec$z,
        i = spec$mesh$it[1,] - 1,
        j = spec$mesh$it[2,] - 1,
        k = spec$mesh$it[3,] - 1,
        facecolor = "gray20",
        opacity=0.3,
        inherit = FALSE,
        name="mesh"
      )%>%
        layout(showlegend = FALSE,
               updatemenus = list(
          list(type='buttons',
               direction = "right",
               xanchor = 'center',
               yanchor = "top",
               x = 0.5,
               y = 1.27,
               buttons = list(
                 list(method = "update",
                      args = list(list(visible=c(T, T))),
                      label = 'points+mesh'),
                 list(method = "update",
                      args = list(list(visible=c(F, T))),
                      label = 'points'),
                 list(method = "update",
                      args = list(list(visible=c(T,F))),
                      label = 'mesh')
               )))
        )
    })




    observeEvent(input$marksize, {
      plotlyProxy("myPlot") %>%
        plotlyProxyInvoke(
          "restyle",
          list(
            marker=list(size=input$marksize,color="black",name="points"),
            inherit=TRUE
            )


        )


      }
        )


    observeEvent(input$button, {
      d <-
        unlist(event_data("plotly_click", priority = "event"))
      d_old_new <- rbind(dt$df, d)
      dt$df <- d_old_new
      colnames(dt$df) <- c("pt", "curve", "x", "y", "z")

      dt$df <- dt$df#%>%filter(pt==0)

      dt$df2 <- dt$df %>% filter(pt == 1)

      if (last(dt$df$pt) == 1)
        plotlyProxy("myPlot") %>%
        plotlyProxyInvoke(
          "addTraces",
          list(
            x = c(dt$df[nrow(dt$df), 3], Inf),
            y = c(dt$df[nrow(dt$df), 4], Inf),
            z = c(dt$df[nrow(dt$df), 5], Inf),
            type = 'scatter3d',
            mode = 'markers',
            marker=list(color="blue",size=input$marksize+10),
            name = paste0("point ", nrow(dt$df)),
            inherit = FALSE
          )
        )



    })
    output$text <- renderText({
      if(nrow(dt$df)>0){
      paste0("N pts = ", nrow(dt$df%>% filter(pt == 1)))}else{"N pts = 0"}
    })


    observeEvent(input$del, {
      if (nrow(dt$df) > 0)
        plotlyProxy("myPlot") %>%
        plotlyProxyInvoke("deleteTraces", as.integer(nrow(dt$df2[dt$df2$pt ==1, ]) + 2))

      if (nrow(dt$df) != 0)
        dt$df <-  dt$df[-nrow(dt$df), ] %>% filter(pt == 1)
      if (nrow(dt$df) != 0)
        dt$df2 <-  dt$df[-nrow(dt$df2), ]
    })


    output$info <- renderPrint({
      if (nrow(dt$df) != 0)
        dt$df %>% filter(pt == 1)%>%select(x,y,z)
    })


    observeEvent(input$save, {
      write.csv(dt$df%>% filter(pt == 1)%>%select(x,y,z),
                paste0(out.dir, "/", spec$spec.name, ".csv"),
                row.names = FALSE)
    })

  }

  shinyApp(ui, server)
}
