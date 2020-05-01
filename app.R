rm(list=ls())

library(shiny)
library(reshape)
library(dplyr)
library(stringr)
library(RJSONIO)

#source functions
source("openposeSupportFuncs.R")

#Pull in JSON files and create list of list of matrices for each seg
posegroups <- c(list.files("poses"))
posesInGroup <- getJSONdirs(posegroups[1])
# segfiles <- paste0("data/segmentedFramesJSON/", dancer_segments)
# cocomatrices<- lapply(segfiles, function(x) getFramesFromJSON(x))
# cocomatrices <- lapply(1:length(cocomatrices), function(x) cleanFrames(cocomatrices[[x]], numPeople = 1))


# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Exploring Pose Quantities"),
  fluidRow(
    column(5, offset=0, plotOutput("image")),
    column(7, offset=0, plotOutput("plotSkeleton"))
  ),
  fluidRow(
    column(3,
           wellPanel(
             selectInput('posegroup','Pose Group', posegroups),
             selectInput('pose','Pose', posesInGroup))
    ),
    column(4,
           wellPanel(
             sliderInput("animation", "Frame",
                         min = 1, max = 60,
                         value = 1, step = 1,
                         animate =
                           animationOptions(interval = 80, loop = FALSE)),
             checkboxInput("chull", "Show hull", value = FALSE)
           )
    ),
    column(2,
           wellPanel(
             numericInput("cropxmin", "crop xmin:", 200, min = 0, max = 1920, step = 50),
             numericInput("cropxmax", "crop xmax:", 1700, min = 0, max = 1920, step = 50)
           )
    )      ,
    column(2,
           wellPanel(
             numericInput("cropymin", "crop ymin:", 200, min = 0, max = 1080, step = 50),
             numericInput("cropymax", "crop ymax:", 1000, min = 0, max = 1080, step = 50)
           )
    )      
  ), # row 
)


server <- function(input, output, session) {
  
  values <- reactiveValues(jsondir="", posematrices=list())
  
  observe({
    posesInGroup <- getJSONdirs(paste0("poses/",input$posegroup))
    updateSelectInput(session, 'pose','Pose', posesInGroup)
  })
  

  observe({
    values$jsondir <- paste0("poses/",input$posegroup, "/", input$pose, "_json/")
    values$posematrices <- getFramesFromJSON(values$jsondir)
    numFrames <- length(values$posematrices)
    updateSliderInput(session, "framerange", max = numFrames)
    updateSliderInput(session, "animation", max = numFrames)
  })
  
  output$plotSkeleton <- renderPlot({
    if (jsondir != "") {
      croprange <- c(input$cropxmin, input$cropymin, input$cropxmax, input$cropymax)
      # croprange <- c(input$cropx[1], input$cropy[1], input$cropx[2], input$cropy[2])
      drawFrame(values$posematrices[[input$animation]], crop=croprange, drawchull = input$chull)
    }
    }, width=400)
  
  
  output$image <- renderImage({
    # imagename <- paste0("poses/", input$posegroup, "/", input$pose, ".gif")
    imagename <- paste0("poses/", input$posegroup, "/", input$pose, "_imageframes/",
                        sprintf("frame-%04d.jpg", input$animation))
    list(src = imagename,
              contentType = 'image/jpg',
              width = 500,
              alt = imagename)
  }, deleteFile = FALSE)
}  


# Run the application 
shinyApp(ui = ui, server = server)
