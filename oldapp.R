rm(list=ls())

library(shiny)
library(reshape)
library(dplyr)
library(RJSONIO)

#source functions
source("openposeSupportFuncs.R")

#Pull in JSON files and create list of list of matrices for each seg
dancer_segments <- list.files("data/segmentedFramesJSON/")
segfiles <- paste0("data/segmentedFramesJSON/", dancer_segments)
cocomatrices<- lapply(segfiles, function(x) getFramesFromJSON(x))
cocomatrices <- lapply(1:length(cocomatrices), function(x) cleanFrames(cocomatrices[[x]], numPeople = 1))


#Apply segment name to each item in the list
names(cocomatrices) <- dancer_segments

#Create data frame with all the dancerseg angles
#segAnglesDFs <- lapply(1:16, function(x) computeSegmentAngles(cocomatrices[[x]]))
segAnglesDFs <- lapply(1:length(cocomatrices), function(x) computeSegmentAngles(cocomatrices[[x]]))
#Add dancerseg column
segAnglesDFs <- mapply(cbind, segAnglesDFs, Dancer_Seg = dancer_segments, SIMPLIFY = FALSE)
#Add column to indicate frame
segAnglesDFs <- lapply(segAnglesDFs, function(x) cbind(x, Frame = 1:nrow(x)))
#Combine everything into one data frame
allDancerDF <- do.call('rbind', segAnglesDFs)

#Create reference for angle body parts
angle_body_parts <- names(allDancerDF)[1:4] <-  c('RShoulder', 'RElbow', 'LShoulder', 'LElbow')

#Create output types
output_type <- c("skeleton", "angles", "video")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Exploring Expressivity Through Angles"),
  fluidRow(
    column(3,
    wellPanel(
    selectInput('dancerseg','Clip', dancer_segments, selected ='select a segment'),
      #To plot a skeleton--need to make variable the max depending of # of frames
    sliderInput("animation", "Frame",
    min = 1, max = 60,
    value = 1, step = 1,
    animate =
      animationOptions(interval = 200, loop = FALSE)),
    sliderInput('framerange', 'Frame Range', min = 1, max = 60, step= 1, value = c(1,60)),
    #for selecting the body part
    selectInput('angle', 'Body Angle', angle_body_parts, selected = 'select angle')
           )
    ),
    column(2, imageOutput("image")),
    column(3, offset = 0, plotOutput("plotSkeleton")),
    column(3, offset = 0, plotOutput("plotAngles"))
    )
  # ,
  # fluidRow(
  #   column(3, offset=8,span(textOutput("angleMeasure"), style="color:red"))
  # )
)



server <- function(input, output, session) {
  
  observe({
    maxFrame <- length(cocomatrices[[input$dancerseg]]) 
    updateSliderInput(session, "framerange", max = maxFrame)
    updateSliderInput(session, "animation", max = maxFrame)
  })
  
#RShould:3, 4, Lshould: 6, 7
  output$plotSkeleton <- renderPlot({
    drawStickFig(cocomatrices[[input$dancerseg]][[input$animation]],
                 yrange=c(1800,100), xrange=c(100,880))
      coords <-cocomatrices[[input$dancerseg]][[input$animation]][[1]][,1:2]
      rownames(coords) <- coco_body_parts[1:18]
      points(coords[rownames(coords)==input$angle,1],coords[rownames(coords)==input$angle,2], 
             pch=19, cex = 3, col=rgb(0,1,0,0.5))
  }, height =400, width=250)
  
 # ifelse(input$angle="Rshoulder", plot(cocomatrices[[input$dancerseg]][[3]]),
      #   ifelse(input$angle="Lshoulder", plot(cocomatrices[[input$dancerseg]][[6]]), plot(cocomatrices[[input$dancerseg]][[4]])))
  
  output$angleMeasure <- renderText({
    # debug <- paste("angle",input$angle, "frame", input$animation, "seg", input$dancerseg) 
    angle <- filter(allDancerDF, Dancer_Seg==input$dancerseg & Frame == input$animation) %>%
                    pull(input$angle) %>% round()
    captiontext <- paste("The",input$angle, "angle is", angle, "degrees")
    captiontext
    # debug
    
  })
  
  #Plot Angles: input dancer segment framerange, and body part
  output$plotAngles <- renderPlot({
    plotdata <- filter(allDancerDF, Dancer_Seg==input$dancerseg) 
  par(mar=c(4,4,2,0))
    plot(plotdata[,input$angle], type="n", lwd=2, 
         main=paste(input$dancerseg,input$angle),
         xlab = "Clip Frame", ylab = "Angle", ylim=c(-10,190), yaxt = "n",
         xlim = input$framerange)
    points(plotdata[,input$angle])
    lines(plotdata[,input$angle], lwd=2)
    axis(2, at=seq(0,180,45),labels=seq(0,180,45), las=2)
    abline(180,0, col="blue")
    abline(90,0, lty=2, col="blue")
    abline(0,0, col="blue")
    abline(v=input$animation, col="red", lty=2)
    # title(ylab="Angle", line=2, cex.lab=1.2)
    
    angle <- filter(allDancerDF, Dancer_Seg==input$dancerseg & Frame == input$animation) %>%
      pull(input$angle) %>% round()
    captiontext <- paste(angle, "deg")
    text(input$animation+3, angle+10, captiontext, col="red")
    #rect(xleft= l ,xright= r, ybottom = -100, ytop= 500, density= 100, col=rgb(0,0,0,0.1))
  }, height =320, width=400)
  
  output$image <- renderImage({
     imagename <- paste0("data/segmentedOpenposeFrames/",input$dancerseg,
           "/",input$dancerseg,sprintf("frame%03d.jpg", input$animation))
    list(src = imagename,
              contentType = 'image/jpeg',
              height = 320,
              width = 180,
              alt = imagename)
  }, deleteFile = FALSE)
}  


# Run the application 
shinyApp(ui = ui, server = server)
