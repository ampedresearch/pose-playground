# Raw openpose data come in the form of one .json file for each frame of video
# each .json file then contains a JSON object with a COCO model or BODY25 model for each person in the frame

require("RJSONIO")

frameRate <- 30
bonesCOCO <- read.csv("bonesCOCO.csv")
bonesBODY25 <- read.csv("bonesBODY25.csv")

getJSONdirs <- function(dirName) {
  list.files(dirName) %>% 
    str_split_fixed(., "_|[.]", 2) %>% 
    data.frame(stringsAsFactors = FALSE) %>% 
    select(c(1)) %>% unique() %>% pull
}

getFramesFromJSON <- function(output_dir) {
  # frames will be an multi-dimensional lsit array of framenums x people x [COCO model]
  frames <- list()
  framenum <- 1
  for (jfile in grep(".json",list.files(path=output_dir), value=T)){ #need to make sure to ignore non-JSON files
    json_file <- fromJSON(file.path(output_dir,jfile)) #reading each json file, convert to R list
    # it can also happen that there are no people in the frame
    if (length(json_file$people)) {
      # We will append the frames list with a matrix of pose keypoints
      # First two columns are planar coordinates, thrid is confidence. 
        pose_keypoints2d <- lapply(json_file$people, function(a){
          matrix(a$pose_keypoints_2d, ncol=3, byrow = TRUE) # DON'T throw away third column
        })
        frames[[framenum]] <- pose_keypoints2d # each frame gets a list of matrices
      framenum <- framenum+1
    }
  }
  return(frames)
}



angle <- function(x,y){
  dot.prod <- x%*%y 
  norm.x <- norm(x,type="2")
  norm.y <- norm(y,type="2")
  theta <- acos(dot.prod / (norm.x * norm.y))
  as.numeric(theta)
}


drawFrame <- function(framedat,  plotpeople=Inf, myColors=NULL,
                      crop=NULL, resolution=c(1920,1080),
                    portrait=F, invert=F, drawchull=FALSE) {
  par(mar=c(5.1,4.1,1,1))
  if (!is.null(crop)) {
    plot(1, type="n", xlim=c(crop[1],crop[3]), ylim=c(crop[4],crop[2]), 
         asp = 1, xlab='',ylab='')
  } else {
    plot(1, type="n", xlim=c(0,resolution[1]), ylim=c(resolution[2],0), 
         asp = 1, xlab='',ylab='')
  }
  if (plotpeople == Inf) {
    np <- length(framedat) # frame is a list of matrices, one for each person
  } else {
    np <- plotpeople
  }
  for (i in 1:np){
    drawKeypoints(framedat[[i]])
    if (drawchull) {
      drawChull(framedat[[i]])
    }
  }
}


drawChull <- function(posematrix, portrait=F, color=rgb(0,1,0,0.3)) {
  library(grDevices)
  coorddat <- posematrix[,1:2]
  confdat <- posematrix[,3]
  # flip x.y if necessary
  if (portrait) {coorddat <- coorddat[,c(2,1)]}
  
  # chull can't handle missing/NA points, so instead we will mean impute them
  # this way they won't be part of the hull
  coorddat[which(confdat < 0.4),] <- c(mean(coorddat[,1]),mean(coorddat[,2]))
  chpoints <- chull(coorddat)
  
  # make the hull circular
  chpoints <- c(chpoints, chpoints[1])
  
  # and connect the dots
  lines(coorddat[chpoints,1], coorddat[chpoints,2], col=color, lwd=2)
  # for (i in 1:length(chpoints)) {
  #   if (i == length(chpoints)) {
  #     lines(coorddat[chpoints[c(i,1)],1], coorddat[chpoints[c(i,1)],2], col=color)
  #   } else {
  #     lines(coorddat[chpoints[c(i,i+1)],1], coorddat[chpoints[c(i,i+1)],2], col=color)
  #     
  #   }
  # }
  
}


drawKeypoints <- function(posematrix, portrait=F, invert=F, color="purple") {
  
  coorddat <- posematrix[,1:2]
  confdat <- posematrix[,3]
  # flip x.y if necessary
  if (portrait) {coorddat <- coorddat[,c(2,1)]}
  # remove low-confidence points
  coorddat[which(confdat < 0.4),] <- c(NA,NA)
  
## plot points  and bones COCO
  points(coorddat[1:15,], col=color)
  for (i in 1:nrow(bonesCOCO)) {
    bone1 <- bonesCOCO[i,1]
    bone2 <- bonesCOCO[i,2]
    xlist <- c(coorddat[bone1,1],coorddat[bone2,1])
    ylist <- c(coorddat[bone1,2],coorddat[bone2,2])
    lines(xlist, ylist, col=color)
  }
  
  if (nrow(coorddat) == 25) { # add feet for BODY25 models
    points(coorddat[20:25,], col=color)
    for (i in 1:nrow(bonesBODY25)) {
      bone1 <- bonesBODY25[i,1]
      bone2 <- bonesBODY25[i,2]
      xlist <- c(coorddat[bone1,1],coorddat[bone2,1])
      ylist <- c(coorddat[bone1,2],coorddat[bone2,2])
      lines(xlist, ylist, col=color)
    }
  }
  
}




# helper function
cocoInfo <- function(cocoMatrix) {
  # earlier version of this functions returned
  # the number of non-zero entries in XY
  #      length(which(cocoMatrix != 0))
  # But the current version returns average confidence
  mean(cocoMatrix[,3])
}

cleanFrame <- function(frame, numPeople, sortcoord=1) {
  # This function will take a frame and look for at MOST numPeople.
  # If there are > numPeople in the frame, it will throw
  # away the least informative ones.
  # Finally, it will reorder the people in each frame
  # starting from left to right (if x-coordinate is used) 
  infoOrder <- order(unlist(lapply(frame, cocoInfo)),
                 decreasing=TRUE)
  keepPeople <- infoOrder[1:numPeople]
  newFrame <- frame[keepPeople]
  # now sort from left to right
  if (length(newFrame) > 1) {
    posOrder <- order(unlist(
                      lapply(newFrame, function(a){
                        mean(a[,sortcoord], na.rm=T)
                        })
                      ))
    newFrame <- newFrame[posOrder]
  }
  return(newFrame)
}

cleanFrames <- function(frameset, numPeople, sortcoord=1) {
  lapply(frameset, 
         FUN=function(a) {
           cleanFrame(a, numPeople = numPeople, sortcoord=1)
         })
}

exportPNGs <- function(filename, frameset, width=480, height=640, 
                       drawOpts=list(portrait=F, 
                                     yrange=c(1600,300), xrange=c(100,880),
                                     color=3)) {
  
  for (i in 1:length(frameset)) {
    png(filename = paste0(filename,
                          sprintf("%04d",i),".png"), 
        width = width, height = height)
    drawStickFig(frameset[[i]], framenum=i,
                 portrait=drawOpts$portrait, xrange=drawOpts$xrange, yrange=drawOpts$yrange,
                 color=drawOpts$color
    )
    dev.off()
  }
  
}
