# code to pause the image to allow for ease in visualization
pauseit <- function(x)
{
  p1 <- proc.time()
  Sys.sleep(x)
  proc.time() - p1 # The cpu usage should be negligible
}


library(fields)


N=12 # side length of environment
I0 = 20 # initial no. infected
envMat = matrix(0,nrow=N,ncol=N)
envMat[sample(1:length(envMat),I0,replace=F)] = 1

## optimal solution
# diag(envMat) = 1

deltaI = TRUE # are there still infections?
counter = 1

colorTable <- designer.colors(3, c( "navyblue","yellow", "black"), 
                             x = c(0, 40, 140) / 140)

graphCount = 0
# do we want to save the graph?
graphBool = F
# vector of prop. infected
infectedVec = c(I0/(N^2))

while (deltaI) {
  
  infectedVec = c(infectedVec, sum(envMat == 1)/(N^2))
  
  envMatPlus = matrix(2,nrow=N+2,ncol=N+2)
  envMatPlus[2:(N+1),2:(N+1)] = envMat
  envMatPlus[1,1] = 0
  
  if (graphBool) {
    # uncomment parts to save image() output as png
    # png(paste(graphCount,".png"))
    image(envMatPlus,col=colorTable,asp=1,axes=F)
    # dev.off()
    graphCount = graphCount + 1
  } else {
    image(envMatPlus,col=colorTable,asp=1,axes=F)
    pauseit(0.25)
  }
  
  
  # SaR vector
  susVec = which(envMat == 0)
  infVec = which(envMat == 1)
  # find neighbors of infected who can get infected
  # (up, left, down, right)
  infVecLeft = infVec[(infVec - 1) %% 12 > 0]
  infVecRight = infVec[(infVec + 1) %% 12 != 1]
  
  neighborVec = infVecLeft - 1
  neighborVec = c(neighborVec, infVecRight + 1)
  neighborVec  = c(neighborVec, infVec + N)
  neighborVec  = c(neighborVec, infVec - N)
  
  neighborVec = neighborVec[neighborVec >= 1]
  neighborVec = neighborVec[neighborVec <= (N^2)]
  
  neighborVec = neighborVec[! neighborVec %in% infVec]
  
  tmp.df = data.frame(table(neighborVec))
  neighborVec = as.numeric(as.character(tmp.df$neighborVec))
  
  newInfVec = neighborVec[which(tmp.df$Freq > 1)]
  if (length(newInfVec) == 0) {
    deltaI = FALSE
  }
  
  # set new infections
  envMat[newInfVec] = 1
  
  counter = counter + 1
  print(counter)
}

if (sum(envMat) == length(envMat)) {
  print("Complete infection accomplished.")
}

## stuff for making an animation out of the saved images above
# system('"C:\\Program Files\\ImageMagick-7.0.8-Q16\\convert.exe" -delay 20 "%d .png[0-11]" animation.gif')
## replace 23 below with last frame of gif
# system('"C:\\Program Files\\ImageMagick-7.0.8-Q16\\convert.exe" animation.gif ( -clone 11 -set delay 150 ) -swap 11,-1 +delete animation.gif')


