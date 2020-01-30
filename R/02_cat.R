# This is an example of how to implement the Newton-Raphson method on complex numbers
# using R's built-in 'complex' number definition, which is written so that you can use +,-,*,/,^ in the usual way

# First define a function to work with - it will return two values.
# The first will be the value of the function at z (i.e., f(z)), the second will be the derivate of f
# at z (i.e., f'(z))
F7<-function(z){ # this has three roots 1,-1/2 + sqrt(3)/2i and -1/2 - sqrt(3)/2i
  # a complex function z^3-1
  return(c(z^3 - 1, 3 * z^2)) # note that arithmetic ops on complex numbers work just like they should
}


# Next we define a global variable to control whether we record roots or the number of iterations taken to find them when drawing our picture
# This is an inelegant way of doing it. It should really be part of the argument to the relevent functions.
bRootOrIterations <- 0   # Set <-1 to record which root is found, or <- 0 to record number of iterations needed



# Here's the function that performs Newton-Raphson
TwoDNewtonRaphson <- function(func, StartingValue, Tolerance, MaxNumberOfIterations) {
  i <- 0  # something to count the iterations
  NewZ <- StartingValue  # start the algorithm at the complex number 'StartingValue'
  Deviation = abs(func(StartingValue)[1])   # Work out how far away from (0,0) func(NewZ) is.

  #Set up a while loop until we hit the required target accuracy or the max. number of steps
  while ((i < MaxNumberOfIterations) && (Deviation > Tolerance)) {
    # Find the next Z-value using Newton-Raphson's formula
    Z <- func(NewZ)   # Remember, this is is a vector of two elements. Z[1] is the is the value of the function; Z[2] is its derivative
    if ((Z[1] == "NaN") || (Z[2] == "NaN")) {
      cat("Function or derivative not defined error.")
      cat("\n", NewZ, Z)
      break
    }

    # So we need to calculate the next value of Z using this formula Z(n+1) <- Z(n)-f(Z(n))/f'(z(n))
    NewZ <- NewZ - Z[1] / Z[2]

    # calculate how far f(z) is from 0
    NewVal <- func(NewZ)
    Deviation <- abs(NewVal[1])
    i <- i + 1
    #cat(paste("\nIteration ",i,":   Z=",NewZ,"  Devn=",Deviation))
  }

  # output the result
  if (Deviation > Tolerance) {
    cat(paste("\nConvergence failure. Deviation:", Deviation, "after ", i, 	"iterations"))
  }

  # what the function returns depends upon whether you are counting how many iterations it takes
  # to converge or checking which root it converged to...
  if (bRootOrIterations == 1) {
    return(NewZ)
  } else {
    return(c(i, i))
  }
}

# A function to check whether two points are close together
CloseTo <- function(x, y) {
  # returns 1 if x is close to y
  if (abs(x - y) < 0.1) {
    return(1)
  } else {
    return(0)
  }
}

# And now here's the function that will draw a pretty picture
RootPlotter <- function(Funcn, xmin, xmax, xsteps, ymin, ymax, ysteps, PtSize) {
  # First define a grid of x and y coordinates over which to run Newton-Raphson.
  # When we run ut for the point (x,y) it will start with the complex number x+iy
  x <- seq(xmin, xmax, length.out = xsteps)
  y <- seq(ymin, ymax, length.out = ysteps)
  # define a vector that will store the information telling us what color each point on the grid shoudl be
  ThingsToPlot <- c(-9, -9, 'white', -9, -9)
  pb <- txtProgressBar(max = xsteps, style = 3)
  for (i in 1:xsteps){
    setTxtProgressBar(pb, i)
    for (j in 1:ysteps){
      ThisZ <- complex(1, x[i], y[j])
      color <- 'black' # a default color (used if no root is found, or the number of iterations is too large)

      # The following is written to look for the roots for z^3-1. You will need to modify it if you want to use other functions and work out which root was found
      # For z^3-1 the three roots are 1,  -1/2 + sqrt(3)/2i and -1/2 - sqrt(3)/2i
      # Find the root for the current start point
      Root <- TwoDNewtonRaphson(Funcn, ThisZ, 1e-1, 100)

      # record the color needed for this point on the grid
      if (bRootOrIterations == 0){ # set <-1 to record roots, or <- 0 to record number of iterations needed
        ThisColor <- 261 + 5 * Root[1]
        color <- ThisColor
      } else {
        if (CloseTo(Root, 1+0i)){
          color <- 'blue'
        }
        if (CloseTo(Root, -0.5+0.8660254i)){
          color <- 'red'
        }
        if (CloseTo(Root, -0.5-0.8660254i)){
          color <- 'yellow'
        }
      }
      # record all the information in the array ThingsToPlot so that we can plot the information later
      ThingsToPlot <- rbind(ThingsToPlot, c(x[i], y[j], color, Root[1], Root[2]))
    }
  }
  # And now we have everything, so let's draw the picture.
  plot(ThingsToPlot[, 1:2], col = ThingsToPlot[, 3],
       xlim = c(xmin,xmax), ylim = c(ymin, ymax),
       pch = 16, cex = PtSize * 3)
  # The graphics parameter pch controls what shape is used to plot each point
  # The graphics parameter cex is used to control the size of each point
  # Change these to improve the quality of your pictures
  return(ThingsToPlot)
}



# The following are a bunch of examples I used to create some of the figures shown in class
# Note that I used a 500x500 grid to get nice pictures, so this will take a while to run
# You shoudl use a smaller grid while you are preactising (say 100,100), but then use a bigger grid for you final pictures.
RunTests <- function() {
  # now lets draw some plots
  start_time <- Sys.time()
  A <- RootPlotter(F7, -1, 1, 100, -1, 1, 100, 0.2)
  end_time <- Sys.time()
  cat("\n t=", end_time - start_time)

  #  png("FigFrac2c.png")
  #  A<-RootPlotter(F7,-0.8,0,500,-0.25,0.25,500,0.2)
  #  dev.off()

  #  png("FigFrac3c.png")
  #  A<-RootPlotter(F7,-0.8,-0.5,500,-0.25,0,500,0.2)
  #  dev.off()

  #  png("FigFrac5c.png")
  #  A<-RootPlotter(F7,-0.72,-0.65,500,-0.24,-0.19,500,0.2)
  #  dev.off()

  #  png("FigFrac6c.png")
  #  A<-RootPlotter(F7,-0.71,-0.7,500,-0.21,-0.2,500,0.2)
  #  dev.off()



}


RunTests()
