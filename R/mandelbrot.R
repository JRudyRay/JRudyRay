################################################################################

library(mandelbrot)

x <- mandelbrot(xlim = c(-0.25, 0), ylim = c(0.69, 0.75), resolution = 1000,
           iterations = 500)
plot(x)

?mandelbrot


################################################################################

## OTHER WAY OF DOING MANDELBROT | Z_n+1 = Z_n^2 + c

################################################################################

# Mandelbrot.R
# Myles Harrison
# everydayanaltics.ca
# -------------------

# "Naive" version

mandelbrot_naive <- function(xmin=-2, xmax=2, nx=500,
                             ymin=-1.5, ymax=1.5, ny=500,
                             n=100, showplot=TRUE,
                             cols=colorRampPalette(c("blue","yellow","red","black"))(11))
{

  # variables
  x <- seq(xmin, xmax, length.out=nx)
  y <- seq(ymin, ymax, length.out=ny)
  c <- outer(x,y*1i,FUN="+")
  z <- matrix(0.0, nrow=length(x), ncol=length(y))
  k <- matrix(0.0, nrow=length(x), ncol=length(y))

  for (rep in 1:n) {
    for (i in 1:nx) {
      for (j in 1:ny) {
        if(Mod(z[i,j]) < 2) {
          z[i,j] <- z[i,j]^2 + c[i,j]
          k[i,j] <- k[i,j] + 1
        }
      }
    }
  }

  if (showplot==TRUE) { image(x,y,k,col=cols,xlab="Re(c)",ylab="Im(c)")}

  return(k)

}


# Vectorized version
mandelbrot_vectorized <- function(xmin=-2, xmax=2, nx=500,
                                  ymin=-1.5, ymax=1.5, ny=500,
                                  n=100, showplot=TRUE,
                                  cols=colorRampPalette(c("blue","yellow","red","black"))(11))
{

  # variables
  x <- seq(xmin, xmax, length.out=nx)
  y <- seq(ymin, ymax, length.out=ny)
  c <- outer(x,y*1i,FUN="+")
  z <- matrix(0.0, nrow=length(x), ncol=length(y))
  k <- matrix(0.0, nrow=length(x), ncol=length(y))

  for (rep in 1:n) {
    index <- which(Mod(z) < 2)
    z[index] <- z[index]^2 + c[index]
    k[index] <- k[index] + 1
  }

  if (showplot==TRUE) { image(x,y,k,col=cols, xlab="Re(c)", ylab="Im(c)")}

  return(k)

}

# Compare naive vs. vectorized runtimes
compare_runtimes <- function()
{
  system.time(mandelbrot_naive(showplot=F))
  system.time(mandelbrot_vectorized(showplot=F))
}
