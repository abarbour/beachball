#http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Murrell.pdf
#
library(grid)
library(raster)
#library(ReadImages)
library(png)
library(lattice)

x <- -10:10
nx <- length(x)
y <- runif(nx)

#
mt <- readPNG("mt.png")
mtr <- as.raster(mt)

imdim <- grid::unit((runif(nx)+1)/37, 'npc')
#xyplot(y ~ x, panel=function(x, y, ...){grid.raster(mt, x=x, y=y, width=imdim, height=imdim, default.units="native") #, just="left")})

plot(0:1,0:1)
x. <- seq(0,1,length.out=9)
y. <- runif(9,0.1,0.9)
m. <- runif(9,0,5)
mmax <- 4
mn. <- m./mmax
grid.raster(mtr, 
	height=unit(mn.,'cm'), 
	x=grconvertX(x., from='user', to='nic'), 
	y=grconvertY(y., from='user', to='nic'))

#
# next try SVG
#
#https://www.stat.auckland.ac.nz/~paul/R/grImport/import.pdf
#https://sjp.co.nz/projects/grimport2/grImport2.html
