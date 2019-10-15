.libPaths("Z://mylib")
library(raster)
library(ggplot2)
library(RColorBrewer)
mp <- NULL
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
mp <- ggplot() +   mapWorld + labs(x="Longitude", y="Latitude")

setwd("Z://Flake//May2017")
load("globalpos.Rdata")
load("globalclass.Rdata")
gp <- data.frame(cbind(x=globalpos[,1],y=globalpos[,2], group=globalclass ))
mypal <- brewer.pal(11, "RdYlBu")
mp1 <- mp+ geom_point(data=gp, aes(x=gp$x, y=gp$y, col=factor(gp$group)),size=2, shape=15)+labs(color = "Group")  + 
      scale_color_manual(values=c("#F46D43" , "#D73027" ,"#A50026","#313695" , "#FEE090","#ABD9E9"
                          ,"#4575B4" ,"#74ADD1", "#FDAE61"  ,"#FFFFBF"))
load("uncert.Rdata")
load("globalflakemat.Rdata")
skip <- which(rowSums(globalflakemat==0)>410)
gp2 <- data.frame(cbind(x=globalpos[-skip,1],y=globalpos[-skip,2], uncert=uncert ))
mp2 <- mp+ geom_point(data=gp2, aes(x=gp2$x, y=gp2$y, color=gp2$uncertainty), size=2, shape=15)+labs(color = "Uncert")  + scale_colour_gradientn(colours = terrain.colors(10))

library(fda)
load("globalfd.Rdata")

plot(globalfd[-skip][globalclass[-skip]==1])

plot(globalfd[-skip][globalclass[-skip]==9])
mc <- c("#FEE090", "#F46D43", "#A50026", "#313695", "#4575B4" ,"#D73027", "#ABD9E9" ,"#FDAE61","#74ADD1")




ngfd <- data.frame(t(globalfd[-skip]$coefs))
ngc <- globalclass[-skip]
meanfd <- fd(do.call(cbind, lapply(split(ngfd, ngc), colMeans)), create.bspline.basis(c(0,52), 53))
plot(meanfd, col=mc, lwd=4, lty=1)


load("arcfd.Rdata")
rd <- read.csv("lakeinfo_9groups_may2017.csv")
omeanfd <- fd(do.call(cbind, lapply(split(data.frame(t(arcfd$coefs)), rd$Group), colMeans)), create.bspline.basis(c(0,52), 53))
plot(omeanfd, col=mc, lwd=4, lty=1, bty="n", xlab="Week of Year", ylab="LSWT")
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray85", border = "NA")
abline(h=seq(0,50,5), col="white", lty=2);abline(v=seq(0,50,5), col="white", lty=2)
lines(omeanfd, col=1, lwd=4, lty=1)
