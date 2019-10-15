rm(list=ls())
.libPaths(("Z://mylib"))
library(ncdf4)

setwd("Z:/GL/Flake/erai_global_runs")
files <- list.files(pattern="gflake.nc")
files

nc <- nc_open(files[1])
time <- as.POSIXct(ncvar_get(nc,'time')*86400,origin='1970-01-01', tz ='UTC')
time1 <- strptime(time, format="%Y-%m-%d")
length(time1)

names(nc$var)

globalflakemat <- matrix(NA, ncol=length(time1), nrow=length(files))
pos <- matrix(NA, ncol=2, nrow=length(files))
ids <- rep(NA, length(files))

aa <- apply(as.matrix(files),1, function(x){as.numeric(paste(strsplit(x, "_")[[1]][2]))})

for(i in 1:length(files)){
  nc <- nc_open(files[i])
  ids[i] <- as.numeric(paste(strsplit(files[i], "_")[[1]][2]))
  pos[i,1] <- ncatt_get(nc,0)$Latitude
  pos[i,2] <- ncatt_get(nc,0)$Longitude
  globalflakemat[i,] <- ncvar_get(nc, "FLake_LSWT")
  print(i)
  nc_close(nc)
  }

rownames(globalflakemat) <- rownames(pos) <- ids
globalflakemat <- globalflakemat[order(ids),]
globalpos <- pos[order(ids),2:1]

dim(globalflakemat); dim(globalpos)

plot(globalpos, pch=15, cex=0.5)

colnames(globalflakemat) <- paste(time1)

save(globalflakemat, file="Z:/GL/Flake/May2017/globalflakemat.Rdata")
save(globalpos, file="Z:/GL/Flake/May2017/globalpos.Rdata")

plot(globalflakemat[4,], ylim=c(0,35))

files
library(ncdf4)
nc <- nc_open("Z:/GL/Flake/globalflake//GRID_1892_gflake.nc")
time <- as.POSIXct(ncvar_get(nc,'time')*86400,origin='1970-01-01', tz ='UTC')
time1 <- strptime(time, format="%Y-%m-%d") #
dtime <- round((time1$year + time1$yday/365 + 1900),2)
foy <- time1$yday %/% 7 
foy
save(foy, file="Z:/GL/Flake/May2017//tempapp//foy.Rdata")

library(fda)
fulllam <- df2lambda(c(foy), basis=create.bspline.basis(c(0,52), nbasis=53), df=12)


seasonalfd <- function(dat, wk){
  
  rounded <- round(unlist(lapply(split(dat, wk), mean)),2)
  zeros= as.numeric(names(rounded[rounded<0.5]))+1
  shortBasis <- create.bspline.basis(c(0,52), nbasis=53)
  if (length(zeros)==0){
    ff <- Data2fd(dat, argvals=wk, basis=shortBasis, lambda=fulllam)
    nco <- ff$coefs}
  
  if (length(zeros)>0){
    mylam <- df2lambda(c(wk), basis=create.bspline.basis(c(0,52), nbasis=53-length(zeros)), df=12)
    redshortBasis <- create.bspline.basis(c(0,52), nbasis=53, dropind=zeros)
    ff <- Data2fd(dat, argvals=wk, basis=redshortBasis, lambda=mylam)
    nco <- rep(0, 53)
    nco[-zeros] <- ff$coefs
  }
  
  nco
}

list.files()
setwd("Z:/GL/Flake/May2017/")
plot(globalflakemat[1600,])

skip <- which(rowSums(globalflakemat==0)>410)

dim(globalflakemat)

globalflakecoefs <- matrix(NA, nrow=nrow(globalflakemat), ncol=53)
sids <- (1:nrow(globalflakemat))[-skip]
for (i in sids){
  dee <- seasonalfd(dat=globalflakemat[i,], wk=foy)
  globalflakecoefs[i,] <- dee
  print(i)
}

globalflakecoefs[globalflakecoefs<0] <- 0
globalfd <- fd(t(globalflakecoefs) , create.bspline.basis(c(0,52), nbasis=53))

setwd("Z:/GL/Flake/May2017/")
list.files()

load("arc_fpca.Rdata", verbose=TRUE)
load("arcfd.Rdata", verbose=TRUE)

library(fda)
par(mfrow=c(1,1), cex.main=1.5, cex.axis=1.5, cex.lab=1.5)
sc10 <- matrix(NA,ncol=2,nrow=nrow(globalflakemat))

  
meanfd <- fd(rowMeans(t(globalflakecoefs), na.rm=TRUE), create.bspline.basis(c(0,52), nbasis=53))
  
  
for(i in sids){
  sc10[i,] <- inprod(arc.fpc$harmonics, globalfd[i]-mean(arcfd))
  print(i)
}


plot(sc10)
sc10[skip,]
library(MASS)
library(RColorBrewer)
sc1 <- arc.fpc$scores[,1]
sc2 <- arc.fpc$scores[,2]
rd <- read.csv("lakeinfo_9groups_may2017.csv")
newgroup <- rd$Group
sort(table(newgroup))
dat <- data.frame(cbind(newgroup, sc1, sc2))
myqda <- qda(newgroup ~ sc1 + sc2, data = dat)

preds <- predict(myqda, newdata=data.frame(cbind(sc1=sc10[,1], sc2=sc10[,2])))

preds$class
mycols <- brewer.pal(9, "RdYlBu")

display.brewer.all(n=NULL, type="all", select=NULL, exact.n=TRUE, 
                   colorblindFriendly=TRUE)

dim(globalpos)
length(preds$class)


plot(globalpos[-skip,], col=mycols[preds$class[-skip]], pch=15)



global_set <- cbind(globalpos[-skip,], preds$class[-skip])


save(global_set, file="Z://GL//globalset.Rdata")

shortBasis <- create.bspline.basis(c(0,52), nbasis=53)
globalmean <- fd(do.call(cbind,lapply(split(data.frame(globalflakecoefs), preds$class), colMeans)), shortBasis)
save(globalmean, file="Z://GL//globalmean.Rdata")

