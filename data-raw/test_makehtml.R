

library(MQMF)
library(makehtml)

starttime <- as.character(Sys.time())

#indir <- tempdir()
indir <- "C:/Users/User/Dropbox/rcode2/makehtml/data-raw"
resdir <- filenametopath(indir,"result")
dirExists(resdir,verbose=TRUE)
runlabel <- "runone"
resfile <- setuphtml(resdir=resdir,runname=runlabel)
data(schaef)   # the data-set used - the original yellowfin tuna data


filen <- filenametopath(resdir,"schaef.csv")
addtable(intable=schaef,filen=filen,resfile=resfile,category="Fishery",
         caption="Schaefer's original 1957 Yellowfin Tuna fishery data.")

filen <- filenametopath(resdir,"egtable.csv")
egtab <- matrix(rnorm(25,0,1),nrow=5,ncol=5)
addtable(intable=egtab,filen=filen,resfile=resfile,category="Fishery",
         caption="A second example table")


file <- paste0("Schaefer_Fisherydata",runlabel,".png")
filename <- filenametopath(resdir,file)  #  filename=""
plotprep(width=7,height=4.5,newdev=FALSE,filename=filename,cex=0.9,
         verbose=FALSE)

parset(plots=c(2,1))
ymax <- getmax(schaef$cpue)
plot(schaef$year,schaef$cpue,type="l",lwd=2,ylim=c(0,ymax),
     panel.first=grid(),xlab="",ylab="CPUE")
ymax <- getmax(schaef$catch)
plot(schaef$year,schaef$catch,type="l",lwd=2,ylim=c(0,ymax),
     panel.first=grid(),xlab="",ylab="Catch")
plotoff <- dev.off()

caption <- "The Schaefer Yellowfin tuna fishery data from 1957."
logfilename(filename,resfile=resfile,category="Schaefer",caption)

#effort data
file <- paste0("Schaefer_Effortdata",runlabel,".png")
filename <- filenametopath(resdir,file)  #  filename=""
plotprep(width=7,height=4.5,newdev=FALSE,filename=filename,cex=0.9,
         verbose=FALSE)
ymax <- getmax(schaef$effort)
plot(schaef$year,schaef$effort,type="l",lwd=2,ylim=c(0,ymax),
     panel.first=grid(),xlab="",ylab="Effort Class 4 clipper days '000s")
plotoff <- dev.off()

caption <- "The Schaefer Yellowfin tuna effort data from 1957."
logfilename(filename,resfile=resfile,category="Effort",caption)



endtime <- as.character(Sys.time())


reportlist <- list(
  runname=runlabel,
  starttime=starttime,endtime=endtime
)
str(reportlist,max.level = 1)

runnotes <- "This is merely to illustrate how to use the package."

#  source(filenametopath(sourcedir,"sourcer.R"))
make_html(replist=reportlist,resdir=resdir,width=500,
          openfile=TRUE,runnotes=runnotes,verbose=FALSE)


