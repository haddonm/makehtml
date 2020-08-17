
library(rutilsMH)
library(makehtml)

starttime <- as.character(Sys.time())
# obviously you should define your own directory
indir <- "C:/Users/User/Dropbox/rcode2/makehtml/data-raw" 
resdir <- filenametopath(indir,"result")
dirExists(resdir,verbose=TRUE)
runlabel <- "runone"
analysis <- "Schaefer"
resfile <- setuphtml(resdir=resdir,runname=runlabel,analysis=analysis)

catch <- c(60913,72294,78353,91522,78288,110417,114590,76841,41965,
           50058,64094,89194,129701,160134,200340,192458,224810,183685,
           192234,138918,138623,140581)
effort <- c(5879,6295,6771,8233,6830,10488,10801,9584,5961,5930,6397,
            9377,13958,20381,23984,23013,31856,18726,31529,36423,
            24995,17806)
schaef <- as.data.frame(cbind(year=1934:1955,effort=effort,catch=catch,
                cpue=catch/effort)) # Schaefer's 1957 yellowfin data
# First save the data
filen <- filenametopath(resdir,"schaef.csv")
addtable(intable=schaef,filen=filen,resfile=resfile,category="data",
         caption="Schaefer's original 1957 Yellowfin Tuna fishery data.")
# sort the table on catch
filen <- filenametopath(resdir,"sortedschaef.csv")
sortyft <- schaef[order(schaef[,"catch"]),]
addtable(intable=sortyft,filen=filen,resfile=resfile,category="data",
         caption="The Schaefer 1957 data sorted on catch.",big=TRUE)

# plot the timeseries of cpue and catch
filename <- makefilename("Schaefer_Fisherydata",runlabel,resdir)
#filename=""
caption <- "The Schaefer Yellowfin tuna fishery data from 1957."
plotprep(width=7,height=4.5,filename=filename,cex=0.9,verbose=FALSE)
parset(plots=c(2,1))
ymax <- getmax(schaef$cpue)
plot(schaef$year,schaef$cpue,type="l",lwd=2,ylim=c(0,ymax),
     panel.first=grid(),xlab="",ylab="CPUE")
ymax <- getmax(schaef$catch)
plot(schaef$year,schaef$catch,type="l",lwd=2,ylim=c(0,ymax),
     panel.first=grid(),xlab="",ylab="Catch")
addplot(filen=filename,resfile=resfile,"Fishery",caption=caption)  

txt <- paste0("This is some text that I would want to insert into the ",
              "the html to add a little interpretation to the previous ",
              "plot and lead on the following analysis.")

# plot the catch against the effort
filename <- makefilename("Schaefer_CatchvEffort",runlabel,resdir)
#filename=""
caption <- "The Schaefer Yellowfin tuna data Catch vs Effort."
plotprep(width=7,height=3,filename=filename,cex=0.9,verbose=FALSE)
ymax <- getmax(schaef$catch)
plot(schaef$effort,schaef$catch,type="p",pch=16,cex=1.2,ylim=c(0,ymax),
     panel.first=grid(),xlab="Effort",ylab="Catch")
addplot(filen=filename,resfile=resfile,"Fishery",caption=caption)  


#effort data
filename <- makefilename("Schaefer_Effortdata",runlabel,resdir)
#filename=""
caption <- "The Schaefer Yellowfin tuna effort data from 1957."
plotprep(width=7,height=4.5,newdev=FALSE,filename=filename,cex=0.9,
         verbose=FALSE)
ymax <- getmax(schaef$effort)
plot(schaef$year,schaef$effort,type="l",lwd=2,ylim=c(0,ymax),
     panel.first=grid(),xlab="",ylab="Effort Class 4 clipper days '000s")
addplot(filen=filename,resfile=resfile,"Effort",caption=caption)  


endtime <- as.character(Sys.time())


reportlist <- list(
  runname=runlabel,
  starttime=starttime,endtime=endtime
)
str(reportlist,max.level = 1)

runnotes <- "This is merely to illustrate how to use the package."

#  source(filenametopath(sourcedir,"sourcer.R"))
make_html(replist=reportlist,resdir=resdir,width=500,openfile=TRUE,
          runnotes=runnotes,verbose=FALSE,packagename="makehtml",
          analysis=analysis)




# plot the timeseries of cpue and catch















