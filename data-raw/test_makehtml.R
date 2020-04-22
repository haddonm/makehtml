


library(makehtml)


abmselist <- list(runname="Test_HTML",starttime=Sys.time(),
                  Nwarnings=NA,runnotes=NULL)
plotdir="C:/Users/User/Dropbox/AbMSE/AbMSEuse/plots"



# Need some indication of a successful AbMSE run.

make_html(replist=abmselist,plotdir=plotdir,width=750)





reportlist <- list(
  runname=runname,
  starttime=starttime,
  endtime=endtime,
  regionC=regionC,
  regionD=regionD,
  product=product,
  glb=glb,
  constants=constants
)
str(reportlist,max.level = 1)


make_html(replist=reportlist,rundir=rundir,width=500,
          openfile=TRUE,runnotes=NULL,verbose=TRUE)
