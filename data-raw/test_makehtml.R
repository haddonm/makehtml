


library(makehtml)


abmselist <- list(runname="Test_HTML",starttime=Sys.time(),
                  Nwarnings=NA,runnotes=NULL)
plotdir="C:/Users/Malcolm/Dropbox/AbMSE/AbMSEuse/plots"



# Need some indication of a successful AbMSE run.

make_html(replist=abmselist,plotdir=plotdir,width=750)

