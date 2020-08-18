
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Latest Changes

Read the news.md for the latest changes to the codebase.

# makehtml

<!-- badges: start -->

<!-- badges: end -->

Generic R code for producing multi-tabbed HTML output for plotted
results in a specific directory. The objective is to provide a standard
method for an initial presentation of extensive results from an
analysis. Rmarkdown is wondferful but sometimes one just needs to review
the data and the results and so needs a convenient way of viewing tables
and plots. makehtml aims to provide that method.

This code is adapted and revised from code put together by Ian Taylor
inside the R package **r4ss**. Essentially it creates a CSS3 file that
is used by a series of HTML files, each of which represents a user
defined category of results. All plots should be .png files, and all
tables are derived from saved .csv files. All files, the CSS, HTML, .png
and csv files are saved to a user defined directory (*resdir*), which is
the main user input requirement, along with a *runlabel*. One can now
provide a name for the webpages rather than use the default, which, for
my convenience is set to ‘aMSE’, and this can be done in the *setuphtml*
function by changing the value of the *analysis* argument.

## Installation

You can install the development version from
[GitHub](https://github.com/haddonm/aMSE) with:

``` r
# # If you do not have the required packages installed unhash the code in this block and run it
# if (!require(devtools)){install.packages("devtools")} 
# 
# devtools::install_github("https://github.com/haddonm/makehtml")
# 
# # you will also need rutilsMH
# 
# devtools::install_github("https://github.com/haddonm/rutilsMH")
```

Alternatively, you can generate a branch that you can work on by cloning
the repository, which, again, can be done very simply within RStudio.
Open the New Project option in the project dialog at the top right of
the RStudio screen and selection Version Control, then use
‘<https://github.com/haddonm/makehtml>’ in the top box, identify where
you want the new directory put, and press return.

It would be a good idea to read Hadley Wickham’s draft chapter on Git
and GitHub at <https://r-pkgs.org/index.html>.

## Example

This is a basic example which illustrates the use of makehtml, though I
have hashed out the final call to *make\_html* so that the RMarkdown
version of the readme.md file does not generate a local website:

``` r
library(makehtml)
library(rutilsMH) # for plotprep and parset
starttime <- as.character(Sys.time())

# obviously you should define your own directory
indir <- "C:/Users/User/Dropbox/rcode2/makehtml/data-raw" 
resdir <- filenametopath(indir,"result")
dirExists(resdir,verbose=TRUE)
#> C:/Users/User/Dropbox/rcode2/makehtml/data-raw/result :  exists
runlabel <- "runone"
analysis <- "Schaefer"
resfile <- setuphtml(resdir=resdir,runname=runlabel,analysis=analysis)

# Some data
catch <- c(60913,72294,78353,91522,78288,110417,114590,76841,41965,
           50058,64094,89194,129701,160134,200340,192458,224810,183685,
           192234,138918,138623,140581)
effort <- c(5879,6295,6771,8233,6830,10488,10801,9584,5961,5930,6397,
            9377,13958,20381,23984,23013,31856,18726,31529,36423,
            24995,17806)
schaef <- as.data.frame(cbind(year=1934:1955,effort=effort,catch=catch,
                cpue=catch/effort)) # Schaefer's 1957 yellowfin data
# First save the data; obvioulsy you should use unique filenames
filen <- filenametopath(resdir,"schaef.csv")  # csv files only
addtable(intable=schaef,filen=filen,resfile=resfile,category="data",
         caption="Schaefer's original 1957 Yellowfin Tuna fishery data.")
# addtable logs the filename automatically
# sort the table on catch, just to provide two tables
filen <- filenametopath(resdir,"sortedschaef.csv")
sortyft <- schaef[order(schaef[,"catch"]),]
addtable(intable=sortyft,filen=filen,resfile=resfile,category="data",
         caption="The Schaefer 1957 data sorted on catch.")

# plot the timeseries of cpue and catch,  only png files
filename <- plotfilename("Schaefer_Fisherydata",runlabel,resdir)
plotprep(width=7,height=4.5,filename=filename,cex=0.9,verbose=FALSE)
parset(plots=c(2,1))
ymax <- getmax(schaef$cpue)
plot(schaef$year,schaef$cpue,type="l",lwd=2,ylim=c(0,ymax),
     panel.first=grid(),xlab="",ylab="CPUE")
ymax <- getmax(schaef$catch)
plot(schaef$year,schaef$catch,type="l",lwd=2,ylim=c(0,ymax),
     panel.first=grid(),xlab="",ylab="Catch")
caption <- "The Schaefer Yellowfin tuna fishery data from 1957."
addplot(filen=filename,resfile=resfile,category="Fishery",caption=caption)  

# plot the catch against the effort
filename <- plotfilename("Schaefer_CatchvEffort",runlabel,resdir)
caption <- "The Schaefer Yellowfin tuna data Catch vs Effort."
plotprep(width=7,height=3,filename=filename,cex=0.9,verbose=FALSE)
ymax <- getmax(schaef$catch)
plot(schaef$effort,schaef$catch,type="p",pch=16,cex=1.2,ylim=c(0,ymax),
     panel.first=grid(),xlab="Effort",ylab="Catch")
addplot(filen=filename,resfile=resfile,category="Fishery",caption=caption)  

# plot the effort data; if filename="" no png file is produced or added, but a
# new plot is certainly generated. Once as you want it, then use plotfilename.
filename <- plotfilename("Schaefer_Effortdata",runlabel,resdir)
caption <- "The Schaefer Yellowfin tuna effort data from 1957."
plotprep(width=7,height=4.5,newdev=FALSE,filename=filename,cex=0.9,
         verbose=FALSE)
ymax <- getmax(schaef$effort)
plot(schaef$year,schaef$effort,type="l",lwd=2,ylim=c(0,ymax),
     panel.first=grid(),xlab="",ylab="Effort Class 4 clipper days '000s")
addplot(filen=filename,resfile=resfile,category="Effort",caption=caption)  

endtime <- as.character(Sys.time())

reportlist <- list(  #these 3 are minimal requirements for the replist
  runname=runlabel,  # though the whole of reeplist can be NULL if 
  starttime=starttime,endtime=endtime # you are feeling lazy.
)
str(reportlist,max.level = 1)
#> List of 3
#>  $ runname  : chr "runone"
#>  $ starttime: chr "2020-08-17 14:57:03"
#>  $ endtime  : chr "2020-08-17 14:57:04"

runnotes <- "This is merely to illustrate how to use the package."
# If you unhash the make_html component it will open the local 
# website generated inside resdir, if openfile=FALSE it will be 
# generated but not opened.
#
# make_html(replist=reportlist,resdir=resdir,width=500,openfile=TRUE,
#          runnotes=runnotes,verbose=FALSE,packagename="makehtml")
```
