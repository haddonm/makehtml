% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makehtml_funs.R
\name{addtable}
\alias{addtable}
\title{addtable adds a table to the output}
\usage{
addtable(intable, filen, rundir, category = "any", caption = "", big = FALSE)
}
\arguments{
\item{intable}{the table or data.frame to be saved and output}

\item{filen}{only the filename being given to the table, no path included}

\item{rundir}{full path to the directory to contain the results}

\item{category}{what HTML tab should it be added to? default="any"}

\item{caption}{the caption for the figure or table, default = ""}

\item{big}{if FALSE (the default) the complete table is generated, 
if TRUE then scroll bars are added.}
}
\value{
nothing but it does add a line to resultTable.csv and saves a csv 
    file to rundir
}
\description{
If the filen remains empty, then addtable does nothing.
    Otherwise, addtable saves a table as a csv file into the rundir.
    Then it logs the filename in the the resultTable.csv file containing 
    the  names of each file to be plotted or tabulated. This function 
    saves having to combined the filename with the rundir, and then
    does the filename logging for you. If no category is added 
    explicitly then the local webpage will have an 'any' tab 
    containing these unloved results.
}
\examples{
indir <- tempdir()
rundir <- filenametopath(indir,"result")
dirExists(rundir,verbose=FALSE)
resfile <- setuphtml(rundir)
filen <- "example.csv"
egtable <- matrix(rnorm(25,0,1),nrow=5,ncol=5)
addtable(egtable,filen=filen,rundir=rundir,"ExampleTable",
            caption="An example Table")
dir(rundir) # examine the resfile and the example.csv files.
make_html(rundir=rundir,packagename="makehtml",htmlname="exampletable") 
}
