

#' @title makehtml functions to generate tabbed HTML output files
#'
#' @description The makehtml package is designed to assist with the presentation
#'     of the results of analyses where multiple types of output files are 
#'     expected. For example, in an Abalone MSE, one would expect to summarize
#'     the data used to define the operating model for each run, to summarize
#'     the time-series outputs for the various populations and trends of 
#'     interest, to summarinze the outcomes in terms of the performance
#'     measured used, the outputs from teh harvest control rules trialed, and so
#'     on, across an array of categories of output. makehtml assists by 
#'     sequentilly plotting these into seperate named tabs in a web-site based
#'     output. 
#'     
#' @seealso{
#'   \link{setuphtml}, \link{make_html}, \link{addplot}, \link{addtable},
#'   \link{addtext}, \link{dirExists}, \link{filenametopath}
#' }
#' 
#' 
#' @docType package
#' @name makehtml
NULL

#' @importFrom utils browseURL packageDescription read.csv write.table
#' @importFrom grDevices dev.off
NULL
