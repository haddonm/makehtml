

library(rutilsMH)





dbdir <- getDBdir()
prefixdir <- paste0(dbdir,"A_codeUse/aMSEUse/scenarios/")
postfixdir <- "M125h6"
rundir <- filenametopath(prefixdir,postfixdir)
rundir
abmse=TRUE

cleanrundir(rundir)

