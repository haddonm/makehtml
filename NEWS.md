# NEWS


* 2021-07-04 0.0.1 Development is ongoing but makehtml is now operational so I have removed the fourth development number. Today I have added an addtext function for adding text blocks to a tab. Next steps might be to try to simplify adding objects even further. 

* 2021-06-28 0.0.1.400 I removed .RData files from being deleted when cleanslate=TRUE in the 'setuphtml' function. This is important if anyone saves objects as .RData files in the rundir, which would be a good place to keep them.

* 0.0.1.500 Improved the home page layout and tidied addplot and addtable

* 0.0.1.650 altered 'setuphtml' so that 'cleanslate' does not delete .csv files, which prevents 'cleanslate' deleting the data files if resdir is also used to store a run's data. 

* 0.0.1.700 makehtml added the endmakehtml function that writes out the final syntax needed
at the end of generating a set of results into a website.

* 0.0.1.750 Further modification to addplot, addtable, and setuphtml to simplify their use. Input filenames now must not include the path to resdir

* 0.0.1.800 Modified both addtable and addplot so that the input filename need not have the full path included.

* 0.0.1.850 Modified the functions so that the output html files could have any desired name rather than having to accept the default of 'aMSE'. 

* 0.0.1.900 modified setuphtml so that only the files listed in the resultTable file and the HTML and CSS files are deleted if cleanslate=TRUE. In this way any data files and other files stored in resdir remain safe.

* 0.0.1.950 exported _write_css_ and _write_head_, so they can be imported into other packages.

* 0.0.1.1000 Added a `NEWS.md` file to track changes to the package. In addition, now included an _addplot_ function to simplify the logging of plots, also now the inclusion of scroll bars on tables is optional using a new _big=TRUE_ option in _addtable_. Have reverted to a developmental version number to reflect my dissatisfaction with the package, there are many improvements that can still be made.

