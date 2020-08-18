# NEWS

* 0.0.1.1000 Added a `NEWS.md` file to track changes to the package. In addition, now included an _addplot_ function to simplify the logging of plots, also now the inclusion of scroll bars on tables is optional using a new _big=TRUE_ option in _addtable_. Have reverted to a developmental version number to reflect my dissatisfaction with the package, there are many improvements that can still be made.

* 0.0.1.950 exported _write_css_ and _write_head_, so they can be imported into other packages.

* 0.0.1.900 modified setuphtml so that only the files listed in the resultTable file and the HTML and CSS files are deleted if cleanslate=TRUE. In this way any data files and other files stored in resdir remain safe.

* 0.0.1.850 Modified the functions so that the output html files could have any desired name rather than having to accept the default of 'aMSE'. 

* 0.0.1.800 Modified both addtable and addplot so that the input filename need not have the full path included.
