# makehtml
Generic R code for producing multi-tabbed HTML output for plotted results in a specific directory

This code is adapted and revised from code put together by Ian Taylor inside the R ackage r4ss. Essentially it places a set of CSS codes into a series of HTML files which are processed to include a series of .png plots. There are only two requirements:

* All result files (.png grpahics files) are expected to be in a single directory. Each filename should include a category, such as data, index, timeseries, productivity, etc, ...

* There needs to be a plotFileTable_<name>.csv file in the same directory, which contains the plot filenames to be included, each filename should also have a caption, and a category type 
