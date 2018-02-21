# `usses_water`: Experimental Test of Climate Change Impacts on Grassland ANPP

Research project data and code for an analysis of drought and irrigation experiment at US Sheep Experiment State (USSES) near Dubois, ID.
Goal is to quantify the impact of a 50% increase/decrease of precipitation on:

* ANPP through time
* Relationship of ANPP to soil moisture
* Species composition

##	Running the code

To fully run this code you will need R and a number of R packages.
Running `00_source_usses_water_scripts` in the `code/` directory will run all necessary scripts to complete the analysis.
The code will also stop and send errors letting you know what R packages need to be installed.
Once all packages are installed, the code should run all the way through.

You must ource the script from RStudio, rather than from command line, because we use some Rstudio-specific commands to set working directories.

Please create an [issue](https://github.com/atredennick/usses_water/issues) if you encounter any problems.