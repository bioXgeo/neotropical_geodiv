
## Running the SDM Model on multiple species in a slurm environment 

L1_1_sdm_model.R 


### Requirements

#### **Java **

**MacOS:**  On M1 mac you must compile java, and so it's recommended to use the homebrew package manager which sets all system variables correctly, installs the many other things required by Java, and will pickup the correct version for your Mac.  Current version as of this is 19

 `brew install openjdk@19  # which takes a long time`

On ARM/Apple Silicon and because Mac, that's still not enough.  Then you need to have administrator privileges on the mac, and in the terminal run the following I had to also to the following: 

```Bash
# add it to the path
echo 'export PATH="/opt/homebrew/opt/openjdk/bin:$PATH" >> ~/.zshrc
# tell mac to use it 
sudo ln -sfn $(brew --prefix)/opt/openjdk/libexec/openjdk.jdk /Library/Java/JavaVirtualMachines/openjdk.jdk
# tell mac to find it
echo 'export JAVA_HOME=$(/usr/libexec/java_home)' >> ~/.zshrc
```

On Intel Mac, you may be able to just install the standard version of Java which sets these things automatically. 


**Windows:**  go to https://learn.microsoft.com/en-us/java/openjdk/download and download the latest "MSI" version available.  Currently that's

`Windows 	x64 	msi 	microsoft-jdk-17.0.5-windows-x64.msi`


double click the MSI to install it

**HPCC:**  Java is installed on the HPCC and RJava is also installed with several of the versions of R, vesion 4.0.2 for sure. 

#### **GEOS** 

The raster manipulation package `terra` and the maxent model require spatial libs to be installed on your machine

Terra says it requires:  C++11, GDAL (>= 2.2.3), GEOS (>= 3.4.0), PROJ (>= 4.9.3), sqlite3

- On Mac, again use homebrew:  `brew install geos gdal proj`. (sqlite is already on mac)
- On the MSU HPCC, these are already installed but must match your R version (4.0.2 works)
- Windows... ?

#### **R Libs:** This script requires the following R libs to be installed: 

 - rJava
 - terra (formerly raster) (which requires sp and OS-level libraries)
 - sf
 - dismo
 - [ENMeval](https://jamiemkass.github.io/ENMeval/index.html)
 - wallace
 - Maxent Java jar file
    - installing `dismo` seems to also install this file in the correct place, so no further action seems necessary.  However if you have trouble running maxent.jar, try this alternative method that also worked for me: 
        - the latest version available for download on https://biodiversityinformatics.amnh.org/open_source/maxent/  
        - ENMeval does not seem to have directions for installing this, but see this post: https://stackoverflow.com/questions/44813048/maxent-rjava-situation-1001
        - the download above will download a zip file.  Unzip the file and find the `maxent.jar` file inside the zip folder.   
  

Note, you may see the error

```
Error in x$.self$finalize() : attempt to apply non-function
Error in (function (x)  : attempt to apply non-function
```



based on a [message from Rober Hijmans on stack overflow](https://stackoverflow.com/questions/65556253/r-raster-selffinalize-error-causing-failure), you can get rid of these 

> These annoying messages Will disappear with the next release of Rcpp, which is planned for 
> Jan 2023. You can also install the development version of Rcpp like this:

`install.packages("Rcpp", repos="https://rcppcore.github.io/drat")`


**Test that you can use Java properly in R**

On the MSU HPCC you have to make sure you've loaded R and a compatible version of Java. 

In the R console: 

```R 
install.packages('rJava')
requireNamespace('rJava')
# should be no error
rJava::.jpackage('dismo')
# also no error message. 
```

Once R is working, you need to copy the `maxent.jar` program to a place where the dismo package can find it.   According to the post above, the folder can be found using R 

To run the model code you must

1. install packages, you may try to use the `renv` package for that: 
   ```R
   install.packages('renv')
   renv::init()
   ```
1. copy the file `maxent.jar` to the place it needs to be.  Make sure dismo is installed first and Java is working (see above)
    1. find the folder where dismo uses for Java.  In the R console use the command `system.file("java", package="dismo")` - for me on the HPCC that will show `[1] "/mnt/ufs18/home-107/billspat/R/4.0.2-GCC-8.3.0/dismo/java"`
    1. Using the command line or a file manager (outside of R), copy the file `maxent.jar` (out of the zip file if necessary) to the folder show above.  You can copy/paste the folder into windows explorer bar or using the mac terminal
1. get the CHELSA and pre-thinned rasters files accessible to your computer ( currently on ) 
1. edit the file `.Renviron` and change the path for the 'base_path' to match where the data is
    1. current location on the MSU HPCC is /mnt/research/plz-lab/DATA/bg_geodiversity
    1. if mounted on your computer use /Volumes/plz-lab/DATA/bg_geodiversity or ? for PC
    ```bash
    SDM_BASE_PATH="base folder for all data"
    HPCC_BASE_PATH="optional alternate base folder just for HPCC"
    SDM_ENVS_PATH="environmental_variables" # subdir for env
    SDM_OCCS_PATH_TEMPLATE='occurrence_records/%s_thinned_full'
    SDM_OCCS_FILE_TEMPLATE='%s_thinned_wallace.csv' # single quoted
    ```
1. Restart R if necessary to re-read the changes in `.Renviron`


### Testing

There are unit tests for functions in the `/tests` folder.  To run the tests : 


1. install the testing package `install.packages('testthat')`
1. ensure the .Renviron file has the correct BASE_PATH set ( for my laptop, after mounting HPC drive this is `BASE_PATH="/Volumes/plz-lab/DATA/bg_geodiversity"` )
1. in the R console, use `testthat::test_file('tests/test_sdm_model_eval.R')` or in Rstudio, open the file [tests/test_sdm_model_eval.R] and click 'run tests' at the top. 
1. if the data files are not accessible to the program there will many errors

To do a test run of the model, the `testthat` package doesn't work so great (it hides the output ), but there is a test script to try.  It's a bit ragged for will using the model run function, save data, and see if the CSVs have rows of data in them:

```R
source('sdm_model_eval.R')
source('tests/test_model_run.R')
```

this will save CSVs, GeoTIFF of the SDM, and the output from the model (which contains all the source data, the outputs an the SDM) in an Rdata file for re-reading into R named e.mx

The output files of the test script are save to a temp folder that is removed when you quit R/Rstudio

### Running 

The script `sdm_run.R` takes following cli parameter

  `Rscript --no-restore sdm_run.R Genus_species radiuskm [default 1] runNumber [default 1] output_path` 
  
  outputPath is full path to output

  
Example

`Rscript --no-restore sdm_run.R Alouatta_palliata 1 1 /tmp/output_Alouatta_palliata`

*Note: the `--no-restore` option will avoid re-loading any variables or objects you've saved from your interactive R/Rstudio session.  Do not use the --vanilla option as it will not load the environment which the current script depends upon*

#### HPC submit script

The HPC job script is `sdm_run_all_radii.sh` and has job parameters for memory and cores that are known to work but may be larger than necessary.  

#### Convenience Shell Scripts for HPC

The R code in this job script needs some environment variables for the location of the occurrences file and species name.  So there is more scripts that set these variables for you and submitting  one or all of the species using an HPC job.  The shell 'script' `start_jobs.sh` are actually shell 'functions' that create commands you can run to launch the HPC jobs and also for post-processing (see below)

To run a single species from this directory

1. review, edit and save the script `start_jobs.sh` and alter the directories as needs to point to the species files. They are currently set for using the HPC folder for the "plz-lab" research group.   
2. once edits, you can enable the functions by 'sourcing' the script in the terminal. 
   `source start_jobs.R`
   
3. now the shortcut functions are available in your terminal.  
   - To run one species, use the folder name (with underscore):  `run_one_species_sdm Lagothrix_lagotricha`
   - to to submit a job for each folder in the occurrences folder set in this script: `run_all_species_sdm`
   - to run the post-processing after all species models are created, use `post_process_sdm`
      note this is not a job and will run in the terminal node immediately since it doesn't take much
   
#### Post-processing

After the run is done the code is there to :

1. read in the data in the output directory for all species (the naive code assumes that there is a directory for each species, and no non-species directories, so don't create any)
1. for non-SDM data, aggregate into a single file by averaging across runs, and merging into a single CSV with columns
keying the data on `species X radius` saved as a new CSV in the output directory
1. for the variable importance, create a file that combines all those with columns keyed on 
`species X radius X variable` saved as a new CSV in the output directory
1. For the SDMs, create an average SDM across runs for each `species X radius`, then threshold the averaged 
SDM based on the number of occurences  (see code in `sdm_model_eval.R` for details), and save that as a new GeoTIFF
To create CSVs and SDMs that average across runs and, for SDMs are thresholded.  See function `imagePostProcessingAllRadii()`

   





