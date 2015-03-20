## Data Cleaning Class Project
#  Some helpful settings to help understand and change the flow 
  debug=FALSE                           # (change to debug=TRUE to see details)
  viewtables=FALSE                       # executes a View request (use with RStudio)
  wide=TRUE                             # creates a wide tidy dataset
  long=FALSE                             # creates a long tidy dataset
  unzippedfiles="./ProjectFile" # directory that the files will be unzipped to

# load the libraries used in the script  
  library(plyr, warn.conflicts=FALSE, quietly=TRUE)
  library(dplyr, warn.conflicts=FALSE, quietly=TRUE)
#  the following libraries loaded with suppressed warnings and messages to clean 
#  terminal output  
  suppressWarnings(suppressMessages(library(data.table)))
  suppressWarnings(suppressMessages(library(reshape2)))
  suppressWarnings(suppressMessages(library(lubridate)))

###################Start of getdata() #########################  
## getdata function is used to read in and unzip the data
##  this function is not called if the folder pointed to by the unzippedfiles variable is found  
  getdata <- function() {
    print("downloading the file")
    fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

    setInternet2(use=TRUE) ## setup for https transfer
    setInternet2(NA)  ## verify it worked

    download.file(fileURL,"./ProjectFile.zip" ) 
    print("file downloaded, starting unzip process")
    unzip("./ProjectFile.zip",exdir=unzippedfiles)
    print("unzip complete")
    z<-file.create(paste0(unzippedfiles,"/CreationDate_",
            as.character(format(Sys.Date(), "%Y%m%d"))))   # document creation date  
    if (debug) {print("** files ready")}  ###  for watching the process
  }
################# END of getdata() ###########################

############ Start of cleanvariablenames() ###################  
## cleanvariablenames function is used to reformat the variable names to conform to recommended
##    naming conventions  
  cleanvariablenames <- function(y) {
    names(y) <- gsub("\\(","",names(y))   ## remove '(')
    names(y) <- gsub("\\)","",names(y))   ## remove ')'
    names(y) <- gsub("\\-","",names(y))   ## remove '-'
    names(y) <- gsub(",","",names(y))     ## remove commas
    names(y) <- tolower(names(y))         ## comment this statement out if you believe in camelCase 
    names(y)
  }
############ END of cleanvariablenames() ###################    
  
###########################################################  
##  CODE EXECUTION STARTS HERE  ------->
###########################################################   
  
#  check to see if the unzipped directory is present  
# THIS SHOULD NEVER HAPPEN IF THE FILE IS THERE  
  if (!file.exists(unzippedfiles)) {
    if (debug) {print("** data files not found - downloading now")}  ###  for watching the process 
    dir.create(unzippedfiles)
    getdata()
  }

# I chose to read in the list of txt files in the directories rather than hardcoding the names
#   since I was originally reading in the Inertial Signals data and this made it simpler...
#   it turned out that Inertail Signals was not needed for this exercise
# lvl0 is the top level directory
# lvl1 is the directory containing the base files for the test and train measurements
# lvl2 is the directory containing the Inertial Signal files for the test and train measurements  

  if (debug) {print(paste0(unzippedfiles,"\\UCI HAR Dataset"),pattern="\\.txt$",full.names=T)}
  lvl0files <- dir(paste0(unzippedfiles,"\\UCI HAR Dataset"),pattern="\\.txt$",full.names=T)
  testlvl1  <- dir(paste0(unzippedfiles,"\\UCI HAR Dataset/test"),pattern="\\.txt$",full.names=T)
  testlvl2  <- dir(paste0(unzippedfiles,"\\UCI HAR Dataset/test/Inertial Signals"),pattern="\\.txt$",full.names=T)
  trainlvl1 <- dir(paste0(unzippedfiles,"\\UCI HAR Dataset/train"),pattern="\\.txt$",full.names=T)
  trainlvl2 <- dir(paste0(unzippedfiles,"\\UCI HAR Dataset/train/Inertial Signals"),pattern="\\.txt$",full.names=T)
  if (debug) {print("got the directory information")}
  
# read the files from the base directory
  if (debug) {print(lvl0files)}           ## diagnositcs
  activitylabels <- read.table(lvl0files[1])
  features <- read.table(lvl0files[2])
  if (debug) {print("got the level 0 data")}  ## diagnositcs
  
# read the files from the lvl 1 directories
## measures and features are read in as data.table for improved throughput  
  trainx <- as.data.table(read.table(trainlvl1[2]))    # measures
  trainy <- as.data.table(read.table(trainlvl1[3]))    # features  
  trainsubject <- read.table(trainlvl1[1])             # subjects
  testsubject <- read.table(testlvl1[1])               # subjects
  testy <- as.data.table(read.table(testlvl1[3]))      # features
  testx <- as.data.table(read.table(testlvl1[2]))      # measures
  if (debug) {print("got the level 1 data")}   ## diagnositcs
  
# first step is to combine the columns together for the test data
  test<- cbind(testsubject,testy,testx)
  if (debug) {print(paste0("Dimension of test=",dim(test)))}    ##  for watching details
# next step is to combine the columns together for the train data
  train <- cbind(trainsubject,trainy,trainx)
  if (debug) {print(paste0("Dimension of train=",dim(train)))}    ##  for watching details
# finally we concatonate the train data to the test data
  mergedset <- rbind(train,test)
  if (debug) {print(paste0("Dimension of mergedset=",dim(mergedset)))} ##  for watching details

## note that at this point we could clean up some of the memory in the event you have a small system
## to do so uncomment the next line - left in for analysis
## rm(trainx,trainy,testy,testx,trainsubject,testsubject)  
  
# the Forum discussions led me to ignore the 7 mean values associated with angles
# these values all contained Mean (capital M) in their names, to include those into the 
# list of columns analyzed, simply modify the value grepped for in the next line to "[Mm]ean"
# Another concern was to eliminate the meanFreg() variables as well and this could have been 
# done by changing the grep function call to include the phrase **,fixed=TRUE** immediately 
# preceding the final 2 parentheses in the function call for example ...[,2]],fixed=TRUE))
  keeplabels <- c(grep("mean|std",features[,2]))  # features we are intested in
  keepcolumns <- c(1,2,keeplabels+2)  # the subject and activity are not in the features file (+2)
  if (debug) {
    print(paste0("# labels retained from features file=",length(keeplabels),
                 ", # of columns=",length(keepcolumns)))}

# get rid of unneeded columns (only keep columns with mean or std in their name)
  mergedset <- mergedset[,keepcolumns]

# put in the column names
  names(mergedset) <-c("subject", "activity", as.character(features[keeplabels,2]))

# add descriptive names for the activities
  mergedset$activity <- activitylabels[mergedset$activity,2]

# "standardize" variable names to recommended "standards" 
# not sure I completely agree with all the "rules" but I tried to comply with recommendations   
  names(mergedset) <- cleanvariablenames(mergedset)

# show the "standardized" names if in debug mode to watch the details of the process
  if (debug) { print(names(mergedset)) }
  
  if (viewtables) {View(mergedset)}  ## to make it easier to validate results

  if (debug) {cat("creating widegroupmeans\n")}
# build the wide means arrays  
  widegroupmeans <- mergedset %>% group_by(subject,activity) %>% 
     summarise_each(funs(mean))
  if (debug) {cat("done creating widegroupmeans\n")}
  
  if (viewtables & wide) {View(widegroupmeans)}  ## this is the tidy file which gets uploaded

# build the long means array if requested   
  if (long) {
    if (debug) {cat("creating longgroupmeans\n")}
    longgroupmeans <- melt(widegroupmeans, id=c("subject","activity")) %>% 
      arrange(subject,activity,variable)
    names(longgroupmeans)[3:4] <- c("measure","mean")
    if (viewtables & long) {View(longgroupmeans)}
    if (debug) {cat("done creating longgroupmeans\n")}
  }

  
##  Here is where we write out the table(s)

  if (wide) {
    write.table(widegroupmeans,file="./widetidydataset.txt",row.names=FALSE)
    print("A wide version of the tidy data set has been written to './widetidydataset'")
  }
  if (long) {
    write.table(longgroupmeans,file="./longtidydataset.txt",row.names=FALSE)
    print("A long version of the tidy data set has been written to './longtidydataset'")
  }  
  cat("\nTo read these files back in use the command")
  cat("\n    read.table(\"./xxxxtidydataset.txt\",header=TRUE)\n")
  cat("          where xxxx is wide or long depending on the requested output\n")
  
  print("run_analysis script has completed")        
        