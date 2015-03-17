## Data Cleaning Class Project
#  Some helpful settings to help understand the flow (change to debug=TRUE)
  debug=FALSE
  viewtables=TRUE  # executes a View request (use with RStudio)
  wide=TRUE        # creates a wide tidy dataset
  long=TRUE        # creates a long tidy dataset

  library(plyr)
  library(dplyr)
  library(data.table)
  library(reshape)
  library(lubridate)

###########################################################  
## getdata function is used to read in and unzip the data
  getdata <- function() {
    print("downloading the file")
    fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

    setInternet2(use=TRUE) ## setup for https transfer
    setInternet2(NA)  ## verify it worked

    download.file(fileURL,"./ProjectFile.zip" ) 
    unzip("./ProjectFile.zip",exdir="./ProjectFileUnzipped")
    z<-file.create(paste0("./ProjectFileUnzipped/CreationDate_",
                          as.character(format(Sys.Date(), "%Y%m%d"))))   # document creation date  
    if (debug) {print("** files ready")}  ###  for watching the process
  }
###########################################################

#  check to see if the unzipped directory is present  
# THIS SHOULD NEVER HAPPEN IF THE FILE IS THERE  
if (!file.exists("./ProjectFileUnzipped")) {
  if (debug) {print("** data files not found - downloading now")}  ###  for watching the process 
  dir.create("./ProjectFileUnzipped")
  getdata()
}

# I chose to read in the list of txt files in the directories rather than hardcoding the names
#   since I was originally reading the Inertial Signals data in and this made it simpler
#   turned out that Inertail Signals was not needed for this exercise
# lvl0 is the top level directory
# lvl1 is the directory containing the base files for the test and train measurements
# lvl2 is the directory containing the Inertial Signal files for the test and train measurements  

  lvl0files <- dir("ProjectFileUnzipped/UCI HAR Dataset",pattern="\\.txt$",full.names=T)
  testlvl1 <- dir("ProjectFileUnzipped/UCI HAR Dataset/test",pattern="\\.txt$",full.names=T)
  testlvl2 <- dir("ProjectFileUnzipped/UCI HAR Dataset/test/Inertial Signals",pattern="\\.txt$",full.names=T)
  trainlvl1 <- dir("ProjectFileUnzipped/UCI HAR Dataset/train",pattern="\\.txt$",full.names=T)
  trainlvl2 <- dir("ProjectFileUnzipped/UCI HAR Dataset/train/Inertial Signals",pattern="\\.txt$",full.names=T)

# read the files from the base directory
  activitylabels <- read.table(lvl0files[1])
  features <- read.table(lvl0files[2])

# read the files from the lvl 1 directories
  trainx <- read.table(trainlvl1[2])                   # measures
  trainy <- read.table(trainlvl1[3])                   # features  
  trainsubject <- read.table(trainlvl1[1])             # subjects
  testsubject <- read.table(testlvl1[1])               # subjects
  testy <- read.table(testlvl1[3])                     # features
  testx <- read.table(testlvl1[2])                     # measures

# first step is to combine the columns together for the test data
  test<- cbind(testsubject,testy,testx)
  if (debug) {print(paste0("Dimension of test=",dim(test),"/nl"))}    ##  for watching details
# next step is to combine the columns together for the train data
  train <- cbind(trainsubject,trainy,trainx)
  if (debug) {print(paste0("Dimension of train=",dim(train),"/nl"))}    ##  for watching details
# finally we concatonate the train data to the test data
  mergedset <- rbind(train,test)
  if (debug) {print(paste0("Dimension of mergedset=",dim(mergedset),"/nl"))} ##  for watching details

# the Forum discussions led me to ignore the 7 mean values associated with angles
# these values all contained Mean (capital M) in their names, to include those into the 
# list of columns analyzed, simply modify the value grepped for in the next line to "[Mm]ean"
  keeplabels <- c(grep("mean",features[,2]),grep("std",features[,2]))  # features we are intested in
  keepcolumns <- c(1,2,keeplabels+2)  # the subject and activity are not in the features file
  if (debug) {
    print(paste0("# labels retained from features file=",length(keeplabels),
                 ", # of columns=",length(keepcolumns)))}

# get rid of unneeded columns (only keep columns with mean or std in their name)
  mergedset <- mergedset[,keepcolumns]

# put in the column names
  names(mergedset) <-c("subject", "activity", as.character(features[keeplabels,2]))

# add descriptive names for the activities
  mergedset$activity <- activitylabels[mergedset$activity,2]

# "standardize" special characters from column names and set to lower case 
# not sure I completely agree with all the "rules" but I tried to comply with recommendations  
  remove <- function(x,y) { gsub(x,"",y) }  
  names(mergedset) <- remove("\\(",names(mergedset))
  names(mergedset) <- remove("\\)",names(mergedset))
  names(mergedset) <- remove("\\-",names(mergedset))
  names(mergedset) <- remove(",",names(mergedset))
  names(mergedset) <- tolower(names(mergedset))

# show the "standardized" names if in debug mode to watch the details of the process
if (debug) { print(names(mergedset)) }
  
  if (viewtables) {View(mergedset)}  ## to make it easier to validate results

  widegroupmeans <- mergedset %>% group_by(subject,activity) %>% 
    ddply(.(subject,activity), colwise(mean))

  if (viewtables & wide) {View(widegroupmeans)}  ## this is the tidy file which gets uploaded
  
  if (long) {
    longgroupmeans <- melt(widegroupmeans, id=c("subject","activity")) %>% arrange(subject,activity,variable)
    names(longgroupmeans)[3:4] <- c("measure","mean")
    if (viewtables & long) {View(longgroupmeans)}
  }

##  Here is where we write out the table(s)

  if (wide) {
#    write.csv(widegroupmeans,file="./widetidydataset",row.names=FALSE)
    write.table(widegroupmeans,file="./widetidydataset",row.names=FALSE)
    print("A wide version of the tidy data set has been written to './widetidydataset'")
  }
  if (long) {
#    write.csv(longgroupmeans,file="./longtidydataset",row.names=FALSE)
    write.table(longgroupmeans,file="./longtidydataset",row.names=FALSE)
    print("A long version of the tidy data set has been written to './longtidydataset'")
  }  
  cat("\nTo read these files back in use the command")
  cat("\n    read.table(\"./xxxtidydataset\",header=TRUE)\n")
  cat("          where xxxx is wide or long depending on the requested output\n")
  
  print("run_analysis script has completed")        
        