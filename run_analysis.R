# This script does the following
# 1.  Merges the training and the test sets to create one data set.
# 2.  Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3.  Uses descriptive activity names to name the activities in the data set
# 4   Appropriately labels the data set with descriptive activity names. 
# 5.	Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

# Reshape2 lets you flexibly restructure and
# aggregate data using just two functions: melt and cast.
library(reshape2)

# local directory where the files are
dataDir <- "UCI HAR Dataset"

# UCI dataset has X values, Y values and Subject IDs.

# call GetData function for reading test data  
data.test <-GetData("test", "test")

# Read training data sets

data.train <-GetData("train", "train")

# Merges the training and the test sets to create one data set
data.merge <- rbind(data.test, data.train)
cn <- colnames(data.merge)
cn <- gsub("\\.+mean\\.+", cn, replacement="Mean")
cn <- gsub("\\.+std\\.+", cn, replacement="Std")
colnames(data.merge) <- cn

# Combine training test data sets and add "activity" label as another column
ruta <- paste0(dataDir, "/activity_labels",".txt")
ruta <- file.path(ruta)
name.activities <- read.table(ruta, header=FALSE, as.is=TRUE,
                              col.names=c("idActivity", "activityName"))
name.activities$activityName <- as.factor(name.activities$activityName)
data2.merge<- merge(data.merge, name.activities)

# Create a second, independent tidy data set with the average of each variable 
# for each activity and each subject.   

ids<- c("idActivity", "activityName", "idSubject")
measures<- setdiff(colnames(data2.merge), ids)
data.global<- melt(poem_nome, id=ides, measure.vars=measures)                     

data.global<-dcast(data.global, activityName + idSubject ~ variable, mean)  

ruta<-paste0(dataDir, "/jpa_tidy_data",".txt")
write.table(data.global, ruta)

# Function GetData has two parameters: subdirectory name: where the files are; 
# and name of file ("test" or "train")

GetData <- function (subdir, namefile) {
        
        # reading the Y file
        ruta <- paste0(dataDir, "/", subdir, "/y_", namefile, ".txt") 
        ruta <- file.path(ruta)
        data.y <- read.table(ruta, header=FALSE, col.names=c("idActivity"))
        
        # reading the subject ids file
        ruta <- paste0(dataDir, "/", subdir, "/subject_", namefile, ".txt")
        ruta <- file.path(ruta)
        data.subject <- read.table(ruta, header=FALSE, col.names=c("idSubject"))
        
        # reading features.txt file. 
        ruta<-paste0(dataDir, "/features.txt") 
        ruta<-file.path(ruta)
        data.features <- read.table(ruta, header=FALSE, as.is=TRUE, 
                                    col.names=c("idMeasure","measureName"))
        
        # reading the X files
        
        ruta <- paste0(dataDir, "/", subdir, "/X_", namefile, ".txt") 
        ruta <- file.path(ruta)    
        data.X <- read.table(ruta, header=FALSE, col.names=data.features$measureName)
        
        # getting required columns names
        name_columns <- grep(".*mean\\(\\)|.*std\\(\\)", data.features$measureName)
        
        # subset the data (done early to save memory)
        data.X <- data.X[,name_columns]
        
        # append the activity id and subject id columns
        data.X$idActivity <- data.y$idActivity
        data.X$idSubject  <- data.subject$idSubject
        
        # return data.X dataframe
        data.X  
}