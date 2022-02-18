##1. Merges the training and the test sets to create one data set.

#Reading all of the text files and place into tables
Xtrain <- read.table("~/work-rfiles/UCI HAR Dataset/train/X_train.txt", header = FALSE)
ytrain <- read.table("~/work-rfiles/UCI HAR Dataset/train/y_train.txt", header = FALSE)
subjecttrain <- read.table("~/work-rfiles/UCI HAR Dataset/train/subject_train.txt", header = FALSE)
Xtest <- read.table("~/work-rfiles/UCI HAR Dataset/test/X_test.txt", header = FALSE)
ytest <- read.table("~/work-rfiles/UCI HAR Dataset/test/y_test.txt", header = FALSE)
subjecttest <- read.table("~/work-rfiles/UCI HAR Dataset/test/subject_test.txt", header = FALSE)
features <- read.table("~/work-rfiles/UCI HAR Dataset/features.txt", header = FALSE)
activitylabels <- read.table("~/work-rfiles/UCI HAR Dataset/activity_labels.txt", header = FALSE)

#==========================================================================================================================
##4. Appropriately labels the data set with descriptive variable names. Easier to implement this here than after merging.

colnames(Xtrain) <- features[,c(2)]
colnames(ytrain) <- "activityNumber"
colnames(subjecttrain) <- "subjecttrainID"

colnames(Xtest) <- features[,c(2)]
colnames(ytest) <- "activityNumber"
colnames(subjecttest) <- "subjecttrainID"

colnames(activitylabels) <- c("activityNumber", "activityName")

#==========================================================================================================================
##1. Merges the training and the test sets to create one data set.

#Merge into one data set by using cbind and rbind
bindtrain <- cbind(ytrain, subjecttrain, Xtrain)
bindtest <- cbind(ytest, subjecttest, Xtest)
mergeddata <- rbind(bindtrain, bindtest)
head(mergeddata)

#==========================================================================================================================
##2. Extracts only the measurements on the mean and standard deviation for each measurement.

#get the column names of the merged data set
getNames <- colnames(mergeddata)

#Extract measurements on the mean and standard dev by creating a vector and only getting the value if it returns TRUE
meansd <- mergeddata[, ((grepl("activityNumber", getNames) | grepl("subjecttrainID", getNames) | grepl("mean..", getNames) | grepl("std..", getNames))) == TRUE]
#==========================================================================================================================
##3. Uses descriptive activity names to name the activities in the data set

descriptivenames = merge(meansd, activitylabels, by='activityNumber', all.x=TRUE)
descriptivenames

#==========================================================================================================================
##4 was implemented above for simplicity

#==========================================================================================================================
##5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
sapply(meansd,as.numeric) #use sapply to convert the data frame to numeric in order to apply the aggregate in the next line
secondSet <- aggregate(.~subjecttrainID + activityNumber, meansd, mean) #get the mean of the columns
secondSet
write.table(secondSet, "secondSet.txt", row.name=FALSE)
