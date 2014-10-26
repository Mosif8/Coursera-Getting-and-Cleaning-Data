
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 


#Download and Unzip raw data
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile = "Dataset.zip", )
unzip("Dataset.zip")

#Read data from files for X_test, y_test and subject_test files.
xtest<- read.table(".\\UCI HAR Dataset\\test\\X_test.txt", stringsAsFactors=FALSE, header=FALSE)
ytest<- read.table(".\\UCI HAR Dataset\\test\\y_test.txt", stringsAsFactors=FALSE, header=FALSE)
subtest<- read.table(".\\UCI HAR Dataset\\test\\subject_test.txt", stringsAsFactors=FALSE, header=FALSE)

#Combine data from the above tables adding descriptive labels to the "Activity" and "Subject" columns.
data1<- cbind(xtest, Activity=ytest$V1, Subject=subtest$V1)

#Read data from files for X_train, y_train and subject_train files.
xtrain<- read.table(".\\UCI HAR Dataset\\train\\X_train.txt", stringsAsFactors=FALSE, header=FALSE)
ytrain<- read.table(".\\UCI HAR Dataset\\train\\y_train.txt", stringsAsFactors=FALSE, header=FALSE)
subtrain<- read.table(".\\UCI HAR Dataset\\train\\subject_train.txt", stringsAsFactors=FALSE, header=FALSE)

#Combine data from the above tables adding descriptive 
#labels to the "Activity" and "Subject" columns.
data2<- cbind(xtrain, Activity=ytrain$V1, Subject=subtrain$V1)

#Combine the data1 and Data2 files adding labels 
#to the remaining columns from the Features file 
mergedat<- rbind(data1, data2)
features<- read.table(".\\UCI HAR Dataset\\features.txt", 
                      stringsAsFactors=FALSE, header=FALSE)
alldata<- colnames(mergedat)[1:561]<- features$V2

#Select the columns for mean and std in addition 
#to Activity and Subject while deleting "meanFreq()" columns.
selectCols<- grep("mean()|std()|Activity|Subject" , colnames(mergedat))
keepCols<- mergedat[selectCols]
delCols<- grep("meanFreq()", colnames(keepCols))
cleanCols<- keepCols[-c(delCols)]

#Change Activity names from numbers to descriptions. 
cleanCols$Activity[cleanCols$Activity == 1] <- "WALKING"
cleanCols$Activity[cleanCols$Activity == 2] <- "WALKING UPSTAIRS"
cleanCols$Activity[cleanCols$Activity == 3] <- "WALKING DOWNSTAIRS"
cleanCols$Activity[cleanCols$Activity == 4] <- "SITTING"
cleanCols$Activity[cleanCols$Activity == 5] <- "STANDING"
cleanCols$Activity[cleanCols$Activity == 6] <- "LAYING"

#Clean up column labels 
names(cleanCols) <- gsub("^t", "Time", names(cleanCols))
names(cleanCols) <- gsub("^f", "Frequency", names(cleanCols))
names(cleanCols) <- gsub("-mean\\(\\)", "Mean", names(cleanCols))
names(cleanCols) <- gsub("-std\\(\\)", "StdDev", names(cleanCols))
names(cleanCols) <- gsub("-", "", names(cleanCols))
names(cleanCols) <- gsub("BodyBody", "Body", names(cleanCols))


#Create independent tidy data set with the average 
#of each variable for each activity and each subject. 
tidydata<- ddply(melt(cleanCols, id.vars=c("Subject", "Activity")), 
                 .(Subject, Activity), summarise, MeanSamples=mean(value))

#view tidy data set. 
tidydata
