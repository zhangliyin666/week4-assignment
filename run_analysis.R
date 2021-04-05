#Import labels
activity_labels <- read.table("activity_labels.txt",col.names =  c("activityid","activitytype"))

#Import features
features <- read.table("features.txt",col.names =  c("featureid","featuretype"))

#Import test data
x_test <- read.table("./test/X_test.txt",col.names = features[,2])
y_test <- read.table("./test/y_test.txt",col.names = "activityid")
subject_test <- read.table("./test/subject_test.txt",col.names = "subjectid")

#Import train data
x_train <- read.table("./train/X_train.txt",col.names = features[,2])
y_train <- read.table("./train/y_train.txt",col.names = "activityid")
subject_train <- read.table("./train/subject_train.txt",col.names = "subjectid")

#1.Merges the training and the test sets to create one data set.
x_all <- rbind(x_test,x_train)
y_all <- rbind(y_test,y_train)
sub_all <- rbind(subject_test,subject_train)
data_all <- cbind(x_all,sub_all,y_all)

#2.Extracts only the measurements on the mean and standard deviation for each measurement. 
toselect <- features[grep("mean\\(\\)|std\\(\\)",features[,2],ignore.case = FALSE),]
data_mean <- data_all[,toselect[,1]]

#3.Uses descriptive activity names to name the activities in the data set
data_mean$activityname <- factor(y_all$activityid, labels = as.character(activity_labels[,2]))

#4.Appropriately labels the data set with descriptive variable names. 
x_select <- toselect$featuretype
colnames(data_mean) <- c(x_select,"activityname")
write.table(data_mean,file = "mean&std.txt",col.names = T,row.names = T)

#5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
newdata <- cbind(data_mean,sub_all)
newdata_mean <- newdata %>%
  group_by(activityname,subjectid) %>%
  summarize_each(funs(mean))
write.table(newdata_mean,file = "tidydata.txt",col.names = T,row.names = T)
