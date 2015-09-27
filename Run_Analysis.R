##Merges the training and the test sets to create one data set.
setwd('/Users/Nana/UCI HAR Dataset/')
##Combine Training Data Files
Feature = read.table('./features.txt',header=FALSE);
Activity_Type = read.table('./activity_labels.txt', header=FALSE);
Subject_Train = read.table('./train/subject_train.txt', header=FALSE);
xTrain = read.table('./train/x_train.txt', header=FALSE);
yTrain = read.table('./train/y_train.txt', header=FALSE);

colnames(Activity_Type) = c("activityID", "activityTYPE");
colnames(Subject_Train) = "subjectID";
colnames(xTrain)=Feature[,2];
colnames(yTrain)="activityID";
trainingData = cbind(yTrain,Subject_Train,xTrain);

##Combine Testing Data Files
Subject_Test = read.table('./test/subject_test.txt', header=FALSE);
xTest = read.table('./test/x_test.txt', header=FALSE);
yTest = read.table('./test/y_test.txt', header=FALSE);
colnames(Subject_Test) = "subjectID";
colnames(xTest)=Feature[,2];
colnames(yTest)="activityID";
testingData = cbind(yTest,Subject_Test,xTest);

##Combine Training and Testing data
FinalData=rbind(trainingData, testingData);
colNames=colnames(FinalData);

##Extracts only the measurements on the mean and standard deviation for each measurement. 
logicalVector = (grepl("activity..",colNames) |
                   grepl("subject..",colNames) | 
                   grepl("-mean..",colNames) & 
                   !grepl("-meanFreq..",colNames) & 
                   !grepl("mean..-",colNames) |
                   grepl("-std..",colNames) &
                   !grepl("-std()..-",colNames));
FinalData = FinalData[logicalVector==TRUE];

##Uses descriptive activity names to name the activities in the data set
FinalData = merge(FinalData,Activity_Type,by='activityID',all.x=TRUE);
colNames  = colnames(FinalData);

##Appropriately labels the data set with descriptive variable names. 

for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};
colnames(FinalData) = colNames;
##From the data set in step 4, creates a second, independent tidy data set with 
##the average of each variable for each activity and each subject.
FinalDataNoActivityType  = FinalData[,names(FinalData) != 'activityTYPE'];
tidyData = aggregate(FinalDataNoActivityType[,names(FinalDataNoActivityType) != c('activityID','subjectID')],by=list(activityID=FinalDataNoActivityType$activityID,subjectID = FinalDataNoActivityType$subjectID),mean);
tidyData = merge(tidyData,Activity_Type,by='activityID',all.x=TRUE);
write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t');
