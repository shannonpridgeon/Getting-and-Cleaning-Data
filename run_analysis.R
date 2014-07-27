##joining the subject, activity and data in that order for training
trains <- read.table("~/R/UCI HAR Dataset/train/subject_train.txt", quote="\"", stringsAsFactors=FALSE)
trainy <- read.table("~/R/UCI HAR Dataset/train/y_train.txt", quote="\"", stringsAsFactors=FALSE)
trainx <- read.table("~/R/UCI HAR Dataset/train/x_train.txt", quote="\"", stringsAsFactors=FALSE)
train <- cbind(trains,trainy,trainx,stringsAsFactors = FALSE)
train <- cbind(origin = "train", train,stringsAsFactors = FALSE)
##joining the subject, activity and data in that order for testing
tests <- read.table("~/R/UCI HAR Dataset/test/subject_test.txt", quote="\"", stringsAsFactors=FALSE)
testy <- read.table("~/R/UCI HAR Dataset/test/y_test.txt", quote="\"", stringsAsFactors=FALSE)
testx <- read.table("~/R/UCI HAR Dataset/test/x_test.txt", quote="\"", stringsAsFactors=FALSE)
test <- cbind(tests,testy,testx,stringsAsFactors = FALSE)
test <- cbind(origin = "test", test,stringsAsFactors = FALSE)
##adding the column names
features <- read.table("~/R/UCI HAR Dataset/features.txt", 
                     quote="\"", stringsAsFactors=FALSE)
data <- rbind(train,test,stringsAsFactors = FALSE)
names(data) <- c("origin","subject","activity",features$V2)
##extracting just means and standard deviations
grep1 <- grep("std",names(data))
grep2 <- grep("-mean",names(data))
newdata <- data[ ,c(1:3,grep1,grep2)]  ##still have meanFreq
grep3 <- grep("meanFreq",names(newdata))
badnames <- names(newdata[ ,grep3])
newerdata <- newdata[ ,which(names(newdata) %in% badnames == FALSE)]
##making the column names tidy
names(newerdata) <- tolower(names(newerdata))
names(newerdata) <- gsub("\\(|\\)", "", names(newerdata))
names(newerdata) <- gsub("-", "", names(newerdata))
names(newerdata) <- gsub("^f", "freq", names(newerdata))
names(newerdata) <- gsub("^t", "time", names(newerdata))
names(newerdata) <- gsub("bodybody","body", names(newerdata))
##newerdata is now tidy. 
##getting the average of each variable for each activity and each subject
newerdata <- newerdata[ ,2:69]  #looks like we won't need the origin column
newerdata$activity <- as.numeric(newerdata$activity)
newerdata$subject <- as.numeric(newerdata$subject)
aggdata <-aggregate(newerdata, by=list(newerdata$subject,newerdata$activity),
                    FUN=mean, na.rm=TRUE, na.action = na.omit)
aggdata <- aggdata[2:181,3:70] #removing group numbers and a top row of 0's
##replace the numbers with activities
##aggdata is tidy with dimension 180 X 68
aggdata[ ,2] <- c(rep("walking",times = 30),rep("walking up",times = 30),
                  rep("walking down",times = 30),rep("sitting",times = 30),
                  rep("standing",times = 30),rep("laying",times = 30))
write.csv(aggdata, file = "aggdata.csv")




