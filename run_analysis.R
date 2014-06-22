###########In this TDs I will create 2 tiny data set (question 2 to 4 : data_Q2) and the final independant tiny data set (question 5 : agg_data)###########

#### First step ####
##Load Test file using read.table function##

X_test <- read.table("./test/X_test.txt", header = FALSE, fill = TRUE, dec=".")

y_test <- read.table("./test/y_test.txt", header = FALSE, fill = TRUE, colClasses = "character")

subject_test <- read.table("./test/subject_test.txt", header = FALSE, fill = TRUE, colClasses = "factor")

##Load Train file using read.table function (same method) ##
	
X_train <- read.table("./train/X_train.txt", header = FALSE, fill = TRUE, dec=".")

y_train <- read.table("./train/y_train.txt", header = FALSE, fill = TRUE, colClasses = "character")

subject_train <- read.table("./train/subject_train.txt", header = FALSE, fill = TRUE, colClasses = "factor")

##Load features file for future use##

features <- read.table("./features.txt", fill = TRUE, dec=".",header = FALSE, col.names=c("index","feature"), colClasses=c("factor","character"))

####Q1 : Merge train data using cbind and rbind####

##Column bind separately test and train data first##

test_data <- cbind(subject_test,y_test,X_test)
train_data <- cbind (subject_train,y_train,X_train)

##row bind together test and train data (they have the same "structure") in "data" dataframe##

data <- rbind (test_data,train_data)

####Q2 : (first tiny data set) Extracts only the measurements on the mean and standard deviation for each measurement####

##grep in feature file where data like "std" or "mean"##

std_and_mean_col_ID <- c(grep("std",features$feat),grep("mean",features$feat))

##sort this data using sort() function##

std_and_mean_col_ID <- sort (std_and_mean_col_ID)

##Create the required extract (adding +2 because the 2 first columns are used for subject and activity)##

data_Q2 <- data[,c(1,2,std_and_mean_col_ID+2)]

####Q3 : Uses descriptive activity names to name the activities in the data set####

transco_activity <- function (input) {
	
	input[input==1] <- "WALKING"
	input[input==2] <- "WALKING_UPSTAIRS"
	input[input==3] <- "WALKING_DOWNSTAIRS"
	input[input==4] <- "SITTING"
	input[input==5] <- "STANDING"
	input[input==6] <- "LAYING"
 	
	return (input)

}

data_Q2[,2] <- transco_activity(data_Q2[,2])

####Q4 : Appropriately labels the data set with descriptive variable names####

colnames(data_Q2)[1] <- "subject_id"
colnames(data_Q2)[2] <- "activity"

colnames(data_Q2)[c(-1,-2)] <- features$feat[std_and_mean_col_ID]

####Q5 : Creates a second, independent tidy data set with the average of each variable for each activity and each subject. ####

agg_data <-aggregate(data[,c(-1,-2)], by=list(data[,2],data[,1]), FUN=mean, na.rm=TRUE)

##Then, to get a proper dataframe I rename the activity and variables as previously##

agg_data[,2] <- transco_activity(agg_data[,2])

colnames(agg_data)[1] <- "subject_id"
colnames(agg_data)[2] <- "activity"
colnames(agg_data)[c(-1,-2)] <- features$feat

