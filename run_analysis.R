
# Read features as table for later column naming

featuresAll <- read.table("features.txt", stringsAsFactors=FALSE)


# Read data as tables and combine them

x_test <- read.table("test/X_test.txt")

x_train <- read.table("train/X_train.txt")

x_test_train <- rbind(x_test, x_train)


# Read subjects as tables and combine them

subject_test <- read.table("test/subject_test.txt")

subject_train <- read.table("train/subject_train.txt")

subject_test_train <- rbind(subject_test, subject_train)


# Read data labels as tables and combine them

y_test <- read.table("test/y_test.txt")

y_train <- read.table("train/y_train.txt")

y_test_train <- rbind(y_test, y_train)


# column with label names

features <- featuresAll$V2


# Logical Vector to filter out only std and mean columns

stdAndMean <- grepl("(std|mean[^F])", features, perl=TRUE)


x_test_train <- x_test_train[, stdAndMean]

names(x_test_train) <- features[stdAndMean]

names(x_test_train) <- gsub("\\(|\\)", "", names(x_test_train))

names(x_test_train) <- tolower(names(x_test_train))


# Read activity labels

activity_labels <- read.table("activity_labels.txt")

activity_labels[,2] = gsub("_", "", tolower(as.character(activity_labels[,2])))

y_test_train[,1] = activity_labels[y_test_train[,1], 2]

names(y_test_train) <- "activity" # Add activity label


names(subject_test_train) <- "subject"


tidyData <- cbind(subject_test_train, y_test_train, x_test_train)

write.table(tidyData, "tidyData.txt")


# Create data set with avg of each variable

uS = unique(subject_test_train)[,1]

nS = length(uS)

nA = length(activity_labels[,1])

nC = length(names(tidyData))

td = tidyData[ 1:(nS*nA), ]


row = 1

for (i in 1:nS) {
        
        
        for (j in 1:nA) {
                
                
                
                td[row,1] = uS[i]
                
                
                
                td[row,2] = activity_labels[j, 2]
                
                
                
                tmp <- tidyData[tidyData$subject==i & tidyData$activity==activity_labels[j,2],]
                
                
                
                td[row, 3:nC] <- colMeans(tmp[, 3:nC])
                
                
                
                row = row + 1
                
                
                
        }
        
        
        
}


write.table(td, "tidyData2.txt")