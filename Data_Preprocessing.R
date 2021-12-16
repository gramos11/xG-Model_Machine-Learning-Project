##Setting work directory##
setwd("C:/Users/gramos1/Desktop/Football_Data")

##Reading in the 7 different datasets unique to each league##
##Were originally JSON files loaded into EXCEL query##
england_events <- read_xlsx("EXCEL_Events_England.xlsx")
france_events <- read_xlsx("EXCEL_Events_France.xlsx")
germany_events <- read_xlsx("EXCEL_Events_Germany.xlsx")
italy_events <- read_xlsx("EXCEL_Events_Italy.xlsx")
spain_events <- read_xlsx("EXCEL_Events_Spain.xlsx")
euros_events <- read_xlsx("EXCEL_Events_Euros.xlsx")
wc_events <- read_xlsx("EXCEL_Events_WC.xlsx")

##Combining all datasets into one##
all_events <- rbind(england_events, france_events, germany_events, italy_events, spain_events, euros_events, wc_events)

##Removing all rows with y coordinate of 0##
all_events <- subset(all_events, y!=0 & y!=100)

##Removing all rows with x coordinate of 100##
all_events <- subset(all_events, x!=0 & x!=100)

##Checking for missing values and then removing those rows##
anyNA(all_events)
apply(all_events,2,anyNA)
which(is.na(all_events$subEventId))
all_events <- na.omit(all_events)

##Removing eventId and subeventId columns since they are the same values across all observations and will not aid in model building process##
all_events <- all_events[-c(1, 10)]

##Identifying the id numbers that show up multiple times in our dataset and seperating them into a seperate dataframe##
duplicates <-  all_events[duplicated(all_events$id),,drop=FALSE]
all_events <-  all_events[!duplicated(all_events$id),,drop=FALSE]

##Identifying the id numbers that again show up multiple times in the seperated dataframe and seperating those observations into a second duplicates dataframe##
duplicates2 <-  duplicates[duplicated(duplicates$id),,drop=FALSE]
duplicates <-  duplicates[!duplicated(duplicates$id),,drop=FALSE]

##Removing all of the counter attacks in the duplicates dataframe because they have already been accounted for in the duplicates2 dataframe##
duplicates <- duplicates[!(duplicates$id.1==1901),]

##Creating our counter attack variable by first creating a vector of 1's the same length as our duplicates2 dataframe because we know all of those observations are counter attacks##
counter <- c(rep(1, 363))

##Combining the counter vector we just created with the duplicates2 dataframe##
duplicates2 <- cbind(duplicates2, counter)

##Creating a vector for a goal variable of all 1's for our dataframe duplicates2 because we know not only are all of those observations counter attacks but also goals##
goal <- c(rep(1, 363))

##Combining the goal scored vector with the duplicates2 dataframe##
duplicates2 <- cbind(duplicates2, goal)

##Since duplicates2 has all of the counter attack goals we know the original dataset has all of the counter attacks that were not goals##
##So, we moved all of the counter attacks in the original dataset to a new dataframe called counter_notgoal##
counter_notgoal <- all_events[(all_events$id.1==1901),]

##We made a vector of 1's for the counter_notgoal dataframe to signify that all of the observations were counter attacks##
counter <- c(rep(1, 2040))

##We combined the counter vector with the counter_notgoal dataframe##
counter_notgoal <- cbind(counter_notgoal, counter)

##Since all of the counter attacks which resulted in a goal are in the duplicates2 dataframe, we know none of the observations in counter_notgoal resulted in goals##
##So, we make a vector of 0's for counter_notgoal to signify that the shots did not result in a goal##
goal <- c(rep(0, 2040))

##We combine the goal vector with the counter_notgoal dataframe##
counter_notgoal <- cbind(counter_notgoal, goal)

##We finally remove all of the counter attacks from the original dataset because we have them all accounted for in duplicates2 and counter_notgoal##
all_events <- all_events[!(all_events$id.1==1901),]

##Merging the counter_notgoal dataframe and the duplicates dataframes so that we have our new variables 'counter' and 'goal' included with the rest of the known goals in the dataset##
counters_goals <- merge(counter_notgoal, duplicates, by = "id", all.y = TRUE)

##Removing the duplicated columns that appeared following the merge##
counters_goals <- counters_goals[-c(2:9)]

##For all the missing values for the counter column we replaced them with a 0 because we know these observations coming from the duplicates dataframe are not counter attacks##
counters_goals[, 2][is.na(counters_goals[, 2])] <- 0

##For all the missing values for the goal column we replaced them with a 1 because we know these observations coming from the duplicates dataframe are all goals scored##
counters_goals[, 3][is.na(counters_goals[, 3])] <- 1

##Double checking for any other missing values that may have arisen by mistake##
anyNA(counters_goals)

##Renaming the column names in the newly merged dataframe##
colnames(counters_goals)[4] <- "id.1"
colnames(counters_goals)[5] <- "playerId"
colnames(counters_goals)[6] <- "y"
colnames(counters_goals)[7] <- "x"
colnames(counters_goals)[8] <- "matchId"
colnames(counters_goals)[9] <- "teamId"
colnames(counters_goals)[10] <- "matchPeriod"
colnames(counters_goals)[11] <- "eventSec"

##Combining the counters_goals and duplicates2 dataframes so that all of the known counter attacks and goals scored are all in one dataframe called counters_goals##
counters_goals <- rbind(counters_goals, duplicates2)

##Removing all of the rows in the original dataset with an id number of 101 because we already have these observations accounted for in our dataframe called counters_goals##
all_events <- all_events[!(all_events$id.1==101),]

##Creating a vector of 0's called counter in order to add this variable to the original dataset##
counter <- c(rep(0, 36540))

##Combining the vector we just created with the original dataset##
##Because we know all of the counter attacks are already seperated into the counters_goals dataframe, we know that the observations remaining in the original dataset are not counter attacks##
all_events <- cbind(all_events, counter)

##Creating a vector of 0's called goal in order to add this variable to the original dataset##
goal <- c(rep(0, 36540))

##Combining the vector we just created with the original dataset##
##Because we know all of the goals are already seperated into the counters_goals dataframe, we know that the observations remaining in the original dataset are not goals##
all_events <- cbind(all_events, goal)

##Combining counters_goals and original dataset to create final clean dataframe##
myData <- rbind(all_events, counters_goals)

##Creating a vector object transforming the x location to a distance in terms length##
##Since the x value is a percentage, we multiplied this value by the average field length and then divided by 100 to get a distance in yards##
xyards <- myData$x * 120/100

##Creating a vector object transforming the y location to a distance in terms of width##
##Since the y value is a percentage, we multiplied this value by the average field width and then divided by 100 to get a distance in yards##
yyards <- myData$y * 80/100

##Creating our shotangle variable using the vectors we just made as well as inverse trigonometric functions##
shotangle <- atan( (8 * (120 - xyards) ) / ( (120 - xyards)^2 + (40 - yyards)^2 - (8/2)^2 )) * 180/pi

##Adding shot angle variable to clean dataframe##
myData <- cbind(myData, shotangle)

##Creating distance to goal variable using euclidean distance formula##
distance_to_goal <- sqrt((120 - xyards)^2 + (40 - yyards)^2)

##Adding distance to goal variable to clean dataframe##
myData <- cbind(myData, distance_to_goal)

##replace "1H" with 0 and "2H" with 1 in matchPeriod column for simplicity purposes##
myData$matchPeriod = ifelse(myData$matchPeriod == '1H',0,1) 

##Checking structure of data##
str(myData)

##Changing id.1, matchPeriod, counter, and goal variables to factors
myData[c(1, 7, 10, 11)] = lapply(myData[c(1, 7, 10, 11)], factor)

##Renaming a couple confusing column names##
colnames(myData)[1] <- "bodypartused"
colnames(myData)[9] <- "eventId"

summary(myData)

##Saving dataframe as a .csv file to share with group members##
write.csv(myData,"C://Users//gramos1//Desktop//Football_Data//myData.csv", row.names = FALSE)
