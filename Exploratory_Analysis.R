library(ggplot2)

mydata <- read.csv("C://Users//gramos1//Desktop//Football_Data//myData.csv")
str(mydata)

mydata$goal=as.factor(mydata$goal)
mydata$counter=as.factor(mydata$counter)
mydata$bodypartused=as.factor(mydata$bodypartused)

mean_shotangle <- mean(mydata$shotangle)
mean_shotangle

mean_dist <- mean(mydata$distance_to_goal)
mean_dist

## Of 43067 shots observed in the data set, 4487 are goals. (10.42%)
summary(mydata$goal)
summary(mydata)

## Goals scored (on average) from a slightly wider angle in comparison to non-goal resulting shots.
shotangle_viz <- ggplot(mydata, aes(x=shotangle, color=goal)) +
  geom_density() + labs(x="Shot Angle")
shotangle_viz

## Goals scored (on average) from a slightly lesser distance in comparison to non-goal resulting shots.
distance_viz <- ggplot(mydata, aes(x=distance_to_goal, color=goal)) +
  geom_density() + labs(x="Distance to goal (meters)")
distance_viz

##  15.11% of shots taken on the counter attack result in goals, compared to only 10.14% of non-counter attack shots resulting in goals.
counter_viz <- ggplot(mydata, aes(x=counter, fill=goal)) +
  geom_bar(position="stack") +
  geom_text(aes(label = ..count..), stat = "count", vjust=-0.2, colour = "black")
counter_viz

## No noticeable correlation between the minute in a half and a shot being taken, or a goal being scored.
timeofshot_viz <- ggplot(mydata, aes(x=round((eventSec/60), 0), color=goal)) +
  geom_density() + labs(x="Minute of half") + scale_x_continuous(limits = c(0, 45))
timeofshot_viz

## 10.27% of left foot shots result in goals. 9.85% of right foot shots result in goals. 12.50% of headed shots result in goals.
bodypart_viz <- ggplot(mydata, aes(x=bodypartused, fill=goal)) +
  geom_bar(position = "stack") +
  geom_text(aes(label = ..count..), stat = "count", colour = "black") + 
  geom_text(x=3, y=18000, label="401 = left foot") + 
  geom_text(x=3, y=17000, label="402 = right foot") +
  geom_text(x=3, y=16000, label="403 = header") +
  labs(x="Body part used")
bodypart_viz
