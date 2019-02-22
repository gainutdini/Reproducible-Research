## Assignment
#The basic goal of this assignment is to explore the NOAA Storm Database and answer some basic questions about severe weather events.


## 1.1 Download files into your working directory  
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", "data.csv.bz2")
# 1.2 Read and save the file into data dataset
data <- read.csv("data.csv.bz2",header = T,sep = ",", dec = ".")
head(data)

## 2 Questions:

# 2.1 Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?
# Lets calculate the sum of Facilities and Injures of Events
# Let's subset our dataset. Save only needed variables
df <- data[c("STATE", "EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]
head(data)

# 2.2 Sum the Fatalities and Injuries for each Event and save it in new column

df$Harmful <- df$FATALITIES + df$INJURIES

# 2.3 Find the max of Harmful
max <- max(df$Harmful)
max

# 2.4 Find the most harmful Event

df[df$Harmful == max, "EVTYPE"]

# Lets save it
most_harmful_event <- df[df$Harmful == max, "EVTYPE"]
cat(as.character(most_harmful_event))

# 3 Across the United States, which types of events have the greatest economic consequences?
# We must find the greatest damage. In our dataset we have indices of DMG like "K", "M" and "B".

# 3.1 Let's subset the Property Damage Estimate by "B" that meens billions of dollars.

PROP_B <- df$PROPDMG[which(df$PROPDMGEXP=="B")]
head(PROP_B)
# 3.2 Find the max for the Property Damage Estimate 
PROP_max <- max(PROP_B)
PROP_max

# 3.3 Let's subset the Crop Damage Estimate by "B" that meens billions of dollars.

CROP_B <- df$CROPDMG[which(df$CROPDMGEXP=="B")]
head(CROP_B)

# 3.4 Find the max for the Property Damage Estimate 
CROP_max <- max(CROP_B)
CROP_max

# We can clearly see that the property damage is 115 billion dollar, which is the the largest amount.

#The type of event which creates the greatest economic consequences:

Cons <- df$EVTYPE[which(df$PROPDMG == PROP_max & df$PROPDMGEXP == "B")]
cat(as.character(Cons))


## Plotting
#Number of injuries and fatalities caused by Storm

tornado <- df[which(df$EVTYPE == "TORNADO"),]
par(mfrow=c(1,2))
boxplot(tornado$INJURIES, main ="Injuries caused by Tornado"
        ,xlab="Storm", ylab="Number of Injuries")
boxplot(tornado$FATALITIES, main ="Fatalities caused by Tornado"
        ,xlab="Storm", ylab="Number of Injuries")

#The Property Damage caused by Flood

boxplot(df$PROPDMG[df$EVTYPE=="FLOOD"]
        , main ="Property Damage caused by Flood"
        ,xlab="FLOOD", ylab="Billions of Dollar")

##Results
# Question 1: Across the United States, which types of events have the greatest economic consequences?
most_harmful_event <- df[df$Harmful == max, "EVTYPE"]
most_harmful_event
# It is TORNADO.

# Question 2: Across the United States, which types of events have the greatest economic consequences?
Cons <- df$EVTYPE[which(df$PROPDMG == PROP_max & df$PROPDMGEXP == "B")]
Cons
# It is FLOOD.









