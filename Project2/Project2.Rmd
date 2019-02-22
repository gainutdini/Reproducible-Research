---
title: "Project2"
author: "NiazG"
date: '22 February 2019 y '
output: html_document
---
## Sysnopis
This is an R Markdown document which is used to analyze the [Storm Data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2) based on the [National Weather Service Storm Data](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf). The document is focus in answering the two question which are:

    1. Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?

    2. Across the United States, which types of events have the greatest economic consequences? 

The basic goal of this document is to explore the NOAA Storm Database and understand some basic concepts about weather events.

## Data Processing

Download files into your working directory  
``` {r, echo = TRUE}
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", "data.csv.bz2")
```
Read and save the file into data dataset
``` {r, echo = TRUE}
data <- read.csv("data.csv.bz2",header = T,sep = ",", dec = ".")
```
## Question 1: Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?

Find the most harmful with respect to population health by FALITIES and INJURIES variable
``` {r, echo = TRUE}
df <- data[c("STATE", "EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]
```

Sum the Fatalities and Injuries for each Event and save it in new column
``` {r, echo = TRUE}
df$Harmful <- df$FATALITIES + df$INJURIES
```

Find the max of Harmful
``` {r, echo = TRUE}
max <- max(df$Harmful)
max
```

Find the most harmful Event
``` {r, echo = TRUE}
df[df$Harmful == max, "EVTYPE"]
```

Lets save it
``` {r, echo = TRUE}
most_harmful_event <- df[df$Harmful == max, "EVTYPE"]
cat(as.character(most_harmful_event))
```
## Question 2: Across the United States, which types of events have the greatest economic consequences?

We must find the greatest damage. In our dataset we have indices of DMG like "K", "M" and "B".

Let's subset the Property Damage Estimate by "B" that meens billions of dollars.
``` {r, echo = TRUE}
PROP_B <- df$PROPDMG[which(df$PROPDMGEXP=="B")]
```

Find the max for the Property Damage Estimate 
``` {r, echo = TRUE}
PROP_max <- max(PROP_B)
PROP_max
```

Let's subset the Crop Damage Estimate by "B" that meens billions of dollars.
``` {r, echo = TRUE}
CROP_B <- df$CROPDMG[which(df$CROPDMGEXP=="B")]
```

Find the max for the Property Damage Estimate 
``` {r, echo = TRUE}
CROP_max <- max(CROP_B)
CROP_max
```
We can clearly see that the property damage is 115 billion dollar, which is the the largest amount.

The type of event which creates the greatest economic consequences:
``` {r, echo = TRUE}
Cons <- df$EVTYPE[which(df$PROPDMG == PROP_max & df$PROPDMGEXP == "B")]
cat(as.character(Cons))
```

## Plotting

Number of injuries and fatalities caused by Storm
``` {r, echo = TRUE}
tornado <- df[which(df$EVTYPE == "TORNADO"),]
par(mfrow=c(1,2))
boxplot(tornado$INJURIES, main ="Injuries caused by Tornado"
        ,xlab="Storm", ylab="Number of Injuries")
boxplot(tornado$FATALITIES, main ="Fatalities caused by Tornado"
        ,xlab="Storm", ylab="Number of Injuries")
```

The Property Damage caused by Flood
``` {r, echo = TRUE}
boxplot(df$PROPDMG[df$EVTYPE=="FLOOD"]
        , main ="Property Damage caused by Flood"
        ,xlab="FLOOD", ylab="Billions of Dollar")
```
##Results

Question 1: Across the United States, which types of events have the greatest economic consequences?
``` {r, echo = TRUE}
most_harmful_event <- df[df$Harmful == max, "EVTYPE"]
most_harmful_event
```
The answer is TORNADO.

Question 2: Across the United States, which types of events have the greatest economic consequences?
``` {r, echo = TRUE}
Cons <- df$EVTYPE[which(df$PROPDMG == PROP_max & df$PROPDMGEXP == "B")]
Cons
```
The answer is FLOOD.
