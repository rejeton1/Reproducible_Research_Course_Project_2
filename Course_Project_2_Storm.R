##Reproducible Research Course Project 2

##load required package
library(base)
library(stats)
library(dplyr)
library(stringr)
library(ggplot2)

##Data read
Storm <- read.csv('repdata_data_StormData.csv.bz2')

##Explore Date briefly
names(Storm)
head(Storm)

##Preprocessing Data
##Select only needed Columns
needed_columns <- c("EVTYPE", "FATALITIES", "INJURIES", "PROPDMG",
                    "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")
selected_Storm <- Storm %>% select(all_of(needed_columns))
selected_Storm$EVTYPE <- str_trim(selected_Storm$EVTYPE)

##remove some rows(including "Summary")
good <- !grepl("Summary", selected_Storm$EVTYPE)
Storm3 <- selected_Storm[good, ]


############Question1
##group by EVTYPE and caculate sum of fatality and injury
group_Storm <- Storm3 %>% group_by(EVTYPE) %>% 
  summarize(fatality = sum(FATALITIES), injury = sum(INJURIES))

##ordering by fatality
ordering1 <-
  group_Storm[order(group_Storm$fatality, decreasing = TRUE),]

##ordering by injury
ordering2 <-
  group_Storm[order(group_Storm$injury, decreasing = TRUE),]

##Plot it
par(mfrow = c(1,2), cex.axis=0.8)
barplot(ordering1$fatality[1:3], names.arg = ordering1$EVTYPE[1:3],
        xlab = "Type", ylab = "Fatality")
title(main = "Top3 Sum of fatalities by types of event 1950-2011")
barplot(ordering2$injury[1:3], names.arg = ordering2$EVTYPE[1:3],
        xlab = "Type", ylab = "Injury")

title(main = "Top3 Sum of Injuries by types of event 1950-2011")

##Then, the type of event that are most harmful to health is TORNADO
###############################

###########Question2
####remove data that damage is 0
filter_Storm <- selected_Storm %>% 
  filter(PROPDMG != 0 | PROPDMG != 0) %>%
  filter(PROPDMGEXP %in% c("K", "M", "B", "")) %>%
  filter(CROPDMGEXP %in% c("K", "M", "B", ""))

#####"K", "M", "B" 이외 단위 뭔지 몰라서 일단 빼버리자.


####make new column that mean DAMAGE without Unit.
for(i in 1:nrow(filter_Storm)){
  if(filter_Storm$PROPDMGEXP[i] == ""){
    filter_Storm$property[i] <- 0
  }else if(filter_Storm$PROPDMGEXP[i] == "K"){
    filter_Storm$property[i] <- filter_Storm$PROPDMG[i] * 1000
  }else if(filter_Storm$PROPDMGEXP[i] == "M"){
    filter_Storm$property[i] <- filter_Storm$PROPDMG[i] * 1000000
  }else if(filter_Storm$PROPDMGEXP[i] == "B"){
    filter_Storm$property[i] <- filter_Storm$PROPDMG[i] * 1000000000
  }
}

for(i in 1:nrow(filter_Storm)){
  if(filter_Storm$CROPDMGEXP[i] == ""){
    filter_Storm$crop[i] <- 0
  }else if(filter_Storm$CROPDMGEXP[i] == "K"){
    filter_Storm$crop[i] <- filter_Storm$CROPDMG[i] * 1000
  }else if(filter_Storm$PROPDMGEXP[i] == "M"){
    filter_Storm$crop[i] <- filter_Storm$CROPDMG[i] * 1000000
  }else if(filter_Storm$PROPDMGEXP[i] == "B"){
    filter_Storm$crop[i] <- filter_Storm$CROPDMG[i] * 1000000000
  }
}

filter_Storm$total <- filter_Storm$property + filter_Storm$crop


####group by EVTYPE and sum of the total
final_Storm <- filter_Storm %>% group_by(EVTYPE) %>%
  summarize(economic = sum(total))

##ordering by Damage
ordering3 <- final_Storm[order(final_Storm$economic, decreasing = TRUE),]

##plot it
g <- ggplot(ordering3[1:3,], aes(reorder(EVTYPE, economic, decreasing=TRUE), economic/10^9))
g + geom_bar(stat='identity') + labs(x='Type', y='Economic Damage(Billon dollars)',
                      title = 'Top3 Type of Event that caused Economic
                      Damage 1950-2011') + 
  theme(plot.title = element_text(hjust = 0.5))

####Then the type of event that make the most Damage is HURRICANE.
