############# Diverging Bars #######################################################################
# Data Set used: ODI
# Category: Comparison (Univariate Analysis)
# No. of dimension presented: 1
# No. of measure (Matric) presented: 1
# Special remark: Can be used to present the x -axis variable above or below a referance value.

# Problem Statement: Objective is to identifiy the months with below average and above average matches

library(readxl)
library(ggplot2)
library(dplyr)

odi = read.csv("D://DSML-M/SEM1/EDA/CSV Files/odi-batting.csv") 

# Creating a month column and I need month in abbreviation
odi$MatchDate <- as.Date(odi$MatchDate, "%m-%d-%Y")
odi$Month = format(odi$MatchDate, "%b")

# Using dplyr summarise the month wise matches
matches_month_wise = odi %>% group_by(Month) %>% summarise(Count = n())

# Find the average number of matches across the months
Average = round(mean(matches_month_wise$Count))

# Creating column marked with below and above based on the average matches 
matches_month_wise$Avg_type = ifelse(matches_month_wise$Count < Average, "below", "above") 

# Converting all the count of matches below average to negative values - this will make average as the middle axis
matches_month_wise$Count = ifelse(matches_month_wise$Avg_type == "below", -1*matches_month_wise$Count, matches_month_wise$Count)

chart = ggplot(matches_month_wise, aes(x= Month, y= Count)) + 
  geom_bar(stat='identity', aes(fill=Avg_type), width=.5) + 
  scale_fill_manual(name="Matches", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="Green", "below"="Red")) + 
  labs(subtitle="Avg. Matches Month Wise", 
       title= "Diverging Bars", x = "Month", y = "Match Count") + scale_x_discrete(limits = month.abb) + 
  theme_bw() + 
  geom_text(aes(label = ifelse((Count>0), Count, -1*Count)),vjust = ifelse((matches_month_wise$Count>0), -0.25, 1)) + 
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) 

chart
