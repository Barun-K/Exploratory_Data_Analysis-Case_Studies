######## Scatter Plot ##########################################################################
# Data Set used: mpg
# Category: Relationship (Bivariate Analysis)
# No. of dimension presented: 0
# No. of measure (Matric) presented: 2
# Special remark: Mainly used to find the relationship between two numerical varibale.
# Problem statement : Objective is to plot the relation between highway mileage and City 
# mileage of each class. 

data1= mpg
View(data1)
# Lets plot a simple scatter Plot
chart3 = ggplot(data1, aes(x=cty, y=hwy)) + geom_point(aes(col=class)) +
  labs(subtitle="City Mileage v/s Highway Mileage", y="Highway Mileage", x="City Mileage", title="Scatterplot" ,
       caption = "Source: mpg data set")

chart3

############ Jitter Plot ###################################################################
# Data Set used: mpg

# Source of Data : R - Inbuild data set

# Problem statement : Find the corelation between highway mileage and City mileage of each class without having the problem of data points over lapping

chart4 = ggplot(mpg, aes(x = cty, y = hwy)) + geom_jitter(aes(col = class), width = .5, size=1) +
  labs(subtitle="City Mileage v/s Highway mileage", y="Highway Mileage", x="City Mileage", 
       title="Jittered Points")

chart4


library(gridExtra)
Plot = grid.arrange(chart3, chart4)
Plot


