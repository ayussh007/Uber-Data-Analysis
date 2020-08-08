#########################  Uber Data Analytics  PROJECT    #####################

##########  Step 1  Loading the Libraries

library(ggplot2) #used to plot various graphs
library(ggthemes) #various themes that can be used in ggplot
library(lubridate) #makes it easier to work with datasets invloving date & time
library(dplyr) #It provides some great, easy-to-use functions that are very handy when
               #performing exploratory data analysis and manipulation
library(tidyr) # provides three main functions for tidying your messy data:
               #gather() , separate() and spread() 
library(DT) # Used for data tables
library(scales) #The scales packages provides the internal scaling infrastructure used
                #by ggplot2

######### Step 2 Defining Colors

colors = c("#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0")



########### 3. Reading the Data into their designated variables

apr_data <- read.csv("C:\\Users\\dell\\Desktop\\UBER\\uber-raw-data-apr14.csv")
may_data <- read.csv("C:\\Users\\dell\\Desktop\\UBER\\uber-raw-data-may14.csv")
jun_data <- read.csv("C:\\Users\\dell\\Desktop\\UBER\\uber-raw-data-jun14.csv")
jul_data <- read.csv("C:\\Users\\dell\\Desktop\\UBER\\uber-raw-data-jul14.csv")
aug_data <- read.csv("C:\\Users\\dell\\Desktop\\UBER\\uber-raw-data-aug14.csv")
sep_data <- read.csv("C:\\Users\\dell\\Desktop\\UBER\\uber-raw-data-sep14.csv")

# Then, in the next step, we will perform the appropriate formatting of Date.Time
#column.Then, we will proceed to create factors of time objects like day, month, year

# rbind() function combines vector, matrix or data frame by rows. 
# The column numbers of the two 
# datasets must be the same, otherwise the combination will be meaningless.

data_2014 <- rbind(apr_data,may_data,jun_data,jul_data,aug_data,sep_data)

# The as. POSIX* functions convert an object to one of the two classes used to represent date/times 

data_2014$Date.Time <- as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S")

data_2014$Time <- format(as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S"), format = "%H:%M:%S")

data_2014$Date.Time <- ymd_hms(data_2014$Date.Time)

#Factors are used to represent categorical data. Factors can be ordered or unordered
#and are an important class for statistical analysis and for plotting.

data_2014$day <- factor(day(data_2014$Date.Time))  #factorizing w.r.t day
data_2014$month <- factor(month(data_2014$Date.Time, label = TRUE)) # w.r.t month
data_2014$year <- factor(year(data_2014$Date.Time)) # w.r.t year
data_2014$dayofweek <- factor(wday(data_2014$Date.Time, label = TRUE)) w.r.t dayofweek

data_2014$hour <- factor(hour(hms(data_2014$Time)))
data_2014$minute <- factor(minute(hms(data_2014$Time)))
data_2014$second <- factor(second(hms(data_2014$Time)))




################  PLOTTING THE TRIPS BY THE HOURS IN A DAY   ####################

hour_data <- data_2014 %>%
  group_by(hour) %>%
  dplyr :: summarise(Total = n())  # collecting the hour data and summarising it
datatable(hour_data) # show us data in the form of a table

# geom_bar uses stat="bin" . This makes the height of each bar equal to the number of 
# cases in each group, and it is incompatible with mapping values to the y aesthetic.
# If you want the heights of the bars to represent values in the data, use stat="identity" 
# and map a value to the y aesthetic.

ggplot(hour_data, aes(hour, Total)) + geom_bar(stat = "identity", fill = "blue", color = "orange") +
  ggtitle("Trips Every Hour") + theme(legend.position = "none") + scale_y_continuous(labels = comma)
# ggtitle gives the title to the given plot and labels = comma is used to seperate each bar
# hour_data is used and displayed on the x axis  

month_hour <- data_2014 %>%
  group_by(month, hour) %>%
  dplyr :: summarise(Total = n()) # collecting the month & hour data & summarizing it
datatable(month_hour) 

## Plotting now


ggplot(month_hour, aes(hour, Total, fill = month)) + geom_bar(stat = "identity") +
  ggtitle("Trips By Hour and Month") + scale_y_continuous(labels = comma)
# In this we have 6 different colours for every month that shows us the trip taken in each month 
# and in every hour

###################3 PLOTTING OF THE DATA BY TRIPS DURING EVERYDAY OF THE MONTH   ##########

day_group <- data_2014 %>% group_by(day) %>% dplyr::summarise(Total = n())
datatable(day_group)

ggplot(day_group, aes(day, Total)) + geom_bar(stat = "identity", fill = "steelblue", color = "red") +
  ggtitle("Trips Every Day") + theme(legend.position = "none") + scale_y_continuous(labels = comma)


day_month_group <- data_2014 %>% group_by(month,day) %>% dplyr::summarise(Total = n())

ggplot(day_month_group, aes(day, Total, fill = month)) + geom_bar(stat = "identity") +
  ggtitle("Trips By Day and Month") + scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors)



#########  NUMBER OF TRIPS TAKING PLACE DURING MONTHS IN A YEAR  #######################33

month_group <- data_2014 %>% group_by(month) %>% dplyr::summarise(Total = n())
datatable(month_group)


ggplot(month_group, aes(month, Total, fill = month)) + geom_bar(stat = "identity") +
  ggtitle("Trips By Month") + theme(legend.position = "none") + scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors)


month_weekday <- data_2014 %>% group_by(month, dayofweek) %>% dplyr::summarise(Total = n())
ggplot(month_weekday, aes(month, Total, fill = dayofweek)) + geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Trips By Day and Month") + scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors)


###########  FINDING OUT THE NUMBER OF TRIPS BY BASES   #########################

ggplot(data_2014, aes(Base)) + geom_bar(fill = "darkred") + scale_y_continuous(labels = comma) + ggtitle("Trips by Bases")

# Trips by Bases and Month

ggplot(data_2014, aes(Base, fill = month)) + geom_bar(position = "dodge") +
  scale_y_continuous(labels = comma) + ggtitle ("Trips by Bases and Month") +
  scale_fill_manual(values = colors)


# Trips by Bases and DayofWeek

ggplot(data_2014, aes(Base, fill = dayofweek)) + geom_bar(position = "dodge") +
  scale_y_continuous(labels = comma) + ggtitle("Trips by Bases and DayofWeek") +
  scale_fill_manual(values = colors)


########### Generating Heatmaps of Trips   ########################

# Heat Map is a data Visualization technique that shows magnitude of phenomenon as color in
# 2 dimesnsions

# We will plot 5 HeatMaps using ggpplots
# 1. We will plot heatmap by Hour and Day
# 2. We will plot Heatmap by Month and Day
# 3. We will plot HeatMap by Month and Day of the Week
# 4. We will plot Heatmap which will delineates Month and Bases
# 5. We will plot Heatmap by bases and day of the week

day_and_hour <- data_2014 %>% group_by(day,hour) %>% dplyr::summarise(Total = n())
datatable(day_and_hour)

ggplot(day_and_hour, aes(day,hour,fill = Total)) + geom_tile(color = "white") + 
  ggtitle("HeatMap by Hour and Day")


ggplot(day_month_group, aes(day,month,fill = Total)) + geom_tile(color = "white") + 
  ggtitle("HeatMap by Month and Day")

ggplot(month_weekday, aes(dayofweek,month,fill = Total)) + geom_tile(color = "white") + 
  ggtitle("HeatMap by Month and Day of Week")


month_base <- data_2014 %>% group_by(Base, month) %>% dplyr::summarise(Total = n())
dayofweek_bases <- data_2014 %>% group_by(Base,dayofweek) %>% dplyr::summarise(Total = n())
ggplot(month_base, aes(Base, month, fill = Total)) + geom_tile(color = "white") + 
  ggtitle("Heat Map by Month and Bases")

ggplot(dayofweek_bases, aes(Base, dayofweek, fill = Total)) + geom_tile(color = "white") +
  ggtitle("Heat Map by Bases and Day of Week")

