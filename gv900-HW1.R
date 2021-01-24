##------------------------------------------------------------------------------
## R code for GV900 Homework Assignment 1
## University of Essex
##------------------------------------------------------------------------------
#Load up the following packages: 
library(ggplot2)
library(magrittr)
library(tidyverse) 

##-----------------------------------------------------------------------------
#Task 1-  Load the 'world' dataset (world.csv), and store it as an object named 
#world.data.

world.data <- read.csv("world.csv")
read.csv("world.csv")

#-------------------------------------------------------------------------------
#Task 2
#Create a frequency table 

table(world.data$oecd)
world.data$oecd
data.frame(table(world.data $ oecd))

#Store the frequency table into a data frame

(ft.oecd <- world.data$oecd %>% table %>%  data.frame)


#Check the percentage of countries that are OECD members, then add it as a column 

(ft.oecd$Percentage <- prop.table(table(world.data $ oecd)))


#Change name  of columns 
colnames(ft.oecd) <- c('OECD Member', 'Freq', 'Percentage')

#-------------------------------------------------------------------------------
#Task 3
#(A) How many countries in the data set are OECD members? 
#(A) Answer: 30

#(B) How many countries in the data set are not? 
#(B) Answer: 161

#(C) What percentage of countries are OECD members? 
#(C) Answer: 15% 

#(D) What percentage of countries are non-members? 
#(D) Answer: 84%

#-------------------------------------------------------------------------------
#Task 4
#Draw a bar chart that measures OECD Membership, then change axis labels 

ggplot(world.data, aes(x = oecd)) + geom_bar() +
  xlab("OECD Membership") + 
  ylab("Number of Countries")

#-------------------------------------------------------------------------------
#Task 5
#List 3 countries coded as OECD Members

#Australia, Austria and Belgium 

#List 3 countries that are non-democratic 

#Afghanisatan, Algeria and Angola

#-------------------------------------------------------------------------------
#Task 6 
#Provide the Range for gdp_10_thou

summary(world.data$gdp_10_thou)

#Calculate the Standard Deviation 

sd(world.data$gdp_10_thou, na.rm=TRUE)

#-------------------------------------------------------------------------------
#Task 7 
#What is the distribution of the gdp_10-thou variable? 

#Answer: Positively skewed 
#The mean is greater than the median, so it is skewed right

#-------------------------------------------------------------------------------
#Task 8 
#Draw a histogram for the gdp_10_thou

ggplot(world.data, aes(x = gdp_10_thou)) + geom_histogram() +
  xlab("Per Capita GDP (in 10,000 Dollars)") + 
  ylab("Number of Countries")

#-------------------------------------------------------------------------------
#Task 9 
#Work out which two countries' GDP is over $40,000 per capita

world.data[c("country", "gdp_10_thou")]
world.data[world.data$gdp_10_thou>4, 1]
#Answer: Luxembourg and Norway's GDP is over $40,000 per capita 

#-------------------------------------------------------------------------------
#Task 10
#Calculate the Standard Error of the mean of GDP per capita 

sd(world.data$gdp_10_thou, na.rm=TRUE) /  
  sqrt(length(world.data$gdp_10_thou[!is.na(world.data$gdp_10_thou)]))

#Answer: Standard error of the mean of GDP per capita= 0.07091015
#-------------------------------------------------------------------------------
#Task 11
#Calculate the 95% confidence interval 

# mean of the column 
pe <- mean(world.data$gdp_10_thou, na.rm = TRUE)

# standard deviation of column
# number of obs
n <- length(world.data$gdp_10_thou[is.na(world.data$gdp_10_thou) == FALSE] )

# standard error
se <- sd(world.data$gdp_10_thou, na.rm=TRUE) /  
         sqrt(n)

# Finally, we construct a 95% confidence interval by identifying the 
# lower and upper bounds
pe - 2 * se   # Lower bound
pe + 2 * se   # Upper bound

#Lower bound 95% confidence interval= 0.4599983
#Upper bound 95% confidence interval= 0.7436389
#-------------------------------------------------------------------------------
#Task 12 
#Draw histograms of per capita GDP, one for democracies and non-democracies

world.data[, c("democ_regime", "gdp_10_thou")]

g <- ggplot(world.data, aes(x=gdp_10_thou))
g <- g + geom_histogram()
g <- g + theme(axis.text.x = element_text(size = 12))
g <- g + xlab("Per capita GDP(in 10,000 dollars)")
g <- g + ylab("Number of countries")
g <- g + facet_wrap( ~ democ_regime) 
g

#-------------------------------------------------------------------------------
#Task 13
#Create a new data frame that excludes the missing values 
dem.gdp <-  world.data[ !is.na(world.data$democ_regime), ]
# Alternatively
dem.gdp <-  world.data[ is.na(world.data$democ_regime) == FALSE, ]

# Create a new variable, creating the labels 'Democracy' and 'Autocracy', 
# instead of 'Yes' and 'No'

dem.gdp$dem.dum[dem.gdp$democ_regime == 'Yes']  <- "Democracy"
dem.gdp$dem.dum[dem.gdp$democ_regime == 'No']  <- "Autocracy"
dem.gdp$dem.dum

#Recreate the histogram from Task 12 

g <- ggplot(dem.gdp, aes(x=gdp_10_thou))
g <- g + geom_histogram()
g <- g + theme(axis.text.x = element_text(size = 12))
g <- g + xlab("Per capita GDP(in 10,000 dollars)")
g <- g + ylab("Number of countries")
g <- g + facet_wrap( ~ dem.dum) 
g

#-------------------------------------------------------------------------------
#Task 14
#Calculate the mean of per capita GDP for democracies

democTTest <- t.test(world.data$gdp_10_thou[world.data$democ_regime=="Yes"],
                     conf.level =  .95)
str(democTTest)
democTTest$estimate 

#Answer: Mean value of per capita GDP for democracies= 0.801

#Alternative method of working out the mean of per capita GDP for democracies 

meanDem <- world.data %>% 
  drop_na(democ_regime) %>%
  group_by(democ_regime) %>%
  summarise(meanDemType = mean(gdp_10_thou, na.rm = TRUE, .groups = "drop_last"))

#Calculate the 95% confidence interval for democracy 

str(democTTest)
democTTest$conf.int

#Lower bound 95% confidence interval= 0.5961327 
#Upper bound 95% confidence interval= 1.0066527 
#-------------------------------------------------------------------------------
#Task 15 
#Calculate the mean of per capita GDP for autocracies

autocTTest <- t.test(world.data$gdp_10_thou[world.data$democ_regime=="No"],
                     conf.level =  .95)
str(autocTTest)
autocTTest$estimate

#Answer: Mean value of per capita for autocracies= 0.281

#Calculate the 95% confidence interval for autocracy 

str(autocTTest)
autocTTest$conf.int

#Lower bound 95% confidence interval= 0.1526567
#Upper bound 95% confidence interval= 0.4111697