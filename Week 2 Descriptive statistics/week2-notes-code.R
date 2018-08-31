# open the RNHANES library
library(RNHANES)

# download 2013-2014 NHANES data
# use the name of the data you want (DUQ_H for drug use)
# add demographic data using demographics = TRUE
nhanes2013 <- nhanes_load_data(file_name = "",
                               year = "",
                               demographics = )

# frequency table of marijuana use
table()

# bar graph of marijuana use
# need ggplot open if it is not already open
library(ggplot2)
ggplot(data = , aes(x = )) + 
  geom_bar()
         
# open the car library
library(car)

# recode into a new marijuanaUse variable
nhanes2013$marijuanaUse <- recode(nhanes2013$DUQ200,
                                  " = 'Yes';
                                   = 'No';
                                   = 'Refused'; 
                                   = 'Dont know'")

# try the table and graph again
table()

ggplot(data = , aes(x = )) + 
  geom_bar()

# adding titles to the plot
ggplot(data = , aes(x = )) + 
  geom_bar() + 
  xlab("Ever used marijuana or hashish") + 
  ylab("Number of survey participants") +
  ggtitle("Marijuana use among 2013-2014 NHANES survey participants")
       
# add labels and recode 
# refused and don't know to missing NA
nhanes2013$marijuanaUse <- recode(nhanes2013$DUQ200,
                                  " = 'Yes';
                                   = 'No';
                                   = NA; 
                                   = NA")
# try the table and graph again
table()

ggplot(data = , aes(x = )) + 
  geom_bar() + 
  xlab("Ever used marijuana or hashish") + 
  ylab("Number of survey participants") +
  ggtitle("Marijuana use among 2013-2014 NHANES survey participants")


# first tried cocaine 
# frequencies with continuous data
table()

# first tried cocaine 
# frequency table with 5 breaks
table(cut(x = , breaks = 5))

# histogram of date first tried cocaine
ggplot(data = , aes(x = )) + 
  geom_histogram()

ggplot(data = , aes(x = )) + 
  geom_histogram(binwidth = 5, color = I("white")) +
  xlab("Age of first cocaine use") +
  ylab("Frequency") +
  ggtitle("Distribution of age of first cocaine use (NHANES 2013-2014)")

# find the mode for marijuana use
sort(table(nhanes2013$marijuanaUse), decreasing = T)

# mean and median age first used cocaine
mean(nhanes2013$DUQ260, na.rm = TRUE)                
median(nhanes2013$DUQ260, na.rm = TRUE)  

# variance of age of first cocaine use
var(nhanes2013$DUQ260, na.rm = TRUE)   

# standard deviation for age of first use
sd(nhanes2013$DUQ260, na.rm = TRUE)   
#
# range of age first use of cocaine
range(nhanes2013$DUQ260, na.rm = TRUE) 

# interquartile range of age of first use
IQR(nhanes2013$DUQ260, na.rm = TRUE)

# rename and recode continuous variable
nhanes2013$ageFirstUse <- recode(,
                                  "777 = NA; 
                                  999 = NA")
summary(nhanes2013$ageFirstUse)

# table with probability of marijuana use
prop.table(table())

# three histograms of age of first use
# with different bins
par(mfrow=c(1,3))
hist(nhanes2013$ageFirstUse, breaks=15, 
     main="15 bins", probability=TRUE, ylab="Probability", xlab = "Age first use")
hist(nhanes2013$ageFirstUse, breaks=6, 
     main="6 bins", probability=TRUE, ylab="Probability", xlab = "Age first use")
hist(nhanes2013$ageFirstUse, breaks=3, 
     main="3 bins", probability=TRUE, ylab="Probability", xlab = "Age first use")


# probability density function for age of first use
ageUseDensity <- density(nhanes2013$ageFirstUse, na.rm=TRUE)
plot(ageUseDensity, 
     main="Probability density plot for age of first cocaine use")


# turn density object into a data frame to graph
ageUseDensityData <- data.frame(x = ageUseDensity$x, y = ageUseDensity$y)

# graph density data
ggplot(data = ageUseDensityData, mapping = aes(x = x, y = y)) +
    geom_line()+
    geom_area(mapping = aes(x = ifelse(x>12 & x< 15 , x, 0)), fill = "darkgreen") + 
  ylim(0, .15) +
  xlab("Age of first cocaine use") + 
  ylab("Probability density") + 
  ggtitle("Probability density for age of first cocaine use\n(NHANES 2013-2014)")


# compute the probability density function
# rule 2 finds the probability in a given range
pdf <- approxfun(ageUseDensity$x, ageUseDensity$y, rule=2)

# find the area under the curve between 12 and 15
area <- integrate(pdf, , )      
area


# basic bar plot
ggplot(nhanes2013, aes(x = marijuanaUse)) +
  geom_bar()               

# add color, labels, title
ggplot(nhanes2013, aes(x = marijuanaUse, fill = marijuanaUse)) +
  geom_bar() + 
  xlab("Ever used marijuana or hashish") +
  ggtitle("NHANES participants who ever used marijuana or hashish")

# basic histogram
ggplot(, aes(x = )) +
  geom_histogram()             

# histogram with binwidth of 6 and white dividers
# added labels and titles
ggplot(, aes(x = )) +
  geom_histogram(binwidth = 6, color = I("white")) +
  xlab("Age of first cocaine use") +
  ylab("Frequency") +
  ggtitle("Distribution of age of first cocaine use (NHANES 2013-2014)")                 


# basic density plot
ggplot(, aes(x = )) +
  geom_density()             

# density plot
# added labels and titles
ggplot(, aes(x = )) +
  geom_density(fill = I("lightblue")) +
  xlab("Age of first cocaine use") +
  ylab("Density") +
  ggtitle("Distribution of age of first cocaine use (NHANES 2013-2014)")   

