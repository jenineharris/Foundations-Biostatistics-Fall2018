## \textcolor{blue}{Standard edition}

# bring in NHANES data with smoking module
library(RNHANES)
nhanes2013 <- nhanes_load_data("SMQ_H", "2013-2014", demographics = TRUE)
summary(nhanes2013)

# 1. Recode the SMQ020 variable
library(car)
nhanes2013$smoked100cigs <- recode(nhanes2013$SMQ020,
                                  "1 = 'Yes';
                                  2 = 'No';
                                  7 = 'Refused'; 
                                  9 = 'Dont know'")

# 2. Recode the SMD030 variable
nhanes2013$ageStartSmk <- recode(nhanes2013$SMD030,
                                 "777 = NA; 
                                  999 = NA")

# 3. Use an appropriate table and an appropriate graph to show the distribution 
# of the variables you recoded in #1 and #2.
table(nhanes2013$smoked100cigs)

ggplot(data = nhanes2013, aes(x = smoked100cigs)) + 
  geom_bar() + 
  xlab("Ever smoked 100 cigarettes") + 
  ylab("Number of survey participants") +
  ggtitle("Smoking behavior among 2015-2016 NHANES survey participants")

table(cut(nhanes2013$ageStartSmk, 5))

ggplot(data = nhanes2013, aes(x = ageStartSmk)) + 
  geom_histogram() + 
  xlab("Age started smoking regularly") + 
  ylab("Number of survey participants") +
  ggtitle("Smoking behavior among 2015-2016 NHANES survey participants")

# 4. Compute the central tendency for the variables examined in question 3. 
# Use the most appropriate measure of central tendency for each variable.

# mode of ever smoked 100 cigs
sort(table(nhanes2013$smoked100cigs), decreasing = T)

# mean of age started smoking
mean(nhanes2013$ageStartSmk, na.rm = TRUE)

# 5. Report the spread for the variable from question 2. 
# histogram looks pretty normal, so standard deviation
sd(nhanes2013$ageStartSmk, na.rm = TRUE)

# 6. Determine the probability of selecting person at 
# random from the data who 
# smoked at least 100 cigarettes in their life.
prop.table(table(nhanes2013$smoked100cigs))
# 40.4%

# 7. Determine the probability of starting smoking 
# regularly between ages 7 and 12.
ageDensity <- density(nhanes2013$ageStartSmk, na.rm=TRUE)

pdf <- approxfun(ageDensity$x, ageDensity$y, rule=2)

area <- integrate(pdf, 7, 12)      
area
# 5.04% of smokers started between 7 and 12 years old

## \textcolor{blue}{Hacker edition}

# 1-6 see above

# 7. Create a density plot age started smoking cigarettes regularly. 

# turn density object into a data frame to graph
ageDensityData <- data.frame(x = ageDensity$x, y = ageDensity$y)

# graph density data
ggplot(data = ageDensityData, mapping = aes(x = x, y = y)) +
  geom_line()+
  ylim(0, .15) +
  xlab("Age of started smoking regularly") + 
  ylab("Probability density") + 
  ggtitle("Probability density for age started regular smoking (NHANES 2015-2016)")

# 8. Determine the probability of starting smoking regularly between ages 7 and 12.
ageDensity <- density(nhanes2013$ageStartSmk, na.rm=TRUE)

pdf <- approxfun(ageDensity$x, ageDensity$y, rule=2)

area <- integrate(pdf, 7, 12)      
area
# 5.04% of smokers started between 7 and 12 years old

# 9. Shade the density plot for to highlight the area under the curve 
# representing the probability of starting smoking regularly between ages 7 and 12. 
# Be sure you can see the curve and the shaded area well (you may have to adjust 
# some of the values in the plot commands!).  
ggplot(data = ageDensityData, mapping = aes(x = x, y = y)) +
  geom_line()+
  ylim(0, .15) +
  xlab("Age of started smoking regularly") + 
  geom_area(mapping = aes(x = ifelse(x>7 & x< 12 , x, 0)), fill = "darkgreen") + 
  ylab("Probability density") + 
  ggtitle("Probability density for age started regular smoking (NHANES 2015-2016)") 

