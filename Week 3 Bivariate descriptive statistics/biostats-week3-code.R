# open local health department (LHD) data
lhd <- read.csv("http://tinyurl.com/zn46s6e")
summary(lhd)

# open ggplot2 for graphing
library() 

# make a histogram
ggplot(data = , aes(x = )) +
  geom_histogram(color=I("white")) + 
  xlab("Number of people served") + 
  ylab("Number of health departments") +
  ggtitle("Number of people served by health departments")

# graph with facets to see health depts
# with and without HIV programs
ggplot(data = lhd, aes(x=numserved)) +
  geom_histogram(color=I("white")) + 
  facet_grid(rows = vars(hivscreen)) + 
  xlab("Number of people served") + 
  ylab("Number of health departments") +
  ggtitle('Number of people served in health departments\nwith and without HIV screening')


# get the median for numserved by hivscreen
by(data = , 
   INDICES = , 
   FUN = median) 


# get the summary for numserved by hivscreen
by(data = , 
   INDICES = , 
   FUN = summary) 


# boxplot of numserved by hivscreen
ggplot(data = , aes(x = , y = )) + 
  geom_boxplot() + 
  ggtitle("Number served by health departments with\nand without HIV screening")


# take a subset of the lhd data where hivscreen 
# is not NA
lhd.noNA <- subset(x = lhd, hivscreen!='NA')

# check the hivscreen variable in the new data frame
summary(lhd.noNA$hivscreen) 

# plot it again
ggplot(data = , aes(x = , y = )) + 
  geom_boxplot() + 
  ggtitle("Number served by health departments with\nand without HIV screening")


# plot it again without the outliers
# add ylim to change the y-axis limits
ggplot(data = lhd.noNA, aes(x = hivscreen, y = numserved)) + 
  geom_boxplot() + 
  ylim(0, 125000) +
  ggtitle("Number served by health departments with\nand without HIV screening")   


# add axis labels to plot
ggplot(data = lhd.noNA, aes(x = hivscreen, y = numserved)) + 
  geom_boxplot(outlier.shape = NA) + 
  ylim(0, 125000) +
  ylab("Number of people served") +
  xlab("Does health dept provide HIV screening") +
  ggtitle("Number served by health departments with\nand without HIV screening")  


# use the density geom to make a petal plot
# add alpha for transparency
ggplot(data = lhd.noNA, 
       aes(x = numserved, fill = hivscreen)) + 
  geom_density(alpha=I(.7)) + 
  xlab("Number of people served") + 
  ggtitle("Number served by health departments with\nand without HIV screening")


# table of hiv and cancer screening
table(lhd$hivscreen, lhd$cancerscreen)


# add row and column names to table
table(hiv = lhd$hivscreen, cancer = lhd$cancerscreen)


# find percentages
prop.table(table(hiv = lhd$hivscreen, 
                 cancer = lhd$cancerscreen))

# open vcd package
library(vcd)      

# make the mosaic plot
mosaicplot(~hivscreen+cancerscreen,
       data=lhd, color=c("magenta","orange"),
       xlab="HIV screening", ylab="Cancer screening",
       main="HIV screening and diabetes screening in LHDs")

# try bar plots with facets or groups
# subset data so there are no NA values in cancer screen
lhd.noNA.screen <- subset(lhd.noNA, cancerscreen != 'NA')

# bar plot with groups
ggplot(data = lhd.noNA.screen, 
       aes(x = cancerscreen, fill = hivscreen)) + 
  geom_bar(position = "dodge") + 
  xlab("Cancer screening") + 
  labs(fill = "Cancer\nscreening") +
  ggtitle("Cancer screening at health departments with\nand without HIV screening")

# bar plot with facets
ggplot(data = lhd.noNA.screen, 
       aes(x = cancerscreen)) + 
  geom_bar() + 
  facet_grid(rows = vars(hivscreen)) +
  xlab("Cancer screening") + 
  ylab("HIV screening") +
  ggtitle("Cancer screening at health departments with\nand without HIV screening")


# graph revenues by expenditures
ggplot(data = lhd, aes(x = revenues, y = expenditures)) + 
  geom_point() + 
  ggtitle("Relationship between health dept revenues and expenditures")


# covariance of revenues and expenditures
cov(lhd$revenues,lhd$expenditures, use='complete')


# scatterplot of revenues and expenditures
ggplot(lhd, aes(revenues, expenditures)) +  
  geom_smooth(method = "lm", se = FALSE) +
  geom_point()+
  ggtitle("Relationship between health dept revenues and expenditures")


# graph with limits on y-axis
ggplot(lhd, aes(revenues, expenditures)) +  
  geom_smooth(method = "lm", se = FALSE, colour = "pink") +
  geom_point()+
  ggtitle("Relationship between health dept revenues and expenditures")+
  ylim(0,20000000)


# correlation between revenues and expenditures
cor(lhd$revenues, lhd$expenditures, use='complete')


# subset revenues to less than 10 million
lhd.subset <- subset(x = lhd, revenues < 10000000)
summary(lhd.subset$revenues)


# graph with limits on y-axis
ggplot(lhd.subset, aes(revenues, expenditures)) +  
  geom_smooth(method = "lm", se = FALSE, colour = "pink") +
  geom_point()+
  ggtitle("Relationship between health dept revenues and expenditures")+
  ylim(0,20000000)
