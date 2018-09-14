# Challenge 1 Key

# Used Skittles for data

# 1. Make a vector containing the colors of the candies in your bag (e.g., red, blue, yellow)
color <- c('red','yellow','orange','green','purple')

# 2. Make a vector containing the number of candies for each color
num <- c(10, 13, 12, 9, 11)

# 3. Create a data frame from the two vectors
skit <- data.frame(color, num)

# 4. Print a summary of the data 
summary(skit)

# 5. If the summary indicates the color vector is factor type, change it to character type
skit$color <- as.character(skit$color)

# 6. Print the colors with fewer than the median number of Skittles/M&Ms
skit$color[skit$num < 11]  

# or
skit$color[skit$num < median(skit$num, na.rm = TRUE)]

# 7. Print the first row of your data frame 
#use the subset command to print the first row
skit[1,]

# 8. Make a bar graph of the data showing number of candies on the y-axis and color on the x-ax
library(ggplot2)
ggplot(skit, aes(x=color, y=num))+
  geom_bar(stat="identity")+             
  ggtitle("Number of  Skittles by color")   

# 9. Make a new data frame that includes only the colors with more Skittles 
# than the median value and print a summary of the new data frame 
skit2 <- skit[skit$num<11, ]
summary(skit2)

# 10. Make a bar graph of the new data frame showing number of candies per color
ggplot(skit2, aes(x=color, y=num))+
  geom_bar(stat="identity")+             
  ggtitle("Number of  Skittles by color") 