#-------------------------------Ques-1------------------------------------------


library(ggplot2)
library(datasets)
data(iris)

ggplot(iris,aes(x=Petal.Length,y=Sepal.Length,color= Species)) +
  geom_point() + labs(x="Petal Length", y="Sepal Length", color="Species")

# Looking at the plot, some conclusions that may be drawn:
  
# Sepal Length and Petal Length show a direct relation. As Petal Length 
# increases, Sepal Length tends to increase as well.

# Setosa species generally has the smallest Sepal Length and Petal Length 
# compared to the other two species.

# Virginica species has the largest Sepal Length and Petal Length values.

# The species Virginica and versicolor show some overlap between the sepal and 
# length petal values.


#-------------------------------Ques-2------------------------------------------


library(ggplot2)
data("txhousing")

# Displaying the structure of loaded dataset.
str(txhousing)

# Summary of loaded dataset
summary(txhousing)

# Check for missing values
sum(is.na(txhousing))

# If there are missing values, remove rows with missing values
txhousing <- txhousing[complete.cases(txhousing),]

# Boxplot of city v/s House Sales
ggplot(txhousing, aes(x = city, y = sales, color=city)) +
  geom_boxplot() +
  labs(title="City v/s Sales",x = "City", y = "Sales")+
# here i have added this as x-axis labes were not clear and distinguishable  
# before
  theme(axis.text.x = element_text(angle=75, vjust=0.5))

# Plot for Sales over Year
# This plot concludes to give the city having highest sale over the years.
ggplot(txhousing, aes(x = year, y = sales, color=city)) +
  geom_point() +
  geom_smooth(method = "lm")  +
  labs(title= "Sale vs Year", x = "Year", y = "Sales")



# Scatter plot of median house prices by year
ggplot(txhousing, aes(x = year, y = median, color= city)) +
  geom_point() +
  labs(x = "Year", y = "Median House Price", 
       title = "Median House Prices by Year")

# Boxplot of median house prices by month
ggplot(txhousing, aes(x = month, y = median)) +
  geom_boxplot() +
  labs(x = "Month", y = "Median House Price", 
       title = "Median House Prices by Month")


#-----------------------------Ques-3--------------------------------------------


library(ggplot2)
titanic <- read.csv("titanic.csv") 

# Factorising Survived variable from dataset into two levels "died" and
#  "survived" for expected visualisation.
titanic$Survived <- factor(titanic$Survived, labels = c("Died", "Survived"))

# declaring object for containing the plot
final_Plot <- ggplot(titanic, aes(x = Fare, y =Survived, color = Sex)) +
  geom_boxplot(aes(y=Survived)) +
  labs(title = "Fare vs Survival",
       subtitle = "Irrespective of Sex, richer people survived",
       x = "Fare",
       y=NULL) 

# showing the plot 
final_Plot

 
#-------------------------------------------------------------------------------






