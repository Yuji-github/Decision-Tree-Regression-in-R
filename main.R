# install.packages('rpart')
library(ggplot2)
library(caTools)
library(rpart) # for decision tree

# import data 
data = read.csv('Position_Salaries.csv')

# Position and Level are the same 
data = data[2:3]

# plotting import data 
ggplot() +
  geom_point(aes(x = data$Level, y = data$Salary), 
             color = 'red')+
  ggtitle('Salary vs Level') + 
  xlab('Level') + 
  ylab('Salary')
# the plot shows that linear can NOT follow the areas

# skip splitting data 
# set.seed(123)
# split = sample.split(data$Salary, SplitRatio = 2/3) #split(y, splitratio)
# train = subset(data, split == TRUE)
# test = subset(data, split == FALSE)

# skip feature scaling because of Decision Tree
# feature scaling 
# train = scale(train)
# test = scale(test)

# training data
tree_regrssor = rpart(formula = Salary ~., data = data,
                      control = rpart.control(minsplit = 1))
summary(tree_regrssor)

# prediction 
y_pred = predict(tree_regrssor, newdata = data)
y_pred_six = predict(tree_regrssor, data.frame(Level = 6.5))
  
# visualization with non-smooth
ggplot() +
  geom_point(aes(x = data$Level, y = data$Salary), 
             color = 'red')+
  geom_line(aes(x = data$Level, y = y_pred ), 
            color = 'blue')+
  ggtitle('Salary vs Level (Decision Tree (No high resolution)') + 
  xlab('Level') + 
  ylab('Salary')

#  visualization with smoothness (high resolution)
x_grid = seq(min(data$Level), max(data$Level), 0.01) # to modify the steps can show more better results 
ggplot() +
  geom_point(aes(x = data$Level, y = data$Salary), 
             color = 'red')+
  geom_line(aes(x = x_grid, 
                y = predict(tree_regrssor, newdata = data.frame(Level = x_grid))), 
            color = 'blue')+
  ggtitle('Salary vs Level (Decision Tree with smoothness)') + 
  xlab('Level') + 
  ylab('Salary')
