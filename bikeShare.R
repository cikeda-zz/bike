train <- read.csv("train.csv")
test <- read.csv("test.csv")

#factor train test set
train_factor <- train
train_factor$season <- factor(train$season, labels = 
                                c("spring", "summer", "fall", "winter"))
train_factor$weather <- factor(train$weather)
train_factor$workingday <- factor(train$workingday,
                                  labels = c("no", "yes"))
train_factor$holiday <- factor(train$holiday, 
                               labels = c("no", "yes"))

test_factor <- test
test_factor$season <- factor(test$season, labels = 
                                c("spring", "summer", "fall", "winter"))
test_factor$weather <- factor(test$weather)
test_factor$workingday <- factor(test$workingday,
                                  labels = c("no", "yes"))
test_factor$holiday <- factor(test$holiday, 
                               labels = c("no", "yes"))

#time column
train_factor$time <- factor(substring(train$datetime, 12, 13))
test_factor$time <- factor(substring(test$datetime, 12, 13))

#day of week column
train_factor$day <- factor(weekdays(as.Date(train_factor$datetime)))
train_factor$day <- factor(train_factor$day, levels = 
                             c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
test_factor$day <- factor(weekdays(as.Date(test_factor$datetime)))

#month column
train_factor$month <- factor(months(as.Date(train_factor$datetime)))
train_factor$month <- factor(train_factor$month, levels = 
                               c("January", "February", "March", "April", "May", "June", "July", "August", "September", 
                                 "October", "November", "December"))
test_factor$month <- factor(months(as.Date(test_factor$datetime)))
str(train_factor)

#day of year column
train_factor$julian <- julian(as.Date(train_factor$datetime)) - 14974
train_factor$julian <- factor(train_factor$julian)

#checking for NA
which(is.na(train_factor))
length(which(is.na(train_factor)))

#histograms
julian<- ggplot(data = train_factor, aes(x = julian, y = count)) + 
  stat_summary(fun.y = mean, geom = "bar", fill = "white", colour = "black")
julian
aggregate(count~julian, train_factor, mean)

day <- ggplot(data = train_factor, aes(x = day, y = count)) + 
  stat_summary(fun.y = mean, geom = "bar", fill = "white", colour = "black")
day
aggregate(count~day, train_factor, mean)

month <- ggplot(data = train_factor, aes(x = month, y = count)) +
  stat_summary(fun.y = mean, geom = "bar", fill = "white", colour = "black")
month
aggregate(count~month, train_factor, mean)

season <- ggplot(data = train_factor, aes(x = season, y = count)) + 
  stat_summary(fun.y = mean, geom = "bar", fill = "white", colour = "black")
season 
aggregate(count~season, train_factor, mean)

time <- ggplot(data = train_factor, aes(x = time, y = count)) + 
  stat_summary(fun.y = mean, geom = "bar", fill = "white", colour = "black")
time
aggregate(count~time, train_factor, mean)

temp <- ggplot(data = train_factor, aes(x = temp, y = count)) + 
  stat_summary(fun.y = mean, geom = "point", fill = "white", colour = "black")
temp
aggregate(count~temp, train_factor, mean)

atemp <- ggplot(data = train_factor, aes(x = atemp, y = count)) + 
  stat_summary(fun.y = mean, geom = "point", fill = "white", colour = "black")
atemp
aggregate(count~atemp, train_factor, mean)

windspeed <- ggplot(data = train_factor, aes(x = windspeed, y = count)) + 
  stat_summary(fun.y = mean, geom = "point", fill = "white", colour = "black")
windspeed
aggregate(count~windspeed, train_factor, mean)

holiday <- ggplot(data = train_factor, aes(x = holiday, y = count)) + 
  stat_summary(fun.y = mean, geom = "bar", fill = "white", colour = "black")
holiday
aggregate(count~holiday, train_factor, mean)
summary(train_factor$holiday)

workingday <- ggplot(data = train_factor, aes(x = workingday, y = count)) + 
  stat_summary(fun.y = mean, geom = "bar", fill = "white", colour = "black")
workingday
aggregate(count~workingday, train_factor, mean)

weather <- ggplot(data = train_factor, aes(x = weather, y = count)) + 
  stat_summary(fun.y = mean, geom = "bar", fill = "white", colour = "black")
weather
aggregate(count~weather, train_factor, mean)
summary(train_factor$weather)

#visualizing casual and registered (casual + registered = count)
aggregate(cbind(casual, casual/count, registered, registered/count) ~ day, train_factor, mean)
aggregate(cbind(casual, casual/count, registered, registered/count) ~ month, train_factor, mean)

#correlation matrix
set.seed(100)
tf_subset <- train_factor[, c(3:15)]
matrix <- ggpairs(tf_subset[sample.int(nrow(tf_subset), 1000), ])
matrix

