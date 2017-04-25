library(tidyverse)

# Read in data
math <- read.table("student-mat.csv", sep=";", header=TRUE)
port <- read.table("student-por.csv", sep=";", header=TRUE)

# merge data
math <- mutate(math, class = "M")
port <- mutate(port, class = "P")

data <- rbind(math, port)


# remove those with 0 as G3 score
group_by(data, G3) %>%
    summarize(cnt = n())

data <- data[data$G3 != 0,]
data <- data[data$G3 != 1,]

# final grades
ggplot(data, aes(x=G3)) + geom_bar() + 
    labs(x = 'Final Grade', y = '# of Students', title = 'Distribution of Final Grades')

# weekly study time
ggplot(data, aes(x = studytime, y = G3, color = class, shape = class))+ geom_jitter() + 
    geom_smooth(method = "lm") + 
    labs(x = 'Weekly Study Time', y = 'Final Grade', title = 'Studying Improves Final Grades')

# workday alcohol consumption
ggplot(data, aes(x = Dalc, y = G3, color = class, shape = class)) +  geom_jitter() + geom_smooth(method = "lm") +
    labs(x = 'Workday Alcohol Consumption', y = 'Final Grade', 
         title = 'Weekday Drinking Harms Final Grades')

# weekend alcohol consumption
ggplot(data, aes(x = Walc, y = G3, color = class, shape = class)) +  geom_jitter() + geom_smooth(method = "lm") +
    labs(x = 'Weekend Alcohol Consumption', y = 'Final Grade', 
         title = 'Weekend Drinking Harms Final Grades')

# health
ggplot(data, aes(x = health, y = G3, color = class, shape = class)) +  geom_jitter() + geom_smooth(method = "lm") +
    labs(x = 'Health Status', y = 'Final Grade', title = 'Final Grade Not Impacted by Health')
