library(tidyverse)

# Read in data
math <- read.table("student-mat.csv", sep=";", header=TRUE)
port <- read.table("student-por.csv", sep=";", header=TRUE)

# merge data to look at general aptitude 
math <- mutate(math, class = "M")
port <- mutate(port, class = "P")

data <- rbind(math, port)


# remove those with 0 or 1 as G3 score
group_by(data, G3) %>%
    summarize(cnt = n())

data <- data[data$G3 != 0,]
data <- data[data$G3 != 1,]


# is there a difference in success based on the schools?
ggplot(data, aes(x = school, y = G3)) + geom_boxplot()
# students at Gabriel Pereira do slight better on average than Mousinho da Silveira


# Is this because one school has more urban/rural families?
group_by(data, address, school) %>%
    summarize(count = n())
# MS is about 50/50, GP is 80/20


# Reason why people pick each school?
group_by(data, school, reason) %>%
    summarize(count = n())
# 28% chose GP for reputation, only 12% for MS
# 52% chose MS for course, 37% for GP



# weekly study time
ggplot(data, aes(x = studytime, y = G3))+ geom_jitter() + geom_smooth(method = "lm")

# workday alcohol consumption
ggplot(data, aes(x = Dalc, y = G3)) +  geom_jitter() + geom_smooth(method = "lm")

# weekday alcohol consumption
ggplot(data, aes(x = Walc, y = G3)) +  geom_jitter() + geom_smooth(method = "lm")

# health
ggplot(data, aes(x = health, y = G3)) +  geom_jitter() + geom_smooth(method = "lm")


