library(tidyverse)
library(scales)

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

# factors
data <- mutate(data, Dalc = factor(Dalc),
               Walc = factor(Walc))


# is there a difference in success based on the schools?
ggplot(data, aes(x = school, y = G3)) + geom_boxplot()
# students at Gabriel Pereira do ~1 point better on average than Mousinho da Silveira


# Is this because one school has more urban/rural families?
ggplot(data, aes(x = school, fill = address)) + geom_bar(position = "fill") + scale_y_continuous(labels = percent)
# MS is about 50/50, GP is 80/20


# Reason why people pick each school?
ggplot(data, aes(x = school, fill = reason)) + geom_bar(position = "fill") + scale_y_continuous(labels = percent)
# 28% chose GP for reputation, only 12% for MS
# 52% chose MS for course, 37% for GP


# does one gender perform better?
ggplot(data, aes(x = sex, y = G3)) + geom_boxplot()
# difference of ~1 point toward females


# difference in drinking habits between the genders
ggplot(data, aes(x = sex, fill = Dalc)) + geom_bar(position = "fill") + scale_y_continuous(labels = percent)

ggplot(data, aes(x = sex, fill = Walc)) + geom_bar(position = "fill") + scale_y_continuous(labels = percent)
# males drink more heavily during workdays and weekends, both increase consumption on weekends


# which gender more likely to be in romantic relationship?
ggplot(data, aes(x = sex, fill = romantic)) + geom_bar(position = "fill") + scale_y_continuous(labels = percent)
# 38% yes in males, 28% females

# affect on higher education?
ggplot(data, aes(x = sex, fill = higher)) + geom_bar(position = "fill") + scale_y_continuous(labels = percent)
# 93 F to 89 male


# age affect on grades?
ggplot(data, aes(x = age, y = G3)) + geom_jitter() +geom_smooth(method = "lm")
# meh, much older tends to have medium or low

# age and alcohol consumption?
ggplot(data, aes(x = age, fill = Dalc)) + geom_bar(position = "fill") + scale_y_continuous(labels = percent)

ggplot(data, aes(x = age, fill = Walc)) + geom_bar(position = "fill") + scale_y_continuous(labels = percent)
# drinking for all groups goes up on the weekend, kinda a peak at age 17






# weekly study time
ggplot(data, aes(x = studytime, y = G3))+ geom_jitter() + geom_smooth(method = "lm")

lin_reg1 <- lm(G3 ~ studytime, data)
summary(lin_reg1)

# workday alcohol consumption
ggplot(data, aes(x = Dalc, y = G3)) +  geom_jitter() + geom_smooth(method = "lm")

lin_reg2 <- lm(G3 ~ Dalc, data)
summary(lin_reg2)

# weekday alcohol consumption
ggplot(data, aes(x = Walc, y = G3)) +  geom_jitter() + geom_smooth(method = "lm")

lin_reg3 <- lm(G3 ~ Walc, data)
summary(lin_reg3)

# health
ggplot(data, aes(x = health, y = G3)) +  geom_jitter() + geom_smooth(method = "lm")

lin_reg4 <- lm(G3 ~ health, data)
summary(lin_reg4)
