# Setup -------------------------------------------------------------------
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

<<<<<<< HEAD
# final grades
ggplot(data, aes(x=G3)) + geom_bar() + 
    labs(x = 'Final Grade', y = '# of Students', title = 'Distribution of Final Grades')
=======
# factors
data <- mutate(data, Dalc = factor(Dalc),
               Walc = factor(Walc),
               health = factor(health),
               goout = factor(goout),
               studytime = factor(studytime))


# Schools -----------------------------------------------------------------

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


# Gender ------------------------------------------------------------------

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


# Age ---------------------------------------------------------------------

# age affect on grades?
ggplot(data, aes(x = age, y = G3)) + geom_jitter() +geom_smooth(method = "lm")
# meh, much older tends to have medium or low


# age and alcohol consumption?
ggplot(data, aes(x = age, fill = Dalc)) + geom_bar(position = "fill") + scale_y_continuous(labels = percent)

ggplot(data, aes(x = age, fill = Walc)) + geom_bar(position = "fill") + scale_y_continuous(labels = percent)
# drinking for all groups goes up on the weekend, kinda a peak at age 17


# Drinking ----------------------------------------------------------------

# more drinking == poorer health?
ggplot(data, aes(x = Walc, fill = health)) + geom_bar(position = "fill") + scale_y_continuous(labels = percent)
ggplot(data, aes(x = Dalc, fill = health)) + geom_bar(position = "fill") + scale_y_continuous(labels = percent)
# not really clear, maybe even opposite of hwat you would've thought


# correlation between drinking and going out?
ggplot(data, aes(x = Walc, fill = goout)) + geom_bar(position = "fill") + scale_y_continuous(labels = percent)
ggplot(data, aes(x = Dalc, fill = goout)) + geom_bar(position = "fill") + scale_y_continuous(labels = percent)
# yeah, positive relationship that you would've expected



# Parent Education/Job --------------------------------------------------------

# affect on grades
ggplot(data, aes(x = Medu, y = G3)) + geom_jitter() + geom_smooth(method = "lm")
ggplot(data, aes(x = Fedu, y = G3)) + geom_jitter() + geom_smooth(method = "lm")
# positive relation for both, stteper for Medu


# affect on student wanted to persue higher edu?
ggplot(data, aes(x = Medu, fill = higher)) + geom_bar(position = "fill") + scale_y_continuous(labels = percent)
ggplot(data, aes(x = Fedu, fill = higher)) + geom_bar(position = "fill") + scale_y_continuous(labels = percent)
# definite rise in interest for increase in Medu


# affect on the type of job they hold?
ggplot(data, aes(x = Medu, fill = Mjob)) + geom_bar(position = "fill") + scale_y_continuous(labels = percent)
ggplot(data, aes(x = Fedu, fill = Fjob)) + geom_bar(position = "fill") + scale_y_continuous(labels = percent)
# lots of teachers/health at high levels, more at home at low levels of edu


# type of job and who is at home
ggplot(data, aes(x = Fjob, fill = guardian)) + geom_bar(position = "fill") + scale_y_continuous(labels = percent)
# interestingly not always the one who is at home, maybe cohabitation status?
ggplot(data, aes(x = Fjob, fill = guardian)) + geom_bar(position = "fill") + facet_wrap(~Pstatus)+ scale_y_continuous(labels = percent)
# if they are apart, mother is far more likely to be guardian, except when father works in health


# parent's edu and educational support
ggplot(data, aes(x = Medu, fill = famsup)) + geom_bar(position = "fill") + scale_y_continuous(labels = percent)
ggplot(data, aes(x = Fedu, fill = famsup)) + geom_bar(position = "fill") + scale_y_continuous(labels = percent)
# positive relation, as would be expected



# Study Time --------------------------------------------------------------

# study time on final grade
ggplot(data, aes(x = studytime, y = G3)) + geom_jitter() +geom_smooth(method = "lm")
# positive relationship, as expected


# going out effect on studytime
ggplot(data, aes(x = goout, fill = studytime)) + geom_bar(position = "fill") + scale_y_continuous(labels = percent)
# no clear trend


# romantic relationship effect on studytime
ggplot(data, aes(x = romantic, fill = studytime)) + geom_bar(position = "fill") + scale_y_continuous(labels = percent)
# interestingly, those in a relationship study more



# Failures/Absences -------------------------------------------------------

# failures and grades
ggplot(data, aes(x = failures, y = G3)) + geom_jitter()
# negative correlation as expected


# absences and grades
ggplot(data, aes(x = absences, y = G3)) + geom_jitter()
# negative correlation as expected


# failures and bad health?
ggplot(data, aes(x = health, y = absences)) + geom_jitter()
# absences probably not caused by bad health

# absences from difficultly getting to school?
ggplot(data, aes(x = traveltime, y = absences)) + geom_jitter()
# nope


# Extra Help --------------------------------------------------------------

# internet at home on grade
ggplot(data, aes(x = internet, y = G3)) + geom_boxplot() 
# access to internet == ~1 pt higher score


# extra school support on grade 
ggplot(data, aes(x = schoolsup, y = G3)) + geom_boxplot()
# maybe they only give extra support to students who are struggling to begin with?
ggplot(data, aes(x = schoolsup, y = G1)) + geom_boxplot()
# not too much different in scores at first examination


# nursery school attendence on grade
ggplot(data, aes(x = nursery, y = G3)) + geom_boxplot()
# nursery school == ~1 pt higher


# extra paid classes on grade
ggplot(data, aes(x = paid, y = G3)) + geom_boxplot()
# actually lower for extra paid classes, too much on their plates? Or only students who would
# need extra classes take them


# Original Lin Regs ----------------------------------------------------------------
>>>>>>> cd597dd7c88aa25aaa32056b6c59714bd95ab34f

# weekly study time
ggplot(data, aes(x = studytime, y = G3, color = class, shape = class))+ geom_jitter() + 
    geom_smooth(method = "lm") + 
    labs(x = 'Weekly Study Time', y = 'Final Grade', title = 'Studying Improves Final Grades')

lin_reg1 <- lm(G3 ~ studytime, data)
summary(lin_reg1)

# workday alcohol consumption
ggplot(data, aes(x = Dalc, y = G3, color = class, shape = class)) +  geom_jitter() + geom_smooth(method = "lm") +
    labs(x = 'Workday Alcohol Consumption', y = 'Final Grade', 
         title = 'Weekday Drinking Harms Final Grades')

<<<<<<< HEAD
# weekend alcohol consumption
ggplot(data, aes(x = Walc, y = G3, color = class, shape = class)) +  geom_jitter() + geom_smooth(method = "lm") +
    labs(x = 'Weekend Alcohol Consumption', y = 'Final Grade', 
         title = 'Weekend Drinking Harms Final Grades')
=======
lin_reg2 <- lm(G3 ~ Dalc, data)
summary(lin_reg2)

# weekday alcohol consumption
ggplot(data, aes(x = Walc, y = G3)) +  geom_jitter() + geom_smooth(method = "lm") +
    labs(x = 'Weekday Alcohol Consumption', y = 'Final Grade', 
         title = 'Weekday Drinking Harms Final Grades')
>>>>>>> cd597dd7c88aa25aaa32056b6c59714bd95ab34f

lin_reg3 <- lm(G3 ~ Walc, data)
summary(lin_reg3)

# health
<<<<<<< HEAD
ggplot(data, aes(x = health, y = G3, color = class, shape = class)) +  geom_jitter() + geom_smooth(method = "lm") +
    labs(x = 'Health Status', y = 'Final Grade', title = 'Final Grade Not Impacted by Health')
=======
ggplot(data, aes(x = health, y = G3)) +  geom_jitter() + geom_smooth(method = "lm")

lin_reg4 <- lm(G3 ~ health, data)
summary(lin_reg4)
>>>>>>> cd597dd7c88aa25aaa32056b6c59714bd95ab34f
