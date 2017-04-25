## Add possible models or work on variable selection here!
library(tidyverse)
library(car)

# there are n observations
n <- dim(hour)[1]

# number of observations that go in the training st
n_tr <- floor(n * .8)


# randomly select n_tr numbers, without replacement, from 1...n
tr_indices <- sample(x=1:n, size=n_tr, replace=FALSE)

# break the data into a non-overlapping train and test set
train <- hour[tr_indices, ]
test <- hour[-tr_indices, ]


#comprehensive linear model 
woah <- lm(G3 ~ school + sex + age + address + famsize + Pstatus + Medu + Fedu + Mjob + Fjob + 
               reason + guardian + traveltime + studytime + failures + schoolsup + famsup + 
               paid + activities + nursery + higher + internet + romantic + famrel + freetime + 
               goout + Dalc + Walc + health + absences + G1 + G2, data)
summary(woah)
vif(woah)

new_grade <- predict(woah, newdata = test)
test <- test %>% 
    mutate(grade_pred=new_grade)
print(test)
#previous test scores have big impact, good health results in lower test scores?
#also, hypothesis confirmed that students who pay for classes do worse, no doubt due to selection bias


#
#