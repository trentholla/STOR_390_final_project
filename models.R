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
               goout + Dalc + Walc + health + absences + G1 + G2 + class, train)
summary(woah)
vif(woah)
MSE1 <- mean(woah$residuals^2)

#previous test scores have big impact, good health results in lower test scores?
#also, hypothesis confirmed that students who pay for classes do worse, no doubt due to selection bias


#other models
woah2 <- lm(G3 ~ age + schoolsup + paid + absences + G1 + G2 + class, train)
summary(woah2)
MSE2 <- mean(woah2$residuals^2)

woah3 <- lm(G3 ~ G1 + G2 + class, train, na.action = na.omit)
summary(woah3)
MSE3 <- mean(woah3$residuals^2)

woah4 <- lm(G3 ~ age + schoolsup + paid + absences + G1 + G2 +I(G1^2) + I(G2^2) + class, train)
summary(woah4)
MSE4 <- mean(woah4$residuals^2)

woah5 <- lm(G3 ~ absences + G1 + G2 + I(G1*G2) + class, train)
summary(woah5)
MSE5 <- mean(woah5$residuals^2)

models <- list(woah, woah2, woah3, woah4, woah5)
#error
error <- tibble(degree=c(1,2,3,4,5),
                MSE_tr=c(MSE1, MSE2, MSE3, MSE4, MSE5))

ggplot(error)+
    geom_point(aes(x=degree, y=MSE_tr)) +
    geom_line(aes(x=degree, y=MSE_tr)) +
    labs(x = 'Model Number', y = 'Training Error', title = 'Training Error for the Models')

#testing
new_grade <- predict(woah3, newdata = test)
test <- test %>% 
    mutate(grade_pred=exp(new_grade))
print(test)

error <- error %>% 
    add_column(MSE_tst=rep(0, 5))


for(i in 1:5){
    
    # grab the trained model
    model <- models[[i]]
    
    # get the predictions for the test data, compute the residuals
    
    test_results <- test %>% 
        mutate(G3_pred = predict(model, newdata=test)) %>% 
        mutate(resid_sq = (G3-G3_pred)^2) 
    
    # compute the MSE
    mst_tst <- summarise(test_results, mse_tst = mean(resid_sq))[[1]]
    
    error[i, 'MSE_tst'] <- mst_tst
}

error %>% 
    rename(tr=MSE_tr, tst=MSE_tst) %>% 
    gather(key=type, value=error, tr, tst) %>% 
    ggplot() +
    geom_point(aes(x=degree, y=log10(error), color=type)) +
    geom_line(aes(x=degree, y=log10(error), color=type)) + 
    labs(x = 'Model Number', y = 'log10(Error)', title = 'Comparison of Training and Test Errors')
error
