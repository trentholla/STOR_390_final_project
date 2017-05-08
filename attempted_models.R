# stepwise selection forward
null1 <- lm(G3 ~ 1,data = train)
full1 <- lm(G3 ~ school + sex + age + address + famsize + Pstatus + Medu + Fedu + Mjob +
                Fjob+ reason + guardian + traveltime + studytime + failures + schoolsup + 
                famsup + paid + activities + nursery + higher + internet + romantic + famrel + 
                freetime + goout + Dalc + Walc + health + absences + G1 + G2 + class,data=train)
model6 <- step(null1, scope = list(lower=null1, upper=full1), direction="forward")
MSE6 <- mean(model6$residuals^2)

#stepwise selection backward
null1 <- lm(G3 ~ 1,data = train)
full1 <- lm(G3 ~ school + sex + age + address + famsize + Pstatus + Medu + Fedu + Mjob +
                Fjob+ reason + guardian + traveltime + studytime + failures + schoolsup + 
                famsup + paid + activities + nursery + higher + internet + romantic + famrel + 
                freetime + goout + Dalc + Walc + health + absences + G1 + G2 + class,data=train)
model7 <- step(null1, scope = list(lower=null1, upper=fill1), direction="backward")
MSE7 <- mean(model7$residuals^2)

# LASSO
library(glmnet)
train1 <- mutate(data, Dalc = factor(Dalc),
               Walc = factor(Walc),
               health = factor(health),
               studytime = factor(studytime),
               famsize = factor(famsize),
               Mjob = factor(Mjob),
               Fjob = factor(Fjob),
               reason = factor(reason),
               guardian = factor(guardian)
               )
myData <- train1[train1$G3 > 4,]
myData <- myData[myData$G3 < 20,]
myData[!complete.cases(myData),]
myData <- na.omit(myData)
x <- as.matrix(myData[,-33]) 
y <- as.double(as.matrix(myData[, 33]))
table(myData$G3)
table(y)

# Fitting the model (Lasso: Alpha = 1)
set.seed(456)
model8 <- cv.glmnet(x, y, family='multinomial', alpha=1)




# Principal Components Regression
require(pls)
set.seed (1678)

pcr_model <- pcr(G3 ~ school + sex + age + address + famsize + Pstatus + Medu + Fedu + Mjob +
                     Fjob+ reason + guardian + traveltime + studytime + failures + schoolsup + 
                     famsup + paid + activities + nursery + higher + internet + romantic + famrel + 
                     freetime + goout + Dalc + Walc + health + absences + G1 + G2 + class, data = train, scale = TRUE, validation = "CV")
model8 <- predict(pcr_model, test, ncomp = 3)
MSE8 <- mean(model8$residuals^2)
