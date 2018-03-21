#install packages
library(readr)          # easier reading of flat files
library(readxl)         # easier reading of excel files
library(dplyr)          # data manipulation functions
library(tidyr)          # tools for tidy datasets
library(magrittr)       # ceci n'est pas un pipe
library(lubridate)      # easier manipulation of time objects
library(stringr)        # easier manipulation of strings
library(reshape2)       # a much more flexible reshaping for our purpose here
library(here)           # easy path location and setting
library(ggplot2)        # plot data
library(randomForest)
library(rpart)
library(rpart.plot)
library(bestglm)
library(toOrdinal)
library(Zelig)
library(heatmapFit)
library(caret)
library(lattice)


#read in gathered data
AllData = read_csv("/Users/melissamock/Documents/QMSS-GR5069_Spring2018-master/data_challenges/data/processed/AllViolenceData_171220.csv")

#create dataframe
#Alldata = data.frame(df)





#investigate summary statistics, look for any standout correlations
summary(
  lm(total_people_dead ~ total_people_wounded +
       afi + army + navy + federal_police +
       long_guns_seized+ small_arms_seized + 
       clips_seized + cartridge_sezied, 
     data = AllData) 
)

#variables to focus on
col_vector <- c("total_people_dead", "total_people_wounded", "afi", "army",
                "navy", "federal_police", "long_guns_seized", "small_arms_seized",
                "clips_seized", "cartridge_sezied")

#dataframe of correlation combinations
correlations <- AllData %>% 
  select_(.dots = col_vector) %>%
  cor(.) %>%
  round(2) %>%
  melt()

#plot correlations 
ggplot(correlations, aes(x=Var1, y=Var2, fill= value))+
  geom_tile(color = "white") +
  theme_minimal() +
  scale_x_discrete("") +
  scale_y_discrete("") +
  theme(axis.text.x = element_text(angle =30, vjust =1, hjust =1)) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") 


#investigate the quality of our estimation by looking at the distribution in two ways
ggplot(data = AllData) +
  geom_bar(aes(x=total_people_dead), fill = "blue") +
  theme_minimal() +
  scale_x_continuous("", breaks = c(0, 1,2,3,4,5,10,15,20,30,40),
                     labels = c("0", "1","2","3","4", "5","10","15","20","30", "40")) +
  scale_y_continuous("") +  
  theme(axis.text.y = element_text(size=14), 
        axis.text.x = element_text(size=12),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()
  ) 

ggplot(data = AllData) +
  geom_bar(aes(x=log(civilian_dead+1)), fill = "blue") +
  theme_minimal() +
  scale_x_continuous("", breaks = c(0, 1,2,3,4,5,10,15,20,30),
                     labels = c("0", "1","2","3","4", "5","10","15","20","30")) +
  scale_y_continuous("") +  
  theme(axis.text.y = element_text(size=14), 
        axis.text.x = element_text(size=12),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()
  ) 

#both indicate that the data is zero skewed so there is bias in our estimates. 
#It seems our regression analysis is not as reliable as hoped.

#I need to refine my model by determining which covariates are best for prediction
#select subset with following code 

#group variates to include
newdata <- AllData[, names(AllData) %in% c("total_people_wounded", "afi", "army",
                                           "navy", "federal_police", "long_guns_seized", "small_arms_seized",
                                           "clips_seized", "cartridge_sezied")]
newdata <- cbind(newdata, total_people_dead = AllData$total_people_dead)

#identify best subsets using AIC method
subset_full <- bestglm(newdata, family = gaussian, IC = 'AIC', method = 'exhaustive', TopModels = 10) 

#Top 10 models with low AIC
subset_full$BestModels  


#plot lowest AIC 
barplot(subset_full$BestModels[,'Criterion'], 
        names.arg=sapply(1:10, toOrdinal), 
        xlab = "model ranks", ylab = "AIC", 
        ylim = c(7671, 7675), xpd = FALSE, 
        main = "AIC of suggested models")

#print best model 
subset_full$BestModel

#identify best subsets using BIC method
subset_full_bic <- bestglm(newdata, family = gaussian, IC = 'BIC', 
                           method = 'exhaustive', TopModels = 10) 

#Top 10 models with low BIC
subset_full_bic$BestModels  

#plot lowest BIC
barplot(subset_full_bic$BestModels[,'Criterion'], 
        names.arg=sapply(1:10, toOrdinal), 
        xlab = "model ranks", ylab = "BIC", 
        ylim = c(7700, 7715), xpd = FALSE, 
        main = "BIC of suggested models")

#print best model
subset_full_bic$BestModel

# forward selection
forward_subset = regsubsets(total_people_dead ~ ., data = newdata, 
                            nvmax = 9, method = "forward")
summary_fw <- summary(forward_subset)
which.min(summary_fw$cp)
#plot forward selection 
plot(summary_fw$cp, xlab = "Number of Variables", ylab = "Cp")


#Model Selection with Validation Set
#create test and train sets
set.seed(12910) #set seed
n <- dim(newdata)[1]
ntest <- round(n*0.3) #size of testing data
index <- sample(n,ntest) # indices of testing samples
data_test<- newdata[index,]
data_train <- newdata[-index,]

fit = regsubsets(total_people_dead ~ ., data = data_train, nvmax = 9, method = "forward")

test_error = rep(NA, 9)
#test model 
test_model = model.matrix(total_people_dead ~ ., data = data_test)  
for (i in 1:9) {
  coeff = coef(fit, id = i)
  pred = test_model[, names(coeff)] %*% coeff
  test_error[i] = mean((data_test$total_people_dead - pred)^2)
}
plot(sqrt(test_error), ylab = "Root MSE", ylim=c(2, 2.25), 
     pch = 19, type = "b")
points(sqrt(fit$rss[-1]/3777), col = "blue", 
       pch = 19, type = "b")
legend("topright", legend = c("Training", "Validation"), 
       col = c("blue", "black"), pch = 19)

#validation is not plotted so I'm surious what this means for the MSE. 


#model selection with cross validation
set.seed(4837)
forward_subset_cv = bestglm(newdata, family = gaussian, 
                            IC = 'CV', 
                            CVArgs=list(Method="HTF", K=10, REP=1),
                            method = 'forward')   # 10-Fold Cross Validation
summary(forward_subset_cv$BestModel)

error_CV <- forward_subset_cv$Subsets[,"CV"]
sd_CV<- forward_subset_cv$Subsets[,"sdCV"]
k <- 0:(length(error_CV)-1)

dat <- as.data.frame(cbind(k, error_CV, sd_CV))

#plot model with 10-fold cross validation
ggplot(dat, aes(x=k, y=error_CV)) + 
  geom_errorbar(aes(ymin=error_CV-sd_CV, ymax=error_CV+sd_CV), width=.1, col="blue") +
  geom_line() +
  geom_point()+
  labs(title= "Model selection with 10-fold cross-validation and 1-sd rule", 
       x="Subset Size", y= "CV_error")+
  scale_x_discrete(limits=c(0:9))+
  geom_vline(xintercept = oneSdRule(dat[,c("error_CV", "sd_CV")])-1, col="red", linetype="dotted", size=1)

#create binary variables
AllData$total_people_death <- ifelse(AllData$total_people_dead > 1, 1, 0)


#logistic regression model
logit_model <- 
  glm(total_people_death ~ total_people_wounded +
        navy + federal_police +
        long_guns_seized+ cartridge_sezied, 
      family = binomial(link = "logit"), 
      data = AllData) 
summary(logit_model)

z_out <- zelig(total_people_death ~ total_people_wounded +
                 navy + federal_police +
                 long_guns_seized+ cartridge_sezied, 
               model = "logit", data = AllData, cite = FALSE) 
x_low  <- setx(z_out, total_people_wounded= 0)
x_high <- setx(z_out, total_people_wounded = 30)
sim_out <- sim(z_out, x = x_low, x1 = x_high)

#summary of zelig model
summary(sim_out)

#plot 
plot(sim_out)

#prediction for logistic model
pred <- predict(logit_model, type = "response")
heatmap.fit(AllData$total_people_death, pred, reps = 1000, color = TRUE)
#plt 
ggplot(data = AllData) +
  geom_bar(aes(x=total_people_death,
               y = (..count..)/sum(..count..)), fill = "brown4") +
  scale_y_continuous("", labels = scales::percent) +
  scale_x_continuous("", breaks = c(0, 1),
                     labels = c("no deaths", "deaths")) +
  theme_minimal() +
  theme(axis.text.y = element_text(size=14), 
        axis.text.x = element_text(size=12),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()
  ) 


#create random forest model
violence_forest <- randomForest(formula = total_people_dead ~ total_people_wounded +
               afi + army + navy + federal_police +
               long_guns_seized + small_arms_seized +
               clips_seized + cartridge_sezied,
             data = violence_train, method = "rf",
             importance = TRUE,
             prox = TRUE,
             preProc = c("center", "scale"), na.action = na.roughfix)
#print random forest
print(violence_forest)
#plot forest information
 varImpPlot(violence_forest)
 plot(violence_forest)