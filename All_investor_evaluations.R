
## Thesis DSS
## Isabelle van Olphen - u308140
## Investor evaluation data
## MULTINOMIAL LOGISTIC REGRESSION AND CLASSIFICATION DECISION TREE

#-------------------------------------------------------------------------------
# LOAD LIBRARIES
library("readxl")
library("dplyr")
library("ggplot2")
library("caret")
library("nnet")
library("rpart")
library("rpart.plot")
library("MLmetrics")


#-------------------------------------------------------------------------------

# Read in the investor evaluation survey data-----------------------------------
evaluation_survey_1 <- read_excel('/Startups1819_evaluations+rankings_allinvestors.xlsx') # provide relative path

evaluation_survey_2 <- read_excel('/DEiAIII1920_SM1_evaluations+rankings_allinvestors.xlsx') # provide relative path

evaluation_survey_3 <- read_excel('/Startups1920_1_evaluations+rankings_allinvestors.xlsx') # provide relative path

evaluation_survey_4 <- read_excel('/Startups1920_2_evaluations+rankings_allinvestors.xlsx') # provide relative path


# Check the dimensions of the data files
dim(evaluation_survey_1) # 22 x 40
dim(evaluation_survey_2) # 16 x 40
dim(evaluation_survey_3) # 25 x 40
dim(evaluation_survey_4) # 22 x 40


# Explore data files
head(evaluation_survey_1)
head(evaluation_survey_2)
head(evaluation_survey_3)
head(evaluation_survey_4)


# Summarize data files
summary(evaluation_survey_1)
summary(evaluation_survey_2)
summary(evaluation_survey_3)
summary(evaluation_survey_4)


# Join the data files
all_investor_evaluations <- full_join(evaluation_survey_1,
                                      evaluation_survey_2, by = NULL)
all_investor_evaluations <- full_join(all_investor_evaluations,
                                      evaluation_survey_3, by = NULL)
all_investor_evaluations <- full_join(all_investor_evaluations,
                                      evaluation_survey_4, by = NULL)


# Select the relevant variables
all_investor_evaluations <- all_investor_evaluations %>%
  select(wave, pitch, student, investor, probinv,
         quality, desirability, feasibility)


# Remove redundant rows
# (redundant rows are the ones with the entire question instead of the answer)
all_investor_evaluations <- all_investor_evaluations[-c(1, 23, 39, 64),]


# Remove rows of startups Schwifty Shopping and prAItor that did not consent
all_investor_evaluations <- all_investor_evaluations %>%
  filter(str_detect(pitch, "Schwifty Shopping", negate = TRUE))

all_investor_evaluations <- all_investor_evaluations %>%
  filter(str_detect(pitch, "prAItor", negate = TRUE))

dim(all_investor_evaluations)


# Check for missing values
which(is.na(all_investor_evaluations)) # No missing values


# Check independent and dependent variables for possible outliers
table(all_investor_evaluations$probinv, useNA = 'always')
table(all_investor_evaluations$quality, useNA = 'always')
table(all_investor_evaluations$desirability, useNA = 'always')
table(all_investor_evaluations$feasibility, useNA = 'always')

ggplot(all_investor_evaluations, aes(x = wave , color = probinv)) +
  geom_bar() # Probability of investment
ggplot(all_investor_evaluations, aes(x = wave , color = quality)) +
  geom_bar() # Overall quality of business idea
ggplot(all_investor_evaluations, aes(x = wave , color = desirability)) +
  geom_bar() # Desirability of product/service
ggplot(all_investor_evaluations, aes(x = wave , color = feasibility)) +
  geom_bar() # Feasibility of product/service
# No outliers detected


# Make new column with probability of investment
# from percentages to 3 categories
all_investor_evaluations$probinv <- as.numeric(all_investor_evaluations$probinv)

class(all_investor_evaluations$probinv)
         
all_investor_evaluations$inv_prob <- cut(all_investor_evaluations$probinv,
                                     breaks = c(0,49,69,100),
                                     labels = c("low","middle","high"),
                                     include.lowest = TRUE)


# Checking investment probability class distribution
table(all_investor_evaluations$inv_prob)
# low = 49
# middle = 18
# high = 8


# Plotting investment probability class distribution
ggplot(all_investor_evaluations, aes(x = inv_prob , fill = inv_prob)) +
  xlab("Investment probability class") +
  ylab("Number of pitches") +
  theme(legend.key = element_rect(fill = "white"),
        legend.position = c(0.8, 0.8),
        panel.background = element_rect(fill = "white"),
        legend.title = element_blank(),
        
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(color = "black"),
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black")) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  geom_bar(width = 0.8) +
  geom_text(stat='count', aes(label=..count..), vjust = 1.5)



# Add new column for Maximum Recurrence rates per pitcher-investor combination
all_investor_evaluations[,"max_rec"] <- c(maxrec_pitch1_judge1,
                                         maxrec_pitch1_judge2,
                                         maxrec_pitch1_judge3,
                                         maxrec_pitch2_judge1,
                                         maxrec_pitch2_judge2,
                                         maxrec_pitch2_judge3,
                                         maxrec_pitch3_judge1,
                                         maxrec_pitch3_judge2,
                                         maxrec_pitch3_judge3,
                                         maxrec_pitch4_judge1,
                                         maxrec_pitch4_judge2,
                                         maxrec_pitch4_judge3,
                                         maxrec_pitch5_judge1,
                                         maxrec_pitch5_judge2,
                                         maxrec_pitch5_judge3,
                                         maxrec_pitch6_judge1,
                                         maxrec_pitch6_judge2,
                                         maxrec_pitch6_judge3,
                                         maxrec_pitch7_judge1,
                                         maxrec_pitch7_judge2,
                                         maxrec_pitch7_judge3,
                                         maxrec_pitch8_judge1,
                                         maxrec_pitch8_judge2,
                                         maxrec_pitch8_judge3,
                                         maxrec_pitch9_judge1,
                                         maxrec_pitch9_judge2,
                                         maxrec_pitch9_judge3,
                                         maxrec_pitch10_judge1,
                                         maxrec_pitch10_judge2,
                                         maxrec_pitch10_judge3,
                                         maxrec_pitch11_judge1,
                                         maxrec_pitch11_judge2,
                                         maxrec_pitch11_judge3,
                                         maxrec_pitch12_judge1,
                                         maxrec_pitch12_judge2,
                                         maxrec_pitch12_judge3,
                                         maxrec_pitch13_judge1,
                                         maxrec_pitch13_judge2,
                                         maxrec_pitch13_judge3,
                                         maxrec_pitch14_judge1,
                                         maxrec_pitch14_judge2,
                                         maxrec_pitch14_judge3,
                                         maxrec_pitch15_judge1,
                                         maxrec_pitch15_judge2,
                                         maxrec_pitch15_judge3,
                                         maxrec_pitch16_judge1,
                                         maxrec_pitch16_judge2,
                                         maxrec_pitch16_judge3,
                                         maxrec_pitch17_judge1,
                                         maxrec_pitch17_judge2,
                                         maxrec_pitch17_judge3,
                                         maxrec_pitch18_judge1,
                                         maxrec_pitch18_judge2,
                                         maxrec_pitch18_judge3,
                                         maxrec_pitch19_judge1,
                                         maxrec_pitch19_judge2,
                                         maxrec_pitch19_judge3,
                                         maxrec_pitch20_judge1,
                                         maxrec_pitch20_judge2,
                                         maxrec_pitch20_judge3,
                                         maxrec_pitch21_judge1,
                                         maxrec_pitch21_judge2,
                                         maxrec_pitch21_judge3,
                                         maxrec_pitch22_judge1,
                                         maxrec_pitch22_judge2,
                                         maxrec_pitch22_judge3,
                                         maxrec_pitch23_judge1,
                                         maxrec_pitch23_judge2,
                                         maxrec_pitch23_judge3,
                                         maxrec_pitch24_judge1,
                                         maxrec_pitch24_judge2,
                                         maxrec_pitch24_judge3,
                                         maxrec_pitch25_judge1,
                                         maxrec_pitch25_judge2,
                                         maxrec_pitch25_judge3)

# Checking the descriptive statistics of max_rec column
min(all_investor_evaluations$max_rec) # min = 26.30
max(all_investor_evaluations$max_rec) # max = 88.95
mean(all_investor_evaluations$max_rec) # mean = 66.68
sd(all_investor_evaluations$max_rec) # SD = 12.77
median(all_investor_evaluations$max_rec) # median = 66.84

class(all_investor_evaluations$inv_prob) # checking whether it's a factor




# Data split--------------------------------------------------------------------

# Creating a train and test set and setting a seed to make the random sampling
# reproducible

set.seed(123)
train_index = createDataPartition(y = as.vector(all_investor_evaluations$inv_prob),
                                  p = 0.75, list = FALSE)

training_set = all_investor_evaluations[train_index, ]
test_set = all_investor_evaluations[-train_index, ]

dim(training_set); dim(test_set)


# Plotting investment probability class distribution in training set
ggplot(training_set, aes(x = inv_prob , fill = inv_prob)) +
  xlab("Investment probability class") +
  ylab("Number of pitches") +
  theme(legend.key = element_rect(fill = "white"),
        legend.position = c(0.8, 0.8),
        panel.background = element_rect(fill = "white"),
        legend.title = element_blank(),
        
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(color = "black"),
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black")) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  geom_bar(width = 0.8) +
  geom_text(stat='count', aes(label=..count..), vjust = 1.5)


# Plotting investment probability class distribution in test set
ggplot(test_set, aes(x = inv_prob , fill = inv_prob)) +
  xlab("Investment probability class") +
  ylab("Number of pitches") +
  theme(legend.key = element_rect(fill = "white"),
        legend.position = c(0.8, 0.8),
        panel.background = element_rect(fill = "white"),
        legend.title = element_blank(),
        
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(color = "black"),
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black")) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  geom_bar(width = 0.8) +
  geom_text(stat='count', aes(label=..count..), vjust = 1.5)




########## M U L T I N O M I A L  L O G I S T I C  R E G R E S S I O N #########

# Setting the baseline to the majority class: low investment probability
training_set$inv_prob <- relevel(training_set$inv_prob, ref = "low")


# Scaling the independent variables to make them fit for the MLGR
training_set$quality_scaled <- scale(as.numeric(training_set$quality),
                                     center = TRUE, scale = TRUE)
training_set$desirability_scaled <- scale(as.numeric(training_set$desirability),
                                          center = TRUE, scale = TRUE)
training_set$feasibility_scaled <- scale(as.numeric(training_set$feasibility),
                                         center = TRUE, scale = TRUE)
training_set$max_rec_scaled <- scale(as.numeric(training_set$max_rec),
                                     center = TRUE, scale = TRUE)


test_set$quality_scaled <- scale(as.numeric(test_set$quality),
                                 center = TRUE, scale = TRUE)
test_set$desirability_scaled <- scale(as.numeric(test_set$desirability),
                                      center = TRUE, scale = TRUE)
test_set$feasibility_scaled <- scale(as.numeric(test_set$feasibility),
                                     center = TRUE, scale = TRUE)
test_set$max_rec_scaled <- scale(as.numeric(test_set$max_rec),
                                 center = TRUE, scale = TRUE)

head(training_set)
head(test_set)



#################### PREDICT TRAINING SET BASELINE MODEL #######################

# Fitting a baseline multinomial logistic regression model on the training data 
# using 10-fold cross-validation and a BFGS optimizer
set.seed(123)
invest_mlgr_base <- multinom(inv_prob ~ quality_scaled
                             + desirability_scaled
                             + feasibility_scaled,
                             data = training_set,
                             trControl = trainControl(method = "cv",
                                                      number = 10,
                                                      search = "grid"),
                             method='bfgs')


# Summarizing and checking the baseline MLGR model
sum_mlgr_base <- summary(invest_mlgr_base)


# Calculating z-scores and p-values
z <- sum_mlgr_base$coefficients/sum_mlgr_base$standard.errors
p <- (1-pnorm(abs(z),0,1))*2

print(z, digits =2)
print(p, digits =2)


# Checking the odds ratios of the predictors
exp(coef(invest_mlgr_base))


# Checking the output probabilities
head(probability.table <- fitted(invest_mlgr_base))


# Predicting the investment probability class for the training set
training_set$predict_mlgr_base <- predict(invest_mlgr_base,
                                          newdata = training_set, "class")


# Building a classification table
classtable_mlgr_base <- table(
  training_set$predict_mlgr_base, training_set$inv_prob)


# Calculating accuracy
train_acc_base_mlgr <- round((sum(diag(classtable_mlgr_base))/
                                sum(classtable_mlgr_base))*100, 2)
# accuracy = 75.44 %


# Calculating F1 score
train_f1_base_mlgr <- round(F1_Score(training_set$inv_prob,
                                     training_set$predict_mlgr_base,
                                     positive = NULL)*100, 2)
# F1 = 83.78 %





###################### PREDICT TEST SET BASELINE MODEL #########################

# Predicting the values for test set
test_set$predict_mlgr_base <- predict(invest_mlgr_base,
                                      newdata = test_set, "class")


# Building classification table
ctable_mlgr_base <- table(test_set$predict_mlgr_base, test_set$inv_prob)


# Calculating accuracy
test_acc_base_mlgr <- round((sum(diag(ctable_mlgr_base))/
                               sum(ctable_mlgr_base))*100,2) 
# accuracy = 72.22 %


# Calculating F1 score
test_f1_base_mlgr <- round(F1_Score(test_set$inv_prob,
                                     test_set$predict_mlgr_base,
                                     positive = NULL)*100, 2)
# F1 = 96 %




###################### PREDICT TRAINING SET FULL MODEL #########################

# Fitting a multinomial logistic regression model with the max recurrence rates
# between pitchers and jury members (full model) on the training data using
# 10-fold cross-validation and a BFGS optimizer
set.seed(123)
invest_mlgr <- multinom(inv_prob ~ quality_scaled
                        + desirability_scaled
                        + feasibility_scaled
                        + max_rec_scaled,
                        data = training_set,
                        trControl = trainControl(method = "cv",
                                                 number = 10,
                                                 search = "grid"),
                        method='bfgs')


# Summarizing and checking the full MLGR model
sum_mlgr <- summary(invest_mlgr)


# Calculating z-scores and p-values
zz <- sum_mlgr$coefficients/sum_mlgr$standard.errors
pp <- (1-pnorm(abs(zz),0,1))*2

print(zz, digits =2)
print(pp, digits =2)


# Checking the odds ratios of the predictors
exp(coef(invest_mlgr))


# Checking the outpur probabilities
head(probability.table <- fitted(invest_mlgr))


# Predicting the investment probability class for the training set
training_set$predict_mlgr <- predict(invest_mlgr,
                                     newdata = training_set, "class")


# Building a classification table
classtable_mlgr <- table(training_set$predict_mlgr, training_set$inv_prob)


# Calculating accuracy
train_acc_mlgr <- round((sum(diag(classtable_mlgr))/
                           sum(classtable_mlgr))*100, 2)
# accuracy = 75.44 %


# Calculating F1 score
train_f1_mlgr <- round(F1_Score(training_set$inv_prob,
                                training_set$predict_mlgr,
                                positive = NULL)*100, 2)
# F1 = 83.78 %




######################## PREDICT TEST SET FULL MODEL ###########################

# Predicting the values for test dataset
test_set$predict_mlgr <- predict(invest_mlgr, newdata = test_set, "class")


# Building classification table
ctable_mlgr <- table(test_set$predict_mlgr, test_set$inv_prob)


# Calculating accuracy
test_acc_mlgr <- round((sum(diag(ctable_mlgr))/
                          sum(ctable_mlgr))*100,2)
# accuracy = 72.22 %


# Calculating F1 score
test_f1_mlgr <- round(F1_Score(test_set$inv_prob,
                               test_set$predict_mlgr,
                               positive = NULL)*100, 2)
# F1 = 96 %





############ C L A S S I F I C A T I O N  D E C I S I O N  T R E E #############

# Making all independent variables numerical instead of characters
training_set$quality <- as.numeric(training_set$quality)
training_set$desirability <- as.numeric(training_set$desirability)
training_set$feasibility <- as.numeric(training_set$feasibility)
training_set$max_rec <- as.numeric(training_set$max_rec)

test_set$quality <- as.numeric(test_set$quality)
test_set$desirability <- as.numeric(test_set$desirability)
test_set$feasibility <- as.numeric(test_set$feasibility)
test_set$max_rec <- as.numeric(test_set$max_rec)




#################### PREDICT TRAINING SET BASELINE MODEL #######################

# Fitting a baseline classification decision tree model on the training data 
# using 10-fold cross-validation and the Gini Index
set.seed(123)
invest_cdt_base <- rpart(inv_prob ~ quality + desirability + feasibility,
                         data = training_set,
                         minsplit = 20,
                         method = "class",
                         parms = list(split = "gini"),
                         xval = 10)


# prune tree with the optimal complexity parameter
invest_cdt_base <- prune.rpart(invest_cdt_base, cp = 0.01)


# Print CDT model
print(invest_cdt_base)
printcp(invest_cdt_base) # root node error = 35.09 %


# Summarizing and checking the baseline CDT model
summary(invest_cdt_base)


# Display pruning plot
plotcp(invest_cdt_base)


# Plotting the tree model
rpart.plot(invest_cdt_base)


# Printing the rules
rpart.rules(invest_cdt_base)



# Predicting the investment probability for the training set
training_set$predict_cdt_base <- predict(invest_cdt_base,
                                         newdata = training_set, "class")


# Building a classification table
classtable_cdt_base <- table(training_set$predict_cdt_base,
                             training_set$inv_prob)


# Calculating accuracy
train_acc_base_cdt <- round((sum(diag(classtable_cdt_base))/
                               sum(classtable_cdt_base))*100, 2)
# accuracy = 75.44 %


# Calculating F1 score
train_f1_base_cdt <- round(F1_Score(training_set$inv_prob,
                                     training_set$predict_cdt_base,
                                     positive = NULL)*100, 2)
# F1 = 87.18 %





###################### PREDICT TEST SET BASELINE MODEL #########################

# Predicting the values for test dataset
test_set$predict_cdt_base <- predict(invest_cdt_base,
                                     newdata = test_set,
                                     "class")


# Building classification table
ctable_cdt_base <- table(test_set$predict_cdt_base, test_set$inv_prob)


# Calculating accuracy
test_acc_base_cdt <- round((sum(diag(ctable_cdt_base))/
                              sum(ctable_cdt_base))*100,2)
# accuracy = 77.78 %


# Calculating F1 score
test_f1_base_cdt <- round(F1_Score(test_set$inv_prob,
                                   test_set$predict_cdt_base,
                                   positive = NULL)*100, 2)
# F1 = 91.67 %




#################### PREDICT TRAINING SET FULL MODEL #######################

# Fitting a classification decision tree model with the max recurrence rate
# between pitchers and jury members (full model) on the training data using
# 10-fold cross-validation and the Gini Index
set.seed(123)
invest_cdt <- rpart(inv_prob ~ quality + desirability + feasibility
                    + max_rec,
                    data = training_set,
                    minsplit = 20,
                    method = "class",
                    parms = list(split = "gini"),
                    xval = 10)


# Prune tree with optimal com plexity parameter
invest_cdt <- prune.rpart(invest_cdt, cp = 0.01)


# Print CDT model
print(invest_cdt)
printcp(invest_cdt) # root node error = 35.09 %


# Summarizing and checking the baseline CDT model
summary(invest_cdt)


# Display pruning plot
plotcp(invest_cdt)


# Plotting the tree model
rpart.plot(invest_cdt)


# Printing the rules
rpart.rules(invest_cdt)



# Predicting the investment probability for the training set
training_set$predict_cdt <- predict(invest_cdt, newdata = training_set, "class")


# Building a classification table
classtable_cdt <- table(training_set$predict_cdt, training_set$inv_prob)


# Calculating accuracy
train_acc_cdt <- round((sum(diag(classtable_cdt))/sum(classtable_cdt))*100, 2)
# accuracy = 78.95 %


# Calculating F1 score
train_f1_cdt <- round(F1_Score(training_set$inv_prob,
                               training_set$predict_cdt,
                               positive = NULL)*100, 2)
# F1 = 88.31 %





######################## PREDICT TEST SET FULL MODEL ###########################

# Predicting the values for the test set
test_set$predict_cdt <- predict(invest_cdt, newdata = test_set, "class")


# Building classification table
ctable_cdt <- table(test_set$predict_cdt, test_set$inv_prob)


# Calculating accuracy
test_acc_cdt <- round((sum(diag(ctable_cdt))/sum(ctable_cdt))*100,2)
# accuracy = 72.22 %


# Calculating F1 score
test_f1_cdt <- round(F1_Score(test_set$inv_prob,
                              test_set$predict_cdt,
                              positive = NULL)*100, 2)
# F1 = 88 %



