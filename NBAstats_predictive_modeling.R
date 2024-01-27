#' Author: Giovanni Ghedini
#' Data: June, 2023
#' Purpose: Forecast and Prediction NBA historical data


library(rpart)
library(rpart.plot)
library(caret)
library(zoo)
library(caret)
library(ggplot2)
library(dplyr)
library(tseries)
library(rugarch)
library(plotly)
library(rugarch)

my_df <- read.csv("/Users/giovannighedini/Desktop/Summer Term/Forcasting using R/Individual Assignment/NBA_Stats_71_Years.csv")

# find the threshold that defines pur business success
median(my_df$Efficency) # 234

#See Nulls
colSums(is.na(my_df))
#See Blanks
colSums(my_df == "")

# 1 if Season Type is "Playoffs". 0 if is "Regular Season"
my_df$S_Type <- ifelse(my_df$Season.Type == "Playoffs", 1, 0)

#### removing business units using NORMALIZATION ("min max") ####
min_max <- function(x){   #define unique function of x
  normalize <- (x-min(x))/(max(x)-min(x)) 
  return(normalize)
}
#### use my unique function for my variables to remove the units ####
my_df$Season.Start.Year_norm <- min_max(my_df$Season.Start.Year)
my_df$Games.Played_norm <- min_max(my_df$Games.Played)
my_df$Minutes.Played_norm <- min_max(my_df$Minutes.Played)
my_df$FG.Made_norm <- min_max(my_df$FG.Made)
my_df$FG.Attempts_norm <- min_max(my_df$FG.Attempts)
my_df$FG.._norm <- min_max(my_df$FG..)
my_df$X3.Pt.FG.Made_norm <- min_max(my_df$X3.Pt.FG.Made)
my_df$X3.Pt.FG.Attempts_norm <- min_max(my_df$X3.Pt.FG.Attempts)
my_df$X3.Pt.FG.._norm <- min_max(my_df$X3.Pt.FG..)
my_df$FT.Made_norm <- min_max(my_df$FT.Made)
my_df$FT.Attempts_norm <- min_max(my_df$FT.Attempts)
my_df$FT.._norm <- min_max(my_df$FT..)
my_df$Offensive.Rebounds_norm <- min_max(my_df$Offensive.Rebounds)
my_df$Defensive.Rebounds_norm <- min_max(my_df$Defensive.Rebounds)
my_df$Rebounds_norm <- min_max(my_df$Rebounds)
my_df$Assists_norm <- min_max(my_df$Assists)
my_df$Steals_norm <- min_max(my_df$Steals)
my_df$Blocks_norm <- min_max(my_df$Blocks)
my_df$Turnovers_norm <- min_max(my_df$Turnovers)
my_df$Personal.Fouls_norm <- min_max(my_df$Personal.Fouls)
my_df$Points.Scored_norm <- min_max(my_df$Points.Scored)
my_df$Efficency_norm <- min_max(my_df$Efficency)
my_df$AST.TOV_norm <- min_max(my_df$AST.TOV)
my_df$STL.TOV_norm <- min_max(my_df$STL.TOV)


# create two data frames for playoffs stats and regular season stats
my_df_playoffs <- filter(my_df, my_df$S_Type == "1")
my_df_regular <- filter(my_df, my_df$S_Type == "0")


### PLAYOFFS ###

## Tranform Rank in a binary variable - PLAYOFFS
# Calculate the 10th percentile threshold
quantile(my_df_playoffs$Rank, 0.15) # 29
my_df_playoffs$rank_success <- ifelse(my_df_playoffs$Rank < 29, 1, 0)

names(my_df_playoffs)
cor_matrix_playoffs <- cor(my_df_playoffs[,c("rank_success", "Season.Start.Year", "Games.Played", "Minutes.Played", "FG.Made",
                      "FG.Attempts", "FG..", "X3.Pt.FG.Made", "X3.Pt.FG.Attempts",
                      "X3.Pt.FG..", "FT.Made", "FT.Attempts", "FT..", "Offensive.Rebounds",
                      "Defensive.Rebounds", "Rebounds", "Assists", "Steals", "Blocks", "Turnovers",
                      "Personal.Fouls", "Points.Scored", "Efficency", "AST.TOV", "STL.TOV")])
corrplot(cor_matrix_playoffs, method="circle", tl.col="black", tl.srt = 45)
## Efficency: (Points Scored + Rebounds + Assists + Steals + Blocks - Missed FG - Missed FT - Turnovers) / Games Played
## they will have a high correlation

## cor without efficency, rebounds(high corr with offensive and defensive reb)
cor_matrix_playoffs1 <- cor(my_df_playoffs[,c("rank_success", "Season.Start.Year", "Games.Played", "FG..",
                                              "X3.Pt.FG..", "FT..",
                                              "Rebounds", "Assists", "Steals", "Blocks", "Turnovers",
                                              "Personal.Fouls", "Points.Scored", "AST.TOV", "STL.TOV")])
corrplot(cor_matrix_playoffs1, method="circle", tl.col="black", tl.srt = 45)


## sampling into TRAINIG and TESTING
training_idx <- sample(1:nrow(my_df_playoffs), size=0.8*nrow(my_df_playoffs))
my_df_train <- my_df_playoffs[training_idx,]
my_df_test <- my_df_playoffs[-training_idx,]

# LOGISTIC REGRESSION
my_logit <- glm(rank_success ~ Season.Start.Year+Games.Played+Minutes.Played+FG.Made
                +FG.Attempts+FG..+X3.Pt.FG.Made+X3.Pt.FG.Attempts
                +X3.Pt.FG..+FT.Made+FT.Attempts+FT..+Offensive.Rebounds
                +Defensive.Rebounds+Assists+Steals+Blocks+Turnovers
                +Personal.Fouls+Points.Scored+AST.TOV+STL.TOV, data = my_df_train, family = "binomial")
summary(my_logit)

# to modify
my_logit <- glm(rank_success ~ Season.Start.Year+FG..
                +X3.Pt.FG..+FT..+Rebounds
                +Assists+Steals+Blocks+Turnovers
                +Personal.Fouls+AST.TOV+STL.TOV, data = my_df_train, family = "binomial")
summary(my_logit)
## FG, 3pt FG, FT, Rebounds, Steals, Blocks, Turnovers and Ast.TOV are statistically significant
my_logit <- glm(rank_success ~ FG..
                +X3.Pt.FG..+FT..+Rebounds
                +Steals+Blocks+Turnovers
                +AST.TOV, data = my_df_train, family = "binomial")
summary(my_logit)

exp(4.914804)-1 # FG % improves the odds of business success by 13529%
exp(1.939156)-1 # 3pt % improves the odds of business success by 595%
exp(5.167341)-1 # FT % improves the odds of business success by 17444%
exp(0.019296)-1 # Rebounds improves odds of business success by 1.95%
exp(0.084769)-1 # Steals improves odds of business success by 8.85%
exp(0.033566)-1 # Blocks improves odds of business success by 3.41%
exp(0.185638)-1 #for every additional Turnover, the odds of business success increases by 20.40%
exp(0.239341)-1 #improving by 1 unit the AST/TOV ratio, increases the odds of business success by 27.04%


# accuracy and performance for LOGISTIC
# use the TESTING SAMPLE
library(caret)
my_prediction <- predict(my_logit, my_df_test, type="response") #probability for every client to bring business success
confusionMatrix(data=as.factor(as.numeric(my_prediction>0.5)), 
                reference=as.factor(as.numeric(my_df_test$rank_success)))
 
##           Reference
#Prediction    0    1
#          0 1486  51
#          1  19   110
##   Accuracy :  0.958 

## NORMALIZED REGRESSION
my_logit_norm <- glm(rank_success ~ FG.._norm
                     +X3.Pt.FG.._norm+FT.._norm+Rebounds_norm
                     +Steals_norm+Blocks_norm+Turnovers_norm
                     +AST.TOV_norm, 
                     data = my_df_train, family = "binomial")
summary(my_logit_norm)
## Top-down ranking of importance: Turnovers (by far), rebounds, Steals, Blocks, FT, AST.TOV, FG, 3pt

## DECISION TREE
my_tree_playoffs <- rpart(rank_success ~ FG..
                          +X3.Pt.FG..+FT..+Rebounds
                          +Steals+Blocks+Turnovers
                          +AST.TOV, data = my_df_train, method="class", cp=0.005) # add "Method = Class" because it's a Classification
#PRINT the tree
rpart.plot(my_tree_playoffs, type=1, extra=1)

# ACCURACY
my_prediction_tree <- predict(my_tree_playoffs, my_df_test, type="prob")
confusionMatrix(data=as.factor(as.numeric(my_prediction_tree[,2]>0.5)), 
                reference=as.factor(as.numeric(my_df_test$rank_success)))

##           Reference
#Prediction    0    1
#          0 1481  61
#          1  24   100
##   Accuracy :  0.949 
## the tree shows a SLIGHTLY LOWER accuracy in predicting results




### REGULAR SEASON ###

## Tranform Rank in a binary variable - REGULAR SEASON
# Calculate the 10th percentile threshold
quantile(my_df_regular$Rank, 0.20) # 86
my_df_regular$rank_success <- ifelse(my_df_regular$Rank < 86, 1, 0)

names(my_df_regular)
cor_matrix <- cor(my_df_regular[,c("rank_success", "Season.Start.Year", "Games.Played", "Minutes.Played", "FG.Made",
                      "FG.Attempts", "FG..", "X3.Pt.FG.Made", "X3.Pt.FG.Attempts",
                      "X3.Pt.FG..", "FT.Made", "FT.Attempts", "FT..", "Offensive.Rebounds",
                      "Defensive.Rebounds", "Rebounds", "Assists", "Steals", "Blocks", "Turnovers",
                      "Personal.Fouls", "Points.Scored", "Efficency", "AST.TOV", "STL.TOV")])
corrplot(cor_matrix, method="circle", tl.col="black", tl.srt = 45)

# Efficency has high correlation with most of the variables (because it's made from a formula that includes them)
# Points scored is high correlated with player's % at shooting, as well as rebounds, assists, fouls and game played 
# FG, 3pt and FT mades have high correlation with the number of attempts --> use the percentage variable

cor_matrix1 <- cor(my_df_regular[,c("rank_success", "Season.Start.Year", "Games.Played", "FG..",
                     "X3.Pt.FG..", "FT..",
                      "Rebounds", "Assists", "Steals", "Blocks", "Turnovers",
                     "Personal.Fouls", "Points.Scored", "AST.TOV", "STL.TOV")])
corrplot(cor_matrix1, method="circle", tl.col="black", tl.srt = 45)

## sampling into TRAINIG and TESTING
training_idx_regular <- sample(1:nrow(my_df_regular), size=0.8*nrow(my_df_regular))
my_df_train_reg <- my_df_regular[training_idx_regular,]
my_df_test_reg <- my_df_regular[-training_idx_regular,]


#LOGISTIC REGRESSION
my_logit_regular <- glm(rank_success ~ Season.Start.Year+Games.Played+Minutes.Played+FG.Made
                +FG.Attempts+FG..+X3.Pt.FG.Made+X3.Pt.FG.Attempts
                +X3.Pt.FG..+FT.Made+FT.Attempts+FT..+Offensive.Rebounds
                +Defensive.Rebounds+Assists+Steals+Blocks+Turnovers
                +Personal.Fouls+Points.Scored+AST.TOV+STL.TOV, data = my_df_train_reg, family = "binomial")
summary(my_logit_regular)

# to modify
my_logit_regular <- glm(rank_success ~ Season.Start.Year+Games.Played+FG..
                +X3.Pt.FG..+FT..+Rebounds
                +Assists+Steals+Blocks+Turnovers
                +Personal.Fouls+AST.TOV+STL.TOV, data = my_df_train_reg, family = "binomial")
summary(my_logit_regular)
# Games.Played is not statistically significant
# block is not statistically significant because high-correlated with rebounds and fouls
# STL.TOV is not statistically significant

my_logit_regular <- glm(rank_success ~ Season.Start.Year+FG..
                        +X3.Pt.FG..+FT..+Rebounds
                        +Assists+Steals+Turnovers
                        +Personal.Fouls+AST.TOV, data = my_df_train_reg, family = "binomial")
summary(my_logit_regular)

exp(2.426e-02)-1 # Year improves the odds of business success by 2.46%
exp(6.599e+00)-1 # FG% improves the odds of business success by 73336%
exp(3.266e+00)-1 # 3pt% improves the odds of business success by 2521%
exp(1.121e+01)-1 # FT% improves odds of business success by 7386441%
exp(4.651e-03)-1 # Rebounds improves odds of business success by 0.47%
exp(-7.850e-03)-1 # Assists improves odds of business success by -0.78%
exp(1.791e-02)-1 # for every additional Steals, the odds of business success increases by 1.81%
exp(5.226e-02)-1 # Turnovers improves the odds of business success by 5.36%
exp(-5.727e-03)-1 #Personal Fouls improves odds of business success by -0.57%
exp(4.043e-01)-1 #AST.TOV improves odds of business success by 49.83%

## Accuracy
my_prediction_reg <- predict(my_logit_regular, my_df_test_reg, type="response") #probability for every client to bring business success
confusionMatrix(data=as.factor(as.numeric(my_prediction_reg>0.5)), 
                reference=as.factor(as.numeric(my_df_test_reg$rank_success)))

##           Reference
#Prediction    0    1
#          0 2849  166
#          1  120  606
##   Accuracy :  0.9235      

## NORMALIZED REGRESSION
my_logit_norm <- glm(rank_success ~ Season.Start.Year_norm+FG.._norm
                     +X3.Pt.FG.._norm+FT.._norm+Rebounds_norm
                     +Assists_norm+Steals_norm+Turnovers_norm
                     +Personal.Fouls_norm+AST.TOV_norm, 
                     data = my_df_train_reg, family = "binomial")
summary(my_logit_norm)
## Top-down ranking of importance: Turnovers(24), FT(11), AST.TOV(8.49), Rebounds(7.1), FG(6.6), Steals(5.4), 3pt%(3.3), Year(1)
## Negative: Assists (-9.1), Personal fouls (-2)
## the numbers are just to give an idea of the relevance each variable has compared to the other


## DECISION TREE
my_tree_regular <- rpart(rank_success ~ Season.Start.Year+FG..
                         +X3.Pt.FG..+FT..+Rebounds
                         +Assists+Steals+Turnovers
                         +Personal.Fouls+AST.TOV, data = my_df_train_reg, method="class", cp=0.003) # add "Method = Class" because it's a Classification
#PRINT the tree
rpart.plot(my_tree_regular, type=1, extra=1)

# ACCURACY
my_prediction_tree_reg <- predict(my_tree_regular, my_df_test_reg, type="prob")
confusionMatrix(data=as.factor(as.numeric(my_prediction_tree_reg[,2]>0.5)), 
                reference=as.factor(as.numeric(my_df_test_reg$rank_success)))
## the tree shows a SLIGHTLY LOWER accuracy in predicting results

##           Reference
#Prediction    0    1
#          0 2800  177
#          1  169  595
##   Accuracy : 0.9075    




##### FORECASTING #####

## PLAYOFFS

# main variables: Turnovers, Steals, Rebounds, FG%, FT% and AST.TOV, Blocks
#group by years
grouped_turnovers_playoffs <- my_df_playoffs %>% group_by(Season.Start.Year) %>% 
  summarize(avg_turnovers = mean(Turnovers,na.rm = TRUE))

grouped_steals_playoffs <- my_df_playoffs %>% group_by(Season.Start.Year) %>% 
  summarize(avg_steals = mean(Steals,na.rm = TRUE))

grouped_rebounds_playoffs <- my_df_playoffs %>% group_by(Season.Start.Year) %>% 
  summarize(avg_rebounds = mean(Rebounds,na.rm = TRUE))

grouped_blocks_playoffs <- my_df_playoffs %>% group_by(Season.Start.Year) %>% 
  summarize(avg_blocks = mean(Blocks,na.rm = TRUE))

grouped_FG_playoffs <- my_df_playoffs %>% group_by(Season.Start.Year) %>% 
  summarize(avg_FG = mean(FG..,na.rm = TRUE))

grouped_FT_playoffs <- my_df_playoffs %>% group_by(Season.Start.Year) %>% 
  summarize(avg_FT = mean(FT..,na.rm = TRUE))

grouped_AST.TOV_playoffs <- my_df_playoffs %>% group_by(Season.Start.Year) %>% 
  summarize(avg_AST.TOV = mean(AST.TOV,na.rm = TRUE))

## visualize CHARTS
ggplot(data=grouped_turnovers_playoffs)+
  geom_line(aes(x=Season.Start.Year, y=avg_turnovers))

ggplot(data=grouped_steals_playoffs)+
  geom_line(aes(x=Season.Start.Year, y=avg_steals))

ggplot(data=grouped_rebounds_playoffs)+
  geom_line(aes(x=Season.Start.Year, y=avg_rebounds))

ggplot(data=grouped_blocks_playoffs)+
  geom_line(aes(x=Season.Start.Year, y=avg_blocks))

# ADF Test
adf.test(grouped_turnovers_playoffs$avg_turnovers) # non-stationary
adf.test(grouped_steals_playoffs$avg_steals) # non-stationary
adf.test(grouped_rebounds_playoffs$avg_rebounds) # non-stationary
adf.test(grouped_blocks_playoffs$avg_blocks) # non-stationary
adf.test(grouped_FG_playoffs$avg_FG) # non-stationary
adf.test(grouped_AST.TOV_playoffs$avg_AST.TOV) # non-stationary

## DECOMPOSE
turnovers_ts <- ts(grouped_turnovers_playoffs[,c("Season.Start.Year", "avg_turnovers")], 
              frequency = 2, start=c(1979,1))
dec <- decompose(turnovers_ts) 
plot(dec)

## ACF - for MA() components
acf(grouped_turnovers_playoffs$avg_turnovers) # MA(2)
acf(grouped_steals_playoffs$avg_steals) # MA(1)
acf(grouped_rebounds_playoffs$avg_rebounds) # MA(1)
acf(grouped_blocks_playoffs$avg_blocks) # MA(1)
acf(grouped_FG_playoffs$avg_FG) # MA(2) or MA(6)
acf(grouped_AST.TOV_playoffs$avg_AST.TOV) # MA(5)
acf(grouped_FT_playoffs$avg_FT) # MA(0)

## PACF - for AR() components
pacf(grouped_turnovers_playoffs$avg_turnovers) # AR(1)
pacf(grouped_steals_playoffs$avg_steals) # AR(1)
pacf(grouped_rebounds_playoffs$avg_rebounds) # AR(1)
pacf(grouped_blocks_playoffs$avg_blocks) # AR(1)
pacf(grouped_FG_playoffs$avg_FG) # AR(1)
pacf(grouped_AST.TOV_playoffs$avg_AST.TOV) # AR(1)
pacf(grouped_FT_playoffs$avg_FT) # AR(0)

# ARIMA
turnovers_arima <- arima(grouped_turnovers_playoffs$avg_turnovers, 
                            order=c(1,0,2)) # c(AR, I, MA) --> I = 0 for stationary and I = 1 for semi-stationary
predict(turnovers_arima, n.ahead =5)
## 9.625756  9.737306  9.885412 10.014240 10.126298
## 1.023996 1.086446 1.142651 1.183404 1.213328

steals_arima <- arima(grouped_steals_playoffs$avg_steals, 
                         order=c(1,0,1)) # c(AR, I, MA) --> I = 0 for stationary and I = 1 for semi-stationary
predict(steals_arima, n.ahead =5)
## 5.518082 5.769874 5.837420 5.855541 5.860401
## 0.5003808 0.5282344 0.5301824 0.5303224 0.5303324

rebounds_arima <- arima(grouped_rebounds_playoffs$avg_rebounds, 
                      order=c(1,0,1)) # c(AR, I, MA) --> I = 0 for stationary and I = 1 for semi-stationary
predict(rebounds_arima, n.ahead =5)
## 32.43335 32.70457 32.87871 32.99051 33.06230
## 2.425201 2.506713 2.539552 2.552966 2.558475

blocks_arima <- arima(grouped_blocks_playoffs$avg_blocks, 
                        order=c(1,0,1)) # c(AR, I, MA) --> I = 0 for stationary and I = 1 for semi-stationary
predict(blocks_arima, n.ahead =5)
## 3.705942 3.854169 3.921305 3.951712 3.965485
## 0.4254851 0.4689450 0.4773715 0.4790819 0.4794320

FG_arima <- arima(grouped_FG_playoffs$avg_FG, 
                      order=c(1,0,2)) # c(AR, I, MA) --> I = 0 for stationary and I = 1 for semi-stationary
predict(FG_arima, n.ahead =5)
## 0.4186881 0.4112593 0.4183503 0.4196655 0.4199094
## 0.01777531 0.01945535 0.02087099 0.02091798 0.02091959

AST.TOV_arima <- arima(grouped_AST.TOV_playoffs$avg_AST.TOV, 
                  order=c(1,0,5)) # c(AR, I, MA) --> I = 0 for stationary and I = 1 for semi-stationary
predict(AST.TOV_arima, n.ahead =5)
## 1.521605 1.418808 1.473267 1.439856 1.424691
## 0.1022779 0.1137506 0.1222286 0.1258679 0.1286016

FT_arima <- arima(grouped_FT_playoffs$avg_FT, 
                       order=c(1,0,1)) # c(AR, I, MA) --> I = 0 for stationary and I = 1 for semi-stationary
predict(FT_arima, n.ahead =5)
## 0.6070925 0.6082875 0.6093960 0.6104242 0.6113779
## 0.02556088 0.02602838 0.02642397 0.02675964 0.02704509

## GARCH
model_param <- ugarchspec(mean.model=list(armaOrder=c(0,0)), 
                          variance.model= list(model="sGARCH", garchOrder=c(1,1)),
                          distribution.model="norm") ## you can change the distribution model to t-student 

#AST.TOV
garch_model <- ugarchfit(data=grouped_AST.TOV_playoffs$avg_AST.TOV,
                         spec=model_param, out.sample = 20)
print(garch_model)
bootstrap <- ugarchboot(garch_model, method = c("Partial", "Full")[1],
                        n.ahead = 500, n.bootpred = 500)
print(bootstrap)
# t+1           0.13605
# t+2           0.13667
# t+3           0.13729
# t+4           0.13791
# t+5           0.13852
# t+6           0.13913
# t+7           0.13974
# t+8           0.14034
# t+9           0.14094
# t+10          0.14154

#FT
garch_model1 <- ugarchfit(data=grouped_FT_playoffs$avg_FT,
                         spec=model_param, out.sample = 20)
print(garch_model1)
bootstrap1 <- ugarchboot(garch_model1, method = c("Partial", "Full")[1],
                        n.ahead = 500, n.bootpred = 500)
print(bootstrap1)
# t+1           0.029705
# t+2           0.029790
# t+3           0.029874
# t+4           0.029958
# t+5           0.030041
# t+6           0.030124
# t+7           0.030207
# t+8           0.030290
# t+9           0.030372
# t+10          0.030454



## REGULAR SEASON
# relevant variables: Turnovers(24), FT(11), AST.TOV(8.49), Rebounds(7.1), FG(6.6)
#group by years
grouped_turnovers_reg <- my_df_regular %>% group_by(Season.Start.Year) %>% 
  summarize(avg_turnovers = mean(Turnovers,na.rm = TRUE))

grouped_AST.TOV_reg <- my_df_regular %>% group_by(Season.Start.Year) %>%
  summarize(avg_AST.TOV = mean(AST.TOV, na.rm = TRUE))

grouped_FT_reg <- my_df_regular %>% group_by(Season.Start.Year) %>%
  summarize(avg_FT = mean(FT.., na.rm = TRUE))

grouped_rebounds_reg <- my_df_regular %>% group_by(Season.Start.Year) %>%
  summarize(avg_rebounds = mean(Rebounds, na.rm = TRUE))

# ADF test
adf.test(grouped_turnovers_reg$avg_turnovers) # non-stationary
adf.test(grouped_AST.TOV_reg$avg_AST.TOV) # non-stationary
adf.test(grouped_FT_reg$avg_FT) # non-stationary
adf.test(grouped_rebounds_reg$avg_rebounds) # non-stationary

# DECOMPOSE
turnovers_reg_ts <- ts(grouped_turnovers_reg[,c("Season.Start.Year", "avg_turnovers")], 
                          frequency = 2, start=c(1979,1))

dec <- decompose(turnovers_reg_ts) 
plot(dec)

### ACF - for MA() components
acf(grouped_turnovers_reg$avg_turnovers) # MA(8)
acf(grouped_AST.TOV_reg$avg_AST.TOV) # MA(4)
acf(grouped_FT_reg$avg_FT) # MA(0)
acf(grouped_rebounds_reg$avg_rebounds) # MA(5)

### PACF - for AR() components
pacf(grouped_turnovers_reg$avg_turnovers) # AR(1)
pacf(grouped_AST.TOV_reg$avg_AST.TOV) # AR(2)
pacf(grouped_FT_reg$avg_FT) # AR(0)
pacf(grouped_rebounds_reg$avg_rebounds) # AR(1)

# ARIMA
turnovers_reg_arima <- arima(grouped_turnovers_playoffs$avg_turnovers, 
                         order=c(1,1,4)) # c(AR, I, MA) --> I = 0 for stationary and I = 1 for semi-stationary
predict(turnovers_reg_arima, n.ahead =5)
## 9.303478 8.854350 9.942554 8.646990 9.783324
## 0.9775543 1.1243262 1.1574235 1.1759157 1.2858121

AST.TOV_reg_arima <- arima(grouped_AST.TOV_reg$avg_AST.TOV, 
                             order=c(2,1,4)) # c(AR, I, MA) --> I = 0 for stationary and I = 1 for semi-stationary
predict(AST.TOV_reg_arima, n.ahead =5)
## 1.771760 1.802414 1.809293 1.798717 1.801221
## 0.06343904 0.07422265 0.10089184 0.11409199 0.13264982

FT_reg_arima <- arima(grouped_FT_reg$avg_FT, 
                           order=c(1,0,1)) # c(AR, I, MA) --> I = 0 for stationary and I = 1 for semi-stationary
predict(FT_reg_arima, n.ahead =5)
## 0.7061791 0.7061637 0.7061507 0.7061396 0.7061301
## 0.01325285 0.01351349 0.01369887 0.01383137 0.01392640

rebounds_reg_arima <- arima(grouped_rebounds_reg$avg_rebounds, 
                      order=c(1,0,5)) # c(AR, I, MA) --> I = 0 for stationary and I = 1 for semi-stationary
predict(rebounds_reg_arima, n.ahead =5)

# GARCH
model_param <- ugarchspec(mean.model=list(armaOrder=c(0,0)), 
                          variance.model= list(model="sGARCH", garchOrder=c(1,1)),
                          distribution.model="norm") ## you can change the distribution model to t-student 

# Turnovers
garch_model_reg <- ugarchfit(data=grouped_turnovers_reg$avg_turnovers,
                         spec=model_param, out.sample = 20)
print(garch_model_reg)
bootstrap_reg <- ugarchboot(garch_model_reg, method = c("Partial", "Full")[1],
                        n.ahead = 500, n.bootpred = 500)
print(bootstrap_reg)
## huge volatility


# AST.TOV
garch_model_reg1 <- ugarchfit(data=grouped_AST.TOV_reg$avg_AST.TOV,
                             spec=model_param, out.sample = 20)
print(garch_model_reg1)
bootstrap_reg1 <- ugarchboot(garch_model_reg1, method = c("Partial", "Full")[1],
                            n.ahead = 500, n.bootpred = 500)
print(bootstrap_reg1)
# t+1           0.071513
# t+2           0.071891
# t+3           0.072267
# t+4           0.072640
# t+5           0.073011
# t+6           0.073379
# t+7           0.073746
# t+8           0.074110
# t+9           0.074472
# t+10          0.074832

# FT %
garch_model_reg2 <- ugarchfit(data=grouped_FT_reg$avg_FT,
                              spec=model_param, out.sample = 20)
print(garch_model_reg2)
bootstrap_reg2 <- ugarchboot(garch_model_reg2, method = c("Partial", "Full")[1],
                             n.ahead = 500, n.bootpred = 500)
print(bootstrap_reg2)
# t+1           0.016824
# t+2           0.016883
# t+3           0.016941
# t+4           0.016999
# t+5           0.017057
# t+6           0.017114
# t+7           0.017171
# t+8           0.017228
# t+9           0.017285
# t+10          0.017341

# REBOUNDS
garch_model_reg3 <- ugarchfit(data=grouped_rebounds_reg$avg_rebounds,
                              spec=model_param, out.sample = 20)
print(garch_model_reg3)
bootstrap_reg3 <- ugarchboot(garch_model_reg3, method = c("Partial", "Full")[1],
                             n.ahead = 500, n.bootpred = 500)
print(bootstrap_reg3)
# huge volatility 
