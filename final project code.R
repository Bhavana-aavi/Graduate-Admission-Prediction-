library(dplyr)
library(tidyverse)
library(caret)
library(ModelMetrics)
library(ggplot2)
theme_set(theme_bw())
library(corrplot)
#Import the data
df= read.csv("C:/JWU/Introduction to Predictive modeling/week 9/Admission_Predict.csv")
print(df)
is.na(df)
sum(is.na(df))
df <- df[ , -c(1)]
colnames(df)
summary(df)
df$Research<- as.factor(df$Research)
df$University.Rating<- as.factor(df$University.Rating)

#Correlation of numeric variables
corr<- df%>% select('GRE.Score', 'TOEFL.Score', 'SOP','LOR','CGPA','Chance.of.Admit')

cor(corr)
corrplot(cor(corr))
#outliers
ggplot(df, aes(x='',y=GRE.Score))+geom_boxplot(fill= 'steelblue')+ggtitle('Boxplot of GRE Score')
ggplot(df, aes(x='',y=TOEFL.Score))+geom_boxplot(fill= 'steelblue')+ggtitle('Boxplot of TOEFL Score')
ggplot(df, aes(x='',y=SOP))+geom_boxplot(fill= 'steelblue')+ggtitle('Boxplot of SOP')
ggplot(df, aes(x='',y=LOR))+geom_boxplot(fill= 'steelblue')+ggtitle('Boxplot of LOR')
ggplot(df, aes(x='',y=CGPA))+geom_boxplot(fill= 'steelblue')+ggtitle('Boxplot of CGPA')
ggplot(df, aes(x='',y=Chance.of.Admit)) +geom_boxplot(fill = 'steelblue')+ggtitle('Boxplot of Chance of Admit')
ggplot(df, aes(x=Chance.of.Admit,y=Research))+geom_boxplot(fill= 'steelblue')+ggtitle('Boxplot of Research')
ggplot(df, aes(x=Chance.of.Admit,y=University.Rating))+geom_boxplot(fill='steelblue')+ggtitle('Boxplot of University Rating')

hist(df$GRE.Score)
hist(df$TOEFL.Score)
hist(log(df$GRE.Score))
hist(log(df$TOEFL.Score))

ggplot(df, aes(GRE.Score, Chance.of.Admit))+geom_point()+geom_smooth(method= 'lm')+ggtitle('GRE Score vs Chance of Admission')
ggplot(df, aes(CGPA, Chance.of.Admit))+geom_point()+geom_smooth(method= 'lm')+ggtitle('CGPA vs Chance of Admission')
ggplot(df, aes(TOEFL.Score, Chance.of.Admit))+geom_point()+geom_smooth(method= 'lm')+ggtitle('TOEFL Score vs Chance of Admission')
ggplot(df, aes(LOR, Chance.of.Admit))+geom_point()+geom_smooth(method= 'lm')+ggtitle('LOR vs Chance of Admission')
ggplot(df, aes(SOP, Chance.of.Admit))+geom_point()+geom_smooth(method= 'lm')+ggtitle('SOP vs Chance of Admission')


#removing outliers
out<- boxplot.stats(df$LOR)$out
out_ind <- which(df$LOR %in% c(out))
out_ind
df[out_ind, ]
boxplot(df$CGPA, ylab= "CGPA", main = 'boxplot of CGPA')
mtext(paste("Outliers: ", paste(out, collapse= ",")))

out<- boxplot.stats(df$CGPA)$out
out_ind <- which(df$CGPA %in% c(out))
out_ind
df[out_ind, ]
out<- boxplot.stats(df$CGPA)$out
out_ind <- which(df$CGPA %in% c(out))
out_ind
df[out_ind, ]

df <- df[-c(59),]
df<- df[-c(348), ]


df$GRE.Score <- log(df$GRE.Score)
df$TOEFL.Score <- log(df$TOEFL.Score)

head(df)
set.seed(123)
training.samples<- df$Chance.of.Admit %>% 
  createDataPartition(p = 0.8, list = FALSE)
train.data <- df[training.samples, ]
test.data <- df[-training.samples, ]
dim(train.data)
dim(test.data)

model <- lm(Chance.of.Admit ~ . , data= train.data)
summary(model)

train.data<- train.data[ ,-c(3,4)]
test.data<- test.data[ ,-c(3,4)]
dim(train.data)
dim(test.data)
colnames(train.data)


model1 <- lm(Chance.of.Admit ~ ., data= train.data)
summary(model1)

plot(model1)
#make predictions
predictions<- model1%>% predict(test.data)
#Model performance
#Prediction error, RMSE
RMSE(predictions, test.data$Chance.of.Admit)
#MAE
MAE(predictions, test.data$Chance.of.Admit)
#MSE
mse(predictions, test.data$Chance.of.Admit)
#New admission data
newdata <- data.frame(GRE.Score = 5.749393, TOEFL.Score = 4.634729, LOR = 3, CGPA = 8.21, Research= factor(0))
#Predict Chance of admit (actual Chance of admit = 0.65)
model1 %>% predict(newdata)
install.packages('GGally')
library(GGally)
df %>% 
  select(GRE.Score, TOEFL.Score, SOP, LOR, CGPA, Chance.of.Admit) %>% 
  ggpairs(columnLabels = c("GRE.Score", "TOEFL.Score", "SOP", "LOR", "CGPA", "Chance.of.Admit"))


