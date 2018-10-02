## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----message=F, warning=F, results='hide'--------------------------------
list.of.packages <- c("tidyverse","MASS","car", "caret","cowplot","caTools","pROC","ggcorrplot", "corrplot", "rpart","rpart.plot","randomForest")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if (length(new.packages)) {install.packages(new.packages)}

t<-sapply(list.of.packages,library, character.only =T)
rm(t)

## ------------------------------------------------------------------------
df<- read.csv("Training Data - Classification of Patients with Abnormal Blood Pressure (N=2000)_27-Jul-2016.csv")
glimpse(df)

## ------------------------------------------------------------------------
df[is.na(df)]<-0  # replace all null with 0

## ------------------------------------------------------------------------
df %>% 
  group_by(Blood_Pressure_Abnormality) %>% 
  summarise(Count = n())%>% 
  mutate(percent = prop.table(Count)*100)%>%
  ggplot(aes(reorder(Blood_Pressure_Abnormality, -percent), percent), fill = Blood_Pressure_Abnormality)+
  geom_col(fill = c("#FC4E07", "#E7B800"))+
  geom_text(aes(label = sprintf("%.2f%%", percent)), hjust = 0.01,vjust = -0.5, size =3)+ 
  theme_bw()+  
  xlab("Blood_Pressure_Abnormality") + 
  ylab("Percent")+
  ggtitle("Blood_Pressure_Abnormality Percent")


## ------------------------------------------------------------------------
corrplot(cor(df[sapply(df[,-c(1:2)], is.numeric),-c(1:2)]))

## ------------------------------------------------------------------------
df_factor_variables<- c("Blood_Pressure_Abnormality","Sex","Pregnancy","Smoking","Level_of_Stress",
                        "Chronic_kidney_disease","Adrenal_and_thyroid_disorders")

df[df_factor_variables]<- as.data.frame(lapply(df[df_factor_variables], as.factor))

## ---- fig.width=10, fig.height=9-----------------------------------------
options(repr.plot.width = 12, repr.plot.height = 10)
plot_grid(ggplot(df, aes(x=Sex,fill=Blood_Pressure_Abnormality))+ geom_bar()+ theme_bw(), 
          ggplot(df, aes(x=Pregnancy,fill=Blood_Pressure_Abnormality))+ geom_bar(position = 'fill')+theme_bw(),
          ggplot(df, aes(x=Smoking,fill=Blood_Pressure_Abnormality))+ geom_bar(position = 'fill')+theme_bw(),
          ggplot(df, aes(x=Level_of_Stress ,fill=Blood_Pressure_Abnormality))+ geom_bar(position = 'fill')+theme_bw(),
          ggplot(df, aes(x=Chronic_kidney_disease,fill=Blood_Pressure_Abnormality))+ geom_bar(position = 'fill')+theme_bw(),
          ggplot(df, aes(x=Adrenal_and_thyroid_disorders ,fill=Blood_Pressure_Abnormality))+ geom_bar(position = 'fill')+theme_bw()+
          scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          align = "h", nrow = 3)

## ---- fig.width=10-------------------------------------------------------
options(repr.plot.width = 12, repr.plot.height = 10)
plot_grid(ggplot(df, aes(Blood_Pressure_Abnormality, Level_of_Hemoglobin,fill=Blood_Pressure_Abnormality))+ geom_boxplot()+ theme_bw(), 
          ggplot(df, aes(Blood_Pressure_Abnormality, Genetic_Pedigree_Coefficient,fill=Blood_Pressure_Abnormality))+ geom_boxplot()+ theme_bw(), 
          ggplot(df, aes(Blood_Pressure_Abnormality, Age,fill=Blood_Pressure_Abnormality))+ geom_boxplot()+ theme_bw(), 
          ggplot(df, aes(Blood_Pressure_Abnormality, BMI,fill=Blood_Pressure_Abnormality))+ geom_boxplot()+ theme_bw()+
           scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          align = "h", ncol = 2)



## ---- fig.width=10-------------------------------------------------------
options(repr.plot.width = 12, repr.plot.height = 10)
plot_grid(ggplot(df, aes(Blood_Pressure_Abnormality, Physical_activity,fill=Blood_Pressure_Abnormality))+ geom_boxplot()+ theme_bw(), 
          ggplot(df, aes(Blood_Pressure_Abnormality, salt_content_in_the_diet,fill=Blood_Pressure_Abnormality))+ geom_boxplot()+ theme_bw(), 
          ggplot(df, aes(Blood_Pressure_Abnormality, alcohol_consumption_per_day,fill=Blood_Pressure_Abnormality))+ geom_boxplot()+ theme_bw()+
           scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          align = "h", ncol = 2)



## ------------------------------------------------------------------------
num_columns<- c("Level_of_Hemoglobin","Age","BMI","Physical_activity","salt_content_in_the_diet","alcohol_consumption_per_day")
df_cor <- round(cor(df[,num_columns]), 1)
# ggcorrplot(df_cor,  title = "Correlation")+theme(plot.title = element_text(hjust = 0.5))


## ------------------------------------------------------------------------

df_final<- df
df_final<- df[,-1]


## ------------------------------------------------------------------------
#Splitting the data
set.seed(123)
indices = sample.split(df_final$Blood_Pressure_Abnormality, SplitRatio = 0.7)
train = df_final[indices,]
validation = df_final[!(indices),]


  

## ------------------------------------------------------------------------
#Build the first model using all variables
model_1 = glm(Blood_Pressure_Abnormality ~ ., data = train, family = "binomial")
summary(model_1)


## ----echo=TRUE, results='hide'-------------------------------------------
model_2<- stepAIC(model_1, direction="both")

## ------------------------------------------------------------------------
summary(model_2)


## ------------------------------------------------------------------------
vif(model_2)


## ------------------------------------------------------------------------
#Removing Physical_activity due to high p-value 
model_3 <-glm(formula = Blood_Pressure_Abnormality ~ Level_of_Hemoglobin + BMI + Sex + 
                 Chronic_kidney_disease + Adrenal_and_thyroid_disorders
              , family = "binomial", data = train)
summary(model_3)
vif(model_3)

## ------------------------------------------------------------------------

final_model <- model_2


## ------------------------------------------------------------------------

pred <- predict(final_model, type = "response", newdata = validation[,-1])
summary(pred)
validation$prob <- pred

# Using probability cutoff of 50%.

pred_ht <- factor(ifelse(pred >= 0.50, "Yes", "No"))
actual_ht <- factor(ifelse(validation$Blood_Pressure_Abnormality==1,"Yes","No"))
table(actual_ht,pred_ht)


## ------------------------------------------------------------------------


cutoff_ht <- factor(ifelse(pred >=0.50, "Yes", "No"))
conf_final <- confusionMatrix(cutoff_ht, actual_ht, positive = "Yes")
accuracy <- conf_final$overall[1]
sensitivity <- conf_final$byClass[1]
specificity <- conf_final$byClass[2]
accuracy
sensitivity
specificity

## ------------------------------------------------------------------------

perform_fn <- function(cutoff) 
{
  predicted_ht <- factor(ifelse(pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_ht, actual_ht, positive = "Yes")
  accuray <- conf$overall[1]
  sensitivity <- conf$byClass[1]
  specificity <- conf$byClass[2]
  out <- t(as.matrix(c(sensitivity, specificity, accuray))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}


## ----echo=TRUE, results='hide'-------------------------------------------
options(repr.plot.width =8, repr.plot.height =6)
summary(pred)
s = seq(0.01,0.80,length=100)
OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

## ------------------------------------------------------------------------
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),
     type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend("bottom",col=c(2,"darkgreen",4,"darkred"),text.font =3,inset = 0.02,
       box.lty=0,cex = 0.8, 
       lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))
abline(v = 0.5, col="red", lwd=1, lty=2)
axis(1, at = seq(0.1, 1, by = 0.1))


## ----echo=TRUE, results='hide'-------------------------------------------
#Training
Dtree = rpart(Blood_Pressure_Abnormality ~., data = train, method = "class")
summary(Dtree)

#Predicting 
DTPred <- predict(Dtree,type = "class", newdata = validation[,-1])


## ------------------------------------------------------------------------

confusionMatrix(validation$Blood_Pressure_Abnormality, DTPred)

## ------------------------------------------------------------------------

model.rf <- randomForest(Blood_Pressure_Abnormality ~ ., data=train, proximity=FALSE,importance = FALSE,
                         ntree=500,mtry=4, do.trace=FALSE)
model.rf


## ------------------------------------------------------------------------

#Predicting on the validation set and checking the Confusion Matrix.
testPred <- predict(model.rf, newdata=validation[,-1])
table(testPred, validation$Blood_Pressure_Abnormality)

confusionMatrix(validation$Blood_Pressure_Abnormality, testPred)



## ------------------------------------------------------------------------
#Checking the variable Importance Plot
varImpPlot(model.rf)

## ------------------------------------------------------------------------

options(repr.plot.width =10, repr.plot.height = 8)

glm.roc <- roc(response = validation$Blood_Pressure_Abnormality, predictor = as.numeric(pred))
DT.roc <- roc(response = validation$Blood_Pressure_Abnormality, predictor = as.numeric(DTPred))
rf.roc <- roc(response = validation$Blood_Pressure_Abnormality, predictor = as.numeric(testPred))

plot(glm.roc,      legacy.axes = TRUE, print.auc.y = 1.0, print.auc = TRUE)
plot(DT.roc, col = "blue", add = TRUE, print.auc.y = 0.65, print.auc = TRUE)
plot(rf.roc, col = "red" , add = TRUE, print.auc.y = 0.85, print.auc = TRUE)
legend("bottom", c("Random Forest", "Decision Tree", "Logistic"),
       lty = c(1,1), lwd = c(2, 2), col = c("red", "blue", "black"), cex = 0.75)






