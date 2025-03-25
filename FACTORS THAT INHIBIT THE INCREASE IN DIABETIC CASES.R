Diabetic<-read.csv(file.choose(),header = TRUE)
head(Diabetic)
tail(Diabetic)

#H0: The chances of being diabetic increases with the increase in age and BMI
#H1:The chances of being diabetic doesn't increase with the increase in age and BMI
cor(Diabetic$age,Diabetic$bmi,method = "spearman")
#Since we can see a positive correlation of 0.3513,then we conclude that an increase in BMI and age of an individual leads to an increase of contracting diabetes
#since the p-value is greater than 0.05, then we conclude that there is a low correlation between the BMI,age and being diabetic
#We fail to reject the null hypothesis hence the increase in BMI and age leads to an increase in the chances of being diabetic

#H0:The increase in age corresponds to a higher BMI index
#H1:The increase in age  doesn't corresponds to a higher BMI index
Diabetic.aov=lm(age~bmi,data=Diabetic)
summary(Diabetic.aov)
#multiple R-squared elaborates that the age variable varies by 11% to the BMI index in this model
#adjusted R-squared shows that the model has an estimated 11% accuracy in terms of prediction
#since the P-value is <0.05,at 95% confidence level then we reject the null hypothesis and conclude that an increase in age doesn't correspond to a higher bmi index

#H0:High blood glucose level increases the chances of contracting diabetes +hypertension
#H1:High blood glucose level doesn't increase the chances of contracting diabetes+ hypertension
lifestyle.lm=lm(blood_glucose_level~diabetes+hypertension,data=Diabetic)
summary(lifestyle.lm)
#since the p-value is<0.05 then blood_glucose_level is statistically significant  to diabetes and hypertension, hence we fail to reject the null hypothesis and conclude that High blood glucose level increases the chances of contracting diabetes +hypertension
#multiple R-squared elaborates that the blood_glucose_level variable  varies by 17.6% to the  risk of contracting diabetes and hypertension in this model
#adjusted R-squared shows that the model has an estimated 17.6% accuracy in terms of prediction

library(ggplot2)
#check structure of data
str(Diabetic)
#convert the year colum to a factor
Diabetic$year=as.factor(Diabetic$year)
# Bar plot for categorical data
ggplot(data=Diabetic,aes(x=smoking_history))+
  geom_bar(col='red',alpha=0.5)+
  labs(x='SMOKING HISTORY',y='count',title = 'smoking history of patients')+
  theme_bw()
#stacked barplot
ggplot(data=Diabetic,aes(x=smoking_history,fill=gender))+
  geom_bar(col='red',position = 'dodge')+
  labs(x='SMOKING HISTORY',y='count',title = 'smoking history of patients')+
  theme_bw()

#barplot is used for both categorical and quantitative variables
# using violin plot to compare between 3 variables which are: Age and BMI which are quantitative variables,gender being a categorical data
library(ggplot2)
ggplot(data=Diabetic,aes(x=bmi,y=age))+
  geom_violin()+
  theme_bw()+
  facet_wrap(~gender)+
  coord_flip()

  #scatterplot to compare between 2 quantitative variables
ggplot(data=Diabetic,aes(x=blood_glucose_level,y=hypertension))+
  geom_point(size=2)+
  labs(x='GLUCOSE LEVEL',y='HYPERTENSION',title='impact of high blood gliucose level to hypertension')+
  theme(plot.title = element_text(hjust = 0.5))


