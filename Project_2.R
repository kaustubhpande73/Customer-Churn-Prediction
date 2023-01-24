install.packages("dplyr")
library(dplyr)
install.packages("installr")
library(installr)
install.packages("tidyverse")
library("tidyverse")
install.packages('plotrix')
library('plotrix')
install.packages("hrbrthemes") # installing packges hrbrthemes
library(hrbrthemes)
install.packages("GGally") # installing packages GGally 
library(GGally)      
install.packages("viridisLite")
library(viridis)
install.packages('ggpubr')
library('ggpubr')
library(ggridges)
library(viridis)
#remove.packages("tidyverse")
#remove.packages("rlang")
install.packages("rlang")
library(rlang)
install.packages("ggplot2")
library(ggplot2)
install.packages("vctrs")
library(vctrs)
update.pack
unloadNamespace("vctrs")
install.packages("pillar")



df<-read.csv("E:\\DSA\\IDA_Project\\Data\\Telecom_customer churn.csv")
(sum(is.na(df))/prod(dim(df)))*100
#View(df) 
#colnames(df)
df <-df[,c(100,1:99)]
#View(df) 
#ncol(df)
#nrow(df)
#str(df)
sum(duplicated(df$Customer_ID))
df[df == ""] <- NA

df_final <- select(df,-c(numbcars,lor,adults,ownrent,HHstatin,dwlltype,dwllsize,infobase)) 
# dropping these eight columns as they have more than 30% of missing values 
df_final <-select(df_final,-c(kid0_2,kid3_5,kid6_10,kid11_15,kid16_17,truck,rv,crclscod,asl_flag,drop_blk_Mean,eqpdays))
# dropping these columns as these will be of no use for churn predictions 

ncol(df_final)
#View(df_final)


colnames(df_final)

 ## missing value imputation
# imputing missing values with the median of the variables to handle against outliers

df_final$rev_Mean[is.na(df_final$rev_Mean)] <- median(df_final$rev_Mean, na.rm = TRUE)
df_final$mou_Mean[is.na(df_final$mou_Mean)] <- median(df_final$mou_Mean, na.rm = TRUE)
df_final$totmrc_Mean[is.na(df_final$totmrc_Mean)] <- median(df_final$totmrc_Mean, na.rm = TRUE)
df_final$da_Mean[is.na(df_final$da_Mean)] <- median(df_final$da_Mean, na.rm = TRUE)
df_final$ovrmou_Mean[is.na(df_final$ovrmou_Mean)] <- median(df_final$ovrmou_Mean, na.rm = TRUE)
df_final$ovrrev_Mean[is.na(df_final$ovrrev_Mean)] <- median(df_final$ovrrev_Mean, na.rm = TRUE)
df_final$vceovr_Mean[is.na(df_final$vceovr_Mean)] <- median(df_final$vceovr_Mean, na.rm = TRUE)
df_final$datovr_Mean[is.na(df_final$datovr_Mean)] <- median(df_final$datovr_Mean, na.rm = TRUE)
df_final$roam_Mean[is.na(df_final$roam_Mean)] <- median(df_final$roam_Mean, na.rm = TRUE)
df_final$change_mou[is.na(df_final$change_mou)] <- median(df_final$change_mou, na.rm = TRUE)
df_final$change_rev[is.na(df_final$change_rev)] <- median(df_final$change_rev, na.rm = TRUE)
df_final$avg6mou[is.na(df_final$avg6mou)] <- median(df_final$avg6mou, na.rm = TRUE)
df_final$avg6qty[is.na(df_final$avg6qty)] <- median(df_final$avg6qty, na.rm = TRUE)
df_final$avg6rev[is.na(df_final$avg6rev)] <- median(df_final$avg6rev, na.rm = TRUE)


# just a one missing value for this column so deleting that particular row
#df_final[is.na(df_final$phones)==TRUE,] # getting the row number which contains missing value

df_final <- df_final[-c(77200), ]

#creating a separate label for webcrap variable 

df_final <- df_final %>% mutate(hnd_webcap=ifelse(is.na(hnd_webcap)==TRUE,'unknown',hnd_webcap))
df_final <- df_final %>% mutate(income=ifelse(is.na(income)==TRUE,'unknown',income))
df_final <- df_final %>% mutate(prizm_social_one=ifelse(is.na(prizm_social_one)==TRUE,'unknown',prizm_social_one))
df_final <- df_final %>% mutate(creditcd=ifelse(is.na(creditcd)==TRUE,'unknown',creditcd))
df_final <- df_final %>% mutate(ethnic=ifelse(is.na(ethnic)==TRUE,'unknown',ethnic))


## imputing missing values with modes for certain following columns

mf_1<-names(which.max(table(df_final$hnd_price)))
df_final <- df_final %>% mutate(hnd_price=ifelse(is.na(hnd_price)==TRUE,mf_1,hnd_price))

mf_2<-names(which.max(table(df_final$forgntvl)))
df_final <- df_final %>% mutate(forgntvl=ifelse(is.na(forgntvl)==TRUE,mf_2,forgntvl))

mf_3<-names(which.max(table(df_final$area)))
df_final <- df_final %>% mutate(area=ifelse(is.na(area)==TRUE,mf_3,area))

mf_4<-names(which.max(table(df_final$marital)))
df_final <- df_final %>% mutate(marital=ifelse(is.na(marital)==TRUE,mf_4,marital))




names(which(colSums(is.na(df_final)) > 0))


## Looking at correlations of the data 

df_numeric$avg6qty


df_corr <- select(df_final,is.numeric)

library(corrplot) 

correlation <- round(cor(df_corr),2)  
library(reshape2)

correlation

melted_cormat <- melt(correlation)

ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                                   midpoint = 0, limit = c(-1,1), space = "Lab", 
                                   name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()


summary(df_corr)




## Visualizations
df_final$churn <- as.factor(df_final$churn)

View(df_final)

colnames(df_final)

df_churn <- df_final[df_final$churn==1,] 
#breaking dataframes for customers who may churn
df_not_churn <- df_final[df_final$churn==0,] 
# breaking dataframes for customers who won't churn


## Donut_Chart

colnames(df_churn)
view(df_churn)
table(df_final$churn)


df <- data.frame(
  group = c("Churned_Customers", "Not_Churned_Customers"),
  value = c(50437,49562))

ggdonutchart(df, "value", label = "group",
             fill = "group", color = "white",
             palette = c("#00AFBB", "#FC4E07") )

### PIE CHART


table(df_final$creditcd)

y<-c(67234,31033,1732)
labels <- c("Credit_Status_Shown","Credit_Status_Not_Shown","Unknown")
pct <-round((y/sum(y)*100),2)
labels<- paste(labels,pct) # add percents to labels
labels <- paste(labels,"%",sep="") # ad % to labels
pie3D(y,main="Credit Status of Customers",explode = 0.35,labels=labels,radius = 0.75,height = 0.17,theta =0.75) 

table(df_final$months)

x<-c(5140,7116,31052,17627,39064)
labels <-c('A','B','M','S','U')
pct <-round((x/sum(x)*100),2)
labels<- paste(labels,pct) # add percents to labels
labels <- paste(labels,"%",sep="") # ad % to labels
pie3D(x,main="Marital Satatus of Customers",labels=labels,radius = 0.90,height = 0.17,theta =0.75)

table(df_final$ethnic)
table(df_final$creditcd)
table(df_final$marital)
table(df_final$churn)


### VISUALIZATION OF CALL PATTERN 



df_final <- df_final %>% mutate(months=ifelse(months> 30,'Old Customer','New Customer'),months)

table(df_final$months)


ggplot(df_final,aes(x=months, y=mou_Mean,fill=churn))+
  geom_bar(position='stack', stat='identity')+
  labs(x=' Category of Customers', y='Minutes of Use', title='Churning Between Old and New Customers')
 


# Usage of monthly minutes

ggplot(data = df_churn, aes(x=mou_Mean))+
  geom_histogram(fill="red")+geom_histogram(data = df_not_churn, aes(x=mou_Mean),fill="orange")+
  labs(title ="Change of Distribution for monthly use of minutes")+
  labs(y="Density")+
  labs(x="Monthly use of minutes")+
  annotate("text", x=2560, y=17410, label= "Churned_Customers",
           col="red", size=5)+
  annotate("text", x=2960, y=19510, label= "Not_churned_customers",
           col="orange", size=5)

# usage of overage minutes


ggplot(data = df_churn, aes(x=ovrmou_Mean))+
  geom_histogram(fill="green")+geom_histogram(data = df_not_churn, aes(x=mou_Mean),fill="blue")+
  labs(title ="Change of Distribution for overage use of minutes")+
  labs(y="Density")+
  labs(x="Usage of overage minutes")+
  annotate("text", x=960, y=43910, label= "Churned_Customers",
           col="green", size=5)+
  annotate("text", x=1060, y=15510, label= "Not_churned_customers",
           col="blue", size=5)

# percentage change in monthly minutes use vs previous three months average

ggplot(data = df_churn, aes(x=roam_Mean))+
  geom_histogram(fill="brown")+geom_histogram(data = df_not_churn, aes(x=mou_Mean),fill="red")+
  labs(title ="Percentage Change in monthly minutes Vs previous six months average")+
  labs(y="Density")+
  labs(x="Percentage change in minutes")+
  annotate("text", x=930, y=49510, label= "Churned_Customers",
           col="brown", size=5)+
  annotate("text", x=1360, y=15510, label= "Not_churned_customers",
           col="red", size=5)


# percentage change in monthly minutes use vs previous three months average


ggplot(data = df_churn, aes(x=change_mou))+
  geom_histogram(fill="pink")+geom_histogram(data = df_not_churn, aes(x=mou_Mean),fill="violet")+
  labs(title ="Percentage Change in monthly minutes Vs previous three months average")+
  labs(y="Density")+
  labs(x="Percentage change in minutes")+
  annotate("text", x=5060, y=47510, label= "Churned_Customers",
           col="pink", size=5)+
  annotate("text", x=6060, y=33510, label= "Not_Churned_customers",
           col="violet", size=5)

# change of dropped calls between two categories

ggplot(data = df_churn, aes(x=drop_vce_Mean))+
  geom_histogram(fill="cyan")+geom_histogram(data = df_not_churn, aes(x=mou_Mean),fill="coral")+
  labs(title ="Change of mean number of dropped calls between two category customers")+
  labs(y="Density")+
  labs(x="Mean number of Dropped calls")+
  annotate("text", x=1060, y=48510, label= "Churned_Customers",
           col="cyan", size=5)+
  annotate("text", x=1460, y=15510, label= "Not_Churned_customers",
           col="coral", size=5)
 
# change of customer care call minutes between two categories

ggplot(data = df_churn, aes(x=cc_mou_Mean))+
  geom_histogram(fill="bisque3")+geom_histogram(data = df_not_churn, aes(x=mou_Mean),fill="brown2")+
  labs(title ="Change of customer care call minutes between two category of customers")+
  labs(y="Density")+
  labs(x="customer care call minutes")+
  annotate("text", x=1060, y=49010, label= "Churned_Customers",
           col="bisque3", size=5)+
  annotate("text", x=1460, y=15510, label= "Not_Churned_customers",
           col="brown2", size=5)



### Scatter plots of different calls 

par(mfrow=c(2,2))
require(gridExtra)
x <-ggplot(data = df_final,na.rm=F) + geom_point(mapping = aes(x = avgrev, y =totrev,color=income))+
  facet_grid(~churn)
y<-ggplot(data = df_final,na.rm=F) + geom_point(mapping = aes(x = avgrev, y =totrev,color=ethnic))+
  facet_grid(~churn)
z<-ggplot(data = df_final,na.rm=F) + geom_point(mapping = aes(x = avgrev, y =totrev,color=marital))+
  facet_grid(~churn)
w<-ggplot(data = df_final,na.rm=F) + geom_point(mapping = aes(x = avgrev, y =totrev,color=models))+
  facet_grid(~churn)
grid.arrange(x,y,z,w, ncol=2,nrow=2)


table(df_final$months)

## Barchart for customers of different araeas

ggplot(df_final,aes(x=adjrev, y=area,fill=churn)) + 
geom_bar(position='stack', stat='identity')+
 labs(x=' Billing Adjusted Revenue', y='Different areas', title='customer from different areas')+
  geom_text(stat='count', vjust=-1)


## RidgeLine Plot for different Area customers

require(gridExtra)
x<-ggplot(df_churn, aes(x =totrev, y =area, fill = factor(..quantile..))) + 
  stat_density_ridges(quantiles = c(0.25,0.5,0.75)
                      , quantile_lines =TRUE
                      , geom="density_ridges_gradient") + 
  scale_fill_viridis(discrete = TRUE
                     , name = "Quantile"
                     , option = "plasma")+labs(title="Revenue for churned Customers")
y<-ggplot(df_not_churn, aes(x =totrev, y =area, fill = factor(..quantile..))) + 
  stat_density_ridges(quantiles = c(0.25,0.5,0.75)
                      , quantile_lines =TRUE
                      , geom="density_ridges_gradient") + 
  scale_fill_viridis(discrete = TRUE
                     , name = "Quantile"
                     , option = "plasma")+labs(title="Revenue for Not Churned Customers")

grid.arrange(x,y,ncol=2)


#Average monthly minutes of use over the previous three months vs total calls

par(mfrow=c(2,2))
require(gridExtra)
s1<-ggplot(data = df_final,na.rm=F) + geom_point(mapping = aes(x = totcalls, y =avg6mou,color=dualband))+
  labs(title = "Average monthly minutes of use over the previous months vs total calls", x=' Total number of calls over the life of the customer', y='Avg minutes of use in previous 6 months') +
  facet_grid(~churn)
d1 <- ggplot(data = df_final,na.rm=F) + geom_point(mapping = aes(x = totcalls, y =avg3mou,color=creditcd))+
  labs(x = "Total number of calls over the life of the customer", y = "Avg minutes of use in previous 3 months")+
  facet_grid(~churn)
grid.arrange(s1,d1, ncol=1,nrow=2)






### DATA PREPARATION FOR MODELLING

#variables with extreme outliers

par(mfrow=c(2,5))
boxplot(df_final$totmrc_Mean,xlab="Mean total monthly recurring charge")
boxplot(df_final$ovrrev_Mean,xlab="Mean overage revenue")
boxplot(df_final$vceovr_Mean,xlab="Mean revenue of voice overage")
boxplot(df_final$change_mou,xlab="Percentage change in monthly minutes of use vs previous three month average")
boxplot(df_final$drop_vce_Mean,xlab="Mean number of dropped voice calls")
boxplot(df_final$months,xlab="Total Months in service")
boxplot(df_final$avg3rev,xlab="Average monthly revenue over the previous three months")
boxplot(df_final$plcd_dat_Mean,xlab="Mean number of attempted data calls placed")
boxplot(df_final$blck_dat_Mean,xlab="Mean number of blocked data calls")
boxplot(df_final$mou_rvce_Mean,xlab="Mean unrounded minutes of use of received voice calls")




ggplot(df_final,aes(x=churn, y=totcalls,fill=creditcd)) + 
  geom_bar(position='stack', stat='identity')+
  labs(x=' Credit Status', y='Total Revenue', title='Credit Status For Customers')
  
table(df_final$hnd_price)

par(mfrow=c(2,3))
hist(df_final$totmrc_Mean,xlab="Mean total monthly recurring charge")
hist(df_final$ovrrev_Mean,xlab="Mean overage revenue")
hist(df_final$avg3rev,xlab="Average monthly revenue over the previous three months")
hist(df_final$totmou,xlab="Total minutes of use over the life of the customer")
hist(df_final$totcalls,xlab="Total number of calls over the life of the customer")
hist(df_final$mou_rvce_Mean,xlab="Mean unrounded minutes of use of received voice calls")


## Feature Engineering

#df_final_Pre$percent_completed_voice_calls[is.na(df_final_Pre$percent_completed_voice_calls)] <- median(df_final_Pre$percent_completed_voice_calls, na.rm = TRUE)
df_final_Pre$percent_completed_data_calls[is.na(df_final_Pre$percent_completed_data_calls)] <- median(df_final_Pre$percent_completed_data_calls, na.rm = TRUE)
#df_final_Pre$percent_completed_calls[is.na(df_final_Pre$percent_completed_calls)] <- median(df_final_Pre$percent_completed_calls, na.rm = TRUE)
df_final_Pre$percent_dropped_calls[is.na(df_final_Pre$percent_dropped_calls)] <- median(df_final_Pre$percent_dropped_calls, na.rm = TRUE)
df_final_Pre$percent_incomplete_calls[is.na(df_final_Pre$percent_incomplete_calls)] <- median(df_final_Pre$percent_incomplete_calls, na.rm = TRUE)


## Predictive Modelling 

df_final_Pre$hnd_webcap 
hnd_webcap
typeof(df_final$ethnic)
colnames(df_final_Pre)
str(df_final_Pre$hnd_webcap)

# Convert all character columns to factor columns

df_final_Pre[sapply(df_final_Pre, is.character)] <- lapply(df_final_Pre[sapply(df_final_Pre, is.character)], 
                                                           as.factor)
# Scaling only the numeric columns 

#df_final_Pre <- df_final_Pre %>% mutate(across(where(is.numeric), scale))

library(h2o)

df_final_Pre
#initializing the h20 package
h2o.init()

#breaking the whole samples into x and y which is the target variable
y <- "churn"
x <- setdiff(names(df_final_Pre), y)

set.seed(2)

sample <- sample(c(TRUE, FALSE), nrow(df_final_Pre), replace=TRUE, prob=c(0.8,0.2))
train  <- df_final_Pre[sample, ]
validation  <- df_final_Pre[!sample, ]



train_h2o <- as.h2o(train)
validation_h2o <- as.h2o(validation)

### Training these below models without outliers

#Logistic Regression
model_lr <- h2o.glm(x = x,
                    y = y,
                    training_frame = df_h2o,
                    nfolds = 5,   
                    family = 'binomial',
                    lambda = 0,
                    alpha = 0.1,
                    keep_cross_validation_predictions = TRUE,
                    standardize = TRUE,
                    seed = 2,   
                    max_iterations_dispersion = 1000)

# Extract Feature importances for Logistic Regression
varimp_lr <- h2o.varimp(model_lr)
varimp_lr



#Decision Tree
model_dt <- h2o.gbm(x = x,
                    y = y,
                    training_frame = df_h2o,
                    nfolds = 5,
                    ntrees = 1,
                    min_rows = 1,
                    sample_rate = 1,
                    col_sample_rate = 1,
                    keep_cross_validation_predictions = TRUE,
                    max_depth = 3,
                    seed = 2)
model_dt

# Extract Feature importances for Decision Tree
varimp_dt <- h2o.varimp(model_dt)
varimp_dt



#Random Forest
model_rf <- h2o.randomForest(x = x, y = y,
                             training_frame = df_h2o,
                             nfolds = 5,
                             ntrees = 300,
                             #col_sample_rate_change_per_level = 0.8,
                             keep_cross_validation_predictions = TRUE,
                             sample_rate = 0.8,
                             col_sample_rate_per_tree = 0.8,
                             max_depth = 2,nbins_top_level= 512,
                             seed = 2)
model_rf

# Extract Feature importances for Random Forest
varimp_rf <- h2o.varimp(model_rf)
varimp_rf



## GBM model
model_gbm <- h2o.gbm(x = x,
                     y = y,
                     training_frame = df_h2o,
                     ntrees = 250,
                     col_sample_rate = 0.8,
                     sample_rate = 0.8,
                     keep_cross_validation_predictions = TRUE,
                     max_depth =3 ,nbins_top_level=256,
                     seed = 2)

model_gbm

# Extract Feature importances for GBM
varimp_gbm <- h2o.varimp(model_gbm)
varimp_gbm


# Neural Network
model_dl <- h2o.deeplearning(x = x,
                             y = y,
                             distribution = "bernoulli",
                             hidden = c(2),
                             epochs = 50,
                             train_samples_per_iteration = -1,
                             reproducible = TRUE,
                             activation = "RectifierwithDropout",
                             single_node_mode = FALSE,
                             balance_classes = TRUE,
                             standardize = TRUE,
                             force_load_balance = FALSE,
                             nfolds = 5,
                             keep_cross_validation_predictions = TRUE,
                             seed = 5,
                             training_frame = train_h2o,
                             validation_frame = validation_h2o,
                             stopping_rounds = 0)
model_dl

# Extract Feature importances for Neural Network
varimp_dl <- h2o.varimp(model_dl)
varimp_dl


### Training these below models with considering outliers

#Logistic Regression
model_lr <- h2o.glm(x = x,
                    y = y,
                    training_frame = df_h2o,
                    nfolds = 5,   
                    family = 'binomial',
                    lambda = 0,
                    alpha = 0.1,
                    keep_cross_validation_predictions = TRUE,
                    standardize = TRUE,
                    seed = 2,   
                    max_iterations_dispersion = 1000)

# Extract Feature importances for Logistic Regression
varimp_lr <- h2o.varimp(model_lr)
varimp_lr



#Decision Tree
model_dt <- h2o.gbm(x = x,
                    y = y,
                    training_frame = df_h2o,
                    nfolds = 5,
                    ntrees = 1,
                    min_rows = 1,
                    sample_rate = 1,
                    col_sample_rate = 1,
                    keep_cross_validation_predictions = TRUE,
                    max_depth = 3,
                    seed = 2)
model_dt

# Extract Feature importances for Decision Tree
varimp_dt <- h2o.varimp(model_dt)
varimp_dt



#Random Forest
model_rf <- h2o.randomForest(x = x, y = y,
                             training_frame = df_h2o,
                             nfolds = 5,
                             ntrees = 300,
                             #col_sample_rate_change_per_level = 0.8,
                             keep_cross_validation_predictions = TRUE,
                             sample_rate = 0.8,
                             col_sample_rate_per_tree = 0.8,
                             max_depth = 2,nbins_top_level= 512,
                             seed = 2)
model_rf

# Extract Feature importances for Random Forest
varimp_rf <- h2o.varimp(model_rf)
varimp_rf



## GBM model
model_gbm <- h2o.gbm(x = x,
                     y = y,
                     training_frame = df_h2o,
                     ntrees = 250,
                     col_sample_rate = 0.8,
                     sample_rate = 0.8,
                     keep_cross_validation_predictions = TRUE,
                     max_depth =3 ,nbins_top_level=256,
                     seed = 2)

model_gbm

# Extract Feature importances for GBM
varimp_gbm <- h2o.varimp(model_gbm)
varimp_gbm


# Neural Network
model_dl <- h2o.deeplearning(x = x,
                             y = y,
                             distribution = "bernoulli",
                             hidden = c(2),
                             epochs = 50,
                             train_samples_per_iteration = -1,
                             reproducible = TRUE,
                             activation = "RectifierwithDropout",
                             single_node_mode = FALSE,
                             balance_classes = TRUE,
                             standardize = TRUE,
                             force_load_balance = FALSE,
                             nfolds = 5,
                             keep_cross_validation_predictions = TRUE,
                             seed = 5,
                             training_frame = train_h2o,
                             validation_frame = validation_h2o,
                             stopping_rounds = 0)
model_dl

# Extract Feature importances for Neural Network
varimp_dl <- h2o.varimp(model_dl)
varimp_dl


























































































