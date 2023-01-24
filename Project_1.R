## visualization for churned customers

ggplot(data = df_churn,na.rm=F) + geom_point(mapping = aes(x = rev_Mean, y =totmrc_Mean,color=models))+
  geom_smooth(mapping = aes(x = rev_Mean, y =totmrc_Mean,color=models),alpha=I(.35))+ 
  labs(x='rev_Mean', y='totmrc_Mean', title='Scatter Plot between Mean monthly revenue and Mean total monthly recurring charge for churned customers')

## visualization for not churned customers
ggplot(data = df_not_churn ,na.rm=F) + geom_point(mapping = aes(x = rev_Mean, y =totmrc_Mean,color=models))+
  geom_smooth(mapping = aes(x = rev_Mean, y =totmrc_Mean,color=models),alpha=I(.35))+ 
  labs(x='rev_Mean', y='totmrc_Mean', title='Scatter Plot between Mean monthly revenue and Mean total monthly recurring charge for customers who do not churn')

ggplot(df_churn, aes(fill=area, y=peak_vce_Mean, x=models)) + 
  geom_bar(position='stack', stat='identity')+ theme(plot.title = element_text(hjust=0.5, size=20, face='bold'))+
  labs(x='models', y='Mean number of inbound and outbound peak voice calls', title='Type of Phones for different areas and peak number of voice calls for churned customers')

ggplot(df_churn, aes(fill=area, y=peak_vce_Mean, x=models)) + 
  geom_bar(position='stack', stat='identity')+ theme(plot.title = element_text(hjust=0.5, size=20, face='bold'))+
  labs(x='models', y='Mean number of inbound and outbound peak voice calls', title='Stacked Barplot churned customers')

ggplot(df_not_churn, aes(fill=area, y=peak_vce_Mean, x=models)) + 
  geom_bar(position='stack', stat='identity')+ theme(plot.title = element_text(hjust=0.5, size=20, face='bold'))+
  labs(x='models', y='Mean number of inbound and outbound peak voice calls', title='Stacked barplot for customers who didnot churn')


### Data Quality Report & Missing Value Visualizations 

## Data Quality Report
## Numerical Variable

View(df_numeric)
df_numeric$numbcars 
table(df_numeric$uniqsubs)

df_numeric <- select(df_numeric,c(-1,-75,-78,-77,-72,-73,-50,-51,-74,-76,-77,-71,-70,-49))
colnames(df_numeric)

Q1<-function(c,na.rm=TRUE) {  # Computing First Quartile
  quantile(c,na.rm=na.rm)[2]
}

Q3<-function(x,na.rm=TRUE) {  # computing Third Quartile
  quantile(x,na.rm=na.rm)[4]
}

myNumericSummary <- function(x){    
  c(length(x), n_distinct(x), sum(is.na(x)), mean(x, na.rm=TRUE),
    min(x,na.rm=TRUE), Q1(x,na.rm=TRUE), median(x,na.rm=TRUE), Q3(x,na.rm=TRUE),
    max(x,na.rm=TRUE), sd(x,na.rm=TRUE))
}
numericSummary<-df_numeric %>%     
  summarise(across(everything(), myNumericSummary))

numericSummary <-cbind(
  stat=c("n","unique","missing","mean","min","Q1","median","Q3","max","sd"),
  numericSummary)

numericSummaryFinal <- numericSummary %>%
  pivot_longer("rev_Mean":"eqpdays", names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(missing_pct = 100*missing/n,
         unique_pct = 100*unique/n) %>%
  select(variable, n, missing, missing_pct, unique, unique_pct, everything())
library(knitr)
options(digits=3)
options(scipen=99)
numericSummaryFinal %>% kable()

## Data Quality Report
## Categorical Report

df_category <- select(df,is_character)
colnames(df)
colnames(df_category) 
df_1 <- select(df,c(100,91,85,86,87,76,77))
df_2 <- select(df,c("Customer_ID","new_cell","crclscod","asl_flag","prizm_social_one","area","dualband","refurb_new","hnd_webcap","ownrent",
                    "ownrent","dwlltype","marital","infobase","HHstatin","dwllsize","ethnic","kid0_2","kid3_5","kid6_10",
                    "kid11_15","kid16_17","creditcd"))
df_categoric <- merge(df_1,df_2,by='Customer_ID')
df_categoric <- select(df_categoric,c(-1))


getmodes_1 <- function(v,type=1) {
  tbl <- table(v)
  m1<-which.max(tbl)
  if (type==1) {
    return (names(m1)) #1st mode
  }
  else if (type==2) {
    return (names(which.max(tbl[-m1]))) #2nd mode
  }
  else if (type==-1) {
    return (names(which.min(tbl))) #least common mode
  }
  else {
    stop("Invalid type selected")
  }
}
getmodes_2 <- function(v,type=2) {
  tbl <- table(v)
  m1<-which.max(tbl)
  if (type==1) {
    return (names(m1)) #1st mode
  }
  else if (type==2) {
    return (names(which.max(tbl[-m1]))) #2nd mode
  }
  else if (type==-1) {
    return (names(which.min(tbl))) #least common mode
  }
  else {
    stop("Invalid type selected")
  }
}

getmodes_3 <- function(v,type=-1) {
  tbl <- table(v)
  m1<-which.max(tbl)
  if (type==1) {
    return (names(m1)) #1st mode
  }
  else if (type==2) {
    return (names(which.max(tbl[-m1]))) #2nd mode
  }
  else if (type==-1) {
    return (names(which.min(tbl))) #least common mode
  }
  else {
    stop("Invalid type selected")
  }
}

getmodesCnt_1 <- function(v,type=1) {
  tbl <- table(v)
  m1<-which.max(tbl)
  if (type==1) {
    return (max(tbl)) #1st mode freq
  }
  else if (type==2) {
    return (max(tbl[-m1])) #2nd mode freq
  }
  else if (type==-1) {
    return (min(tbl)) #least common freq
  }
  else {
    stop("Invalid type selected")
  }
}

## Function defined for frequency of the second mode

getmodesCnt_2 <- function(v,type=2) {
  tbl <- table(v)
  m1<-which.max(tbl)
  if (type==1) {
    return (max(tbl)) #1st mode freq
  }
  else if (type==2) {
    return (max(tbl[-m1])) #2nd mode freq
  }
  else if (type==-1) {
    return (min(tbl)) #least common freq
  }
  else {
    stop("Invalid type selected")
  }
}

## Function defined for the frequency of the least common mode

getmodesCnt_3 <- function(v,type=-1) {
  tbl <- table(v)
  m1<-which.max(tbl)
  if (type==1) {
    return (max(tbl)) #1st mode freq
  }
  else if (type==2) {
    return (max(tbl[-m1])) #2nd mode freq
  }
  else if (type==-1) {
    return (min(tbl)) #least common freq
  }
  else {
    stop("Invalid type selected")
  }
}

mycategoricSummary <- function(x){
  c(length(x), n_distinct(x), sum(is.na(x)),round((getmodesCnt_1(x)/getmodesCnt_2(x)),2),getmodes_1(x),getmodesCnt_1(x),getmodes_2(x),getmodesCnt_2(x),getmodes_3(x),getmodesCnt_3(x))
}

CategoricalSummary<-df_categoric %>%     
  summarise(across(everything(),mycategoricSummary))
# running a series of statistical functions over all the variables for a categorical data tibble

categoricSummary<-cbind(stat=c("n","unique","missing","freqRatio","1st mode","1st mode freq","2nd mode","2nd mode freq","least common","least common freq"),CategoricalSummary)
# Defining new columns described in categorical report excerpt which evaluates the previously defined statistical functions' values  


## Using pivot_longer and pivot_wider to manipulate the newly created data frame and also adding two columns for knowing percentages of the missing and unique values respectively

colnames(df_categoric)
categoricSummaryFinal <- categoricSummary %>%
  pivot_longer("forgntvl":"creditcd", names_to = "variable", values_to = "value") %>% # using pivot_longer to increase number of rows and decrease columns
  pivot_wider(names_from = stat, values_from = value) %>% # using pivot_wider to increasing columns and decreasing rows
  mutate(missing_pct = 100*as.numeric(missing)/as.numeric(n), # defining two columns for getting percentage value of missing and unique values
         unique_pct = 100*as.numeric(unique)/as.numeric(n)) %>%
  select(variable, n, missing, missing_pct, unique, unique_pct, everything())

library(knitr) # unpacking knitr package for generating report
options(digits=3)  
options(scipen=99) # Artificially rounding computation
categoricSummaryFinal %>% kable() # using kable function to generate final table for the data quality report of the categorical


## Missing Value analysis and Imputation

names(which(colSums(is.na(df_final)) > 0)) # Out of 100 variables we have seen missing values in 43 variables 
View(df)
select(df,is.numeric)  %>%        ## Missing value visualization for numerical variables
  summarise_all(list(~is.na(.)))%>%
  pivot_longer(everything(),
               names_to = "variables", values_to="missing") %>%
  count(variables, missing) %>%
  ggplot(aes(y=variables,x=n,fill=missing))+
  geom_col()




select(df,!is.numeric)  %>%        ## Missing value visualization for categorical variables
  summarise_all(list(~is.na(.)))%>%
  pivot_longer(everything(),
               names_to = "variables", values_to="missing") %>%
  count(variables, missing) %>%
  ggplot(aes(y=variables,x=n,fill=missing))+
  geom_col()
colnames(df)
