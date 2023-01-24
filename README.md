# Customer-Churn-Prediction

## Concise problem statement
The objective of the problem is to perform churn prediction for the customers of a telecom company in the USA. This problem consists of the postpaid customers who are from 19 different segmented regions of the country and the variables describe numerous telecom sector characteristics such as different consumer call patterns, different revenues of the telecom sector for each consumer and also their information inclusive metadata such as credit status, marital status, income, types of phone model, handset prices etc. The churn is the actual target variable which is in binary form 0 and 1 which means if the customer churns it is shown as 1 and if the customer doesn’t churn it is defined as 0.

## List of Major Concerns /Assumptions
-	From features like average consumer calls or revenue patterns it is assumed that these customers are mainly postpaid although this is not clearly mentioned in the problem statement.
-	For features like credit status and marital status some labels are not clearly mentioned in the problem statement although they can be assumed as specific labels for the necessary of the solution without impacting in the form of noise to the whole data.

## Summary Of Findings
-	Most of the consumer call patterns such as the number of calls and the time of their conversations are positively correlated.
-	Numerous telecom revenues and consumer call patterns are also positively correlated.
-	There is a noticeable difference in behavior between churned and unchurned consumers for particular churn detective telecom features as the number of dropped calls, the number of customer care calls, or the overall conversation duration to customer care.
-	For churn detection, personal information of the consumers such as credit card status or marital status are also deemed as important decisive factors.
-	A number of features relating to customer calls are engineered before predictive modeling.
-	Tree based models result in better outcomes over other linear models as this data is infested with correlations, outliers and skews.
-	The length of time a customer utilizes the service is seen to be the most significant factor in determining the fundamental forecasts regarding churn, according to some main feature interpretation following the predictive modeling.
-	This predominantly depicts that old customers mostly tend to stay with the telecom services, they are not likely to churn but the new customers tend to churn significantly.
