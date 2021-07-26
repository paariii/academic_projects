#-------------------importing libraries---------------
library(psych)
library(caTools)
library(ROCR)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ggplot2)
library(caret)
library(e1071)
library(ggpubr)
library(ggthemes)
library(vip)

#-------------------Importing Data------------

h <- read.csv("hotel_bookings.csv" ,stringsAsFactors = T)
summary(h)
View(h)
str(h)

h$is_canceled_f <- as.factor(h$is_canceled)
h$is_repeated_guest <- as.factor(h$is_repeated_guest)
summary(h)

#cancellation in each hotel
ggplot(data = h, aes(x=hotel,fill=is_canceled_f )) + geom_bar() +ggtitle("Cancellation in each Hotel") +
  labs(fill="Cancelled")

#children has 4 NAs and 1 undefined distribution channel.
h <- h[!is.na(h$children), ]
h <- h[(as.character(h$distribution_channel) != "Undefined" ), ]

#removing unused factor "Undefined" in "distribution_channel"
table(h$distribution_channel) #distribution_channel
h$distribution_channel <- factor(h$distribution_channel)
table(h$distribution_channel)
table(h$market_segment) #market_segment
h$market_segment <- factor(h$market_segment)
table(h$market_segment)

#replacing NULL with "others"
levels(h$company) <- c(levels(h$company), "others") #company
h$company[h$company == 'NULL'] <- 'others'
levels(h$agent) <- c(levels(h$agent), "otherAgents") #agent
h$agent[h$agent == 'NULL'] <- 'otherAgents'

#  'Undefined' and 'SC' are the same
h$meal[h$meal=='Undefined'] <- 'SC'
h$meal <- factor(h$meal) #removing unused factor!
table(h$meal)

#detecting an outlier
ggplot(data = h, aes(y=adr  ,x=is_canceled_f )) + geom_boxplot() + xlab("Cancelled") #outlier detected
h <- h[-which.max(h$adr),]
ggplot(data = h, aes(y=adr  ,x=is_canceled_f )) + geom_boxplot() + xlab("Cancelled")

#checking for dataset veracity
h$reservation_status = as.character(h$reservation_status)
h$cancele_check <- ifelse( (((h$reservation_status == "Canceled") || (h$reservation_status == "No-Show")) && (as.character(h$is_canceled_f) == "1")) || ((h$reservation_status == "Check-Out")  && (as.character(h$is_canceled_f) == "0")) ,1,0)
sum(h$cancele_check)
h$reservation_status = as.factor(h$reservation_status)

#introducing new variables
#stays
h$stays = h$stays_in_week_nights + h$stays_in_weekend_nights
ggplot(data = h, aes(x=stays_in_week_nights , y=stays_in_weekend_nights)) + geom_point() #+geom_smooth(y~x , method=lm , se = FALSE)
cor(h$stays_in_week_nights,h$stays_in_weekend_nights) #good. so we use a function of these two as below

#num
h$num = h$babies + h$children + h$adults
table(h$num) # num = 0 is not accepted so we delete them
h <- subset(h,h$num>0)

#country_recode
ggplot(data = h, aes(x=country,fill=is_canceled_f )) + geom_bar()
nrow(subset(h,h$country=="PRT"))/nrow(h)  #approximately 40% customers are from PRT
h$country_recode <- as.factor(ifelse(h$country == "PRT" , 1,0))

#company_recode
h$company_recode <- ifelse(h$company == "others" , 1,0)
h$company_recode <- as.factor(h$company_recode)
ggplot(data = h, aes(x=company_recode  ,fill=is_canceled_f )) + geom_bar() + xlab("New Code") + labs(fill="Cancelled")

#assigned_vs_reserved_room_type
h$assigned_vs_reserved_room_type <- ifelse(as.character (h$assigned_room_type) == as.character (h$reserved_room_type) , "yes","no")
h$assigned_vs_reserved_room_type <- as.factor(h$assigned_vs_reserved_room_type)
ggplot(data = h, aes(x= assigned_vs_reserved_room_type  ,fill=is_canceled_f )) + geom_bar() + xlab("assigned room is the same as reserved room") + labs(fill="Cancelled")

#aggregate three columns to generate date
h$date = as.Date(with(h,paste(arrival_date_year,arrival_date_month,arrival_date_day_of_month,sep="-")),"%Y-%B-%d")
#reorder months
h$arrival_date_month = factor(h$arrival_date_month, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
h$reservation_status_date <- as.Date(h$reservation_status_date)

#-------------------Visualization-----------------------
##histograms and correlations
bookings = aggregate(cbind(lead_time, arrival_date_week_number, previous_cancellations, stays, adr, days_in_waiting_list, num) ~ date, h, mean)
pairs.panels(data.frame(bookings$lead_time, bookings$arrival_date_week_number, 
                        bookings$previous_cancellations, bookings$stays, bookings$adr,
                        bookings$days_in_waiting_list, bookings$num))

##number of guests
num_guests = aggregate(is_canceled~hotel+arrival_date_month+is_canceled_f, data=h, FUN=length)
mean_adr = aggregate(adr~hotel+arrival_date_month, data=h, mean)
for (i in unique(num_guests$arrival_date_month)) {
  num_guests$adr[num_guests$arrival_date_month== i] = mean_adr$adr[mean_adr$arrival_date_month==i] 
}
ratio <- max(num_guests$is_canceled)/max(num_guests$adr)
#Resort Hotel
ggplot(subset(num_guests, hotel=="Resort Hotel"), aes(x=arrival_date_month)) + geom_bar(aes(y=is_canceled, fill=is_canceled_f),position="stack", stat="identity") +
  geom_line(aes(y=adr*ratio, group=1), size=1) + theme_minimal() + scale_fill_discrete(name = "Cancelled") + 
  scale_y_continuous(name = "Total Guests", sec.axis = sec_axis( trans=~./ratio, name="Average Adr")) + xlab("Month") + ggtitle("Resort Hotel")
#City Hotel
ggplot(subset(num_guests, hotel=="City Hotel"), aes(x=arrival_date_month)) + geom_bar(aes(y=is_canceled, fill=is_canceled_f),position="stack", stat="identity") +
  geom_line(aes(y=adr*ratio, group=1), size=1) + theme_minimal() +
  scale_y_continuous(name = "Total Guests", sec.axis = sec_axis( trans=~./ratio, name="Average Adr")) + xlab("Month") + ggtitle("City Hotel") + scale_fill_discrete(name = "Cancelled")

##lead time
lead_res1 = aggregate(is_canceled ~ customer_type+lead_time, data=h, mean)
ggplot (data = lead_res1 , aes(x=lead_time, y=is_canceled, color=is_canceled)) + geom_point() + facet_grid(rows=vars(customer_type)) + 
  geom_hline(yintercept=0.5, linetype="dashed", color = "red", size=1)+ geom_vline(xintercept=250, linetype="dashed", color = "red", size=1) + theme_bw() +
  xlab("Lead Time") + ylab("Cancellation Rate") + theme(legend.position = "none")
#Group: usually no cancellations, Transient: correlated, others: those with lead times less than 250 are likely not to cancel

##customer type
ggplot(h, aes(fill=is_canceled_f, x=customer_type)) + 
  geom_bar() + facet_grid(cols=vars(hotel)) + theme_bw() +
  labs(fill="Cancelled") + xlab("Customer Type") + ylab("Number of Guests")
#most of the customers are Transient and half of them cancel their reservation

##Hypothesis Test
#hypothesis test: previous_bookings_not_canceled vs is_canceled_f
previous_bookings_not_canceled_table <- table(h$previous_bookings_not_canceled , h$is_canceled_f)[1:4,]
previous_bookings_not_canceled_test <- chisq.test(previous_bookings_not_canceled_table)
#hypothesis test: is_repeated_guest vs is_canceled_f
is_repeated_guest_table <- table(h$is_canceled_f , h$is_repeated_guest)
is_repeated_guest_test <- chisq.test(is_repeated_guest_table)

##market segment
ggplot(data = h, aes(x=market_segment ,fill=is_canceled_f )) + geom_bar() +facet_grid(.~hotel)+
  labs(fill="Cancelled") + theme_bw() + xlab("Market Segment")

##deposit type
ggplot(h, aes(fill=is_canceled_f, x=deposit_type)) + 
  geom_bar() + theme_calc() + scale_fill_brewer(palette = "Paired") + facet_grid(cols=vars(hotel)) +
  xlab("Deposit Type") + labs(fill="Cancelled") + theme(legend.position = "right")
#possible relations
tapply(as.numeric(h$is_canceled_f)-1 , h$deposit_type , mean)
tapply(h$lead_time , h$deposit_type , mean)
tapply(as.numeric(h$is_repeated_guest)-1 , h$deposit_type , mean)
tapply(h$previous_cancellations , h$deposit_type , mean)

##deposit type vs repeated guest vs customer type
ggplot(data=h, mapping = aes(x=deposit_type, y=is_repeated_guest))+ geom_jitter(aes(color = is_canceled_f), size = 0.3)+
  ggpubr::color_palette("jco")+ xlab("Deposit Type") + ylab("Repeated Guest") + 
  ggpubr::theme_pubclean() + facet_grid(cols=vars(hotel), rows=vars(customer_type))
#-------------------Data Cleaning------------------
# removing unused columns
drop <- c("is_canceled", "reservation_status","reservation_status_date" ,"cancele_check",
          "date" , "country", "arrival_date_year", "arrival_date_month", "arrival_date_day_of_month",
          "agent" , "company")
h = h[,!(names(h) %in% drop)]

set.seed(1725)
split <- sample.split(h$is_canceled_f, SplitRatio = 0.7)
split[1:20]
h_train <- subset(h, split==TRUE)
h_test <- subset(h, split==FALSE)

h1_train <- subset(h_train,h_train$hotel=="Resort Hotel")
h1_test <- subset(h_test,h_test$hotel=="Resort Hotel")
h2_train <- subset(h_train,h_train$hotel=="City Hotel")
h2_test <- subset(h_test,h_test$hotel=="City Hotel")

drop1 <- c("hotel")
h1_train = h1_train[,!(names(h1_train) %in% drop1)]
h2_train = h2_train[,!(names(h2_train) %in% drop1)]
h1_test = h1_test[,!(names(h1_train) %in% drop1)]
h2_test = h2_test[,!(names(h2_train) %in% drop1)]

#-------------------logistic regression on h data-----------------
#train
logmod <- glm(is_canceled_f ~ . -hotel - adults -children -babies 
              -stays_in_week_nights -stays_in_weekend_nights
              -reserved_room_type -assigned_room_type 
              -required_car_parking_spaces,data= h_train , family = binomial)
summary(logmod)
pred_train_log <- predict(logmod, type="response")
head(pred_train_log)
str(pred_train_log)
pred_ROC_train_log <- prediction(pred_train_log, h_train$is_canceled_f)
perf_ROC_train_log <- performance(pred_ROC_train_log, "tpr", "fpr")
plot(perf_ROC_train_log ,print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.5,0.5) , colorize=TRUE
     , main = "ROC curve for imbalance h_train data")
abline(0,1,lty=2)
table(h_train$is_canceled_f, pred_train_log > 0.4)

as.numeric(performance(pred_ROC_train_log, "auc")@y.values)

#test
pred_test_log <- predict(logmod, type = "response", newdata = h_test)
table(h_test$is_canceled_f, pred_test_log > 0.4)

pred_ROC_log <- prediction(pred_test_log, h_test$is_canceled_f)
perf_ROC_log <- performance(pred_ROC_log, "tpr", "fpr")
plot(perf_ROC_log , main = "test ROC curve for imbalance dataset")
abline(0,1,lty=2)
as.numeric(performance(pred_ROC_log, "auc")@y.values)

#-------------------logistic regression on h1 data-----------------
#make the model
logmod1 <- glm(is_canceled_f ~ .  - adults -children -babies 
              -stays_in_week_nights -stays_in_weekend_nights
              -reserved_room_type -assigned_room_type 
              -required_car_parking_spaces,data= h1_train , family = binomial)
summary(logmod1)

#draw ROC curve and calculate area under the curve-train
pred_train_log <- predict(logmod1, type="response")
pred_ROC_train_log <- prediction(pred_train_log, h1_train$is_canceled_f)
perf_ROC_train_log <- performance(pred_ROC_train_log, "tpr", "fpr")
plot(perf_ROC_train_log ,print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.5,0.5) , colorize=TRUE
     , main = "ROC curve for h1 train data")
abline(0,1,lty=2)
as.numeric(performance(pred_ROC_train_log, "auc")@y.values)
resort_train_matrix_lg <- table(h1_train$is_canceled_f, pred_train_log > 0.35)

#draw ROC curve and calculate area under the curve-test
pred_test_log <- predict(logmod1, type = "response", newdata = h1_test)
pred_ROC_log <- prediction(pred_test_log, h1_test$is_canceled_f)
perf_ROC_log <- performance(pred_ROC_log, "tpr", "fpr")
plot(perf_ROC_log , main = "ROC curve for h1 test data")
abline(0,1,lty=2)
resort_auc_lg <- as.numeric(performance(pred_ROC_log, "auc")@y.values)
resort_Matrix_lg <- table(h1_test$is_canceled_f, pred_test_log > 0.35)

#-------------------logistic regression on h2 data-----------------
#make the model
logmod2 <- glm(is_canceled_f ~ .  - adults -children -babies 
               -stays_in_week_nights -stays_in_weekend_nights
               -reserved_room_type -assigned_room_type 
               -required_car_parking_spaces -arrival_date_week_number,data= h2_train , family = binomial)
summary(logmod2)

#draw ROC curve and calculate area under the curve-train
pred_train_log <- predict(logmod2, type="response")
pred_ROC_train_log <- prediction(pred_train_log, h2_train$is_canceled_f)
perf_ROC_train_log <- performance(pred_ROC_train_log, "tpr", "fpr")
plot(perf_ROC_train_log ,print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.5,0.5) , colorize=TRUE
     , main = "ROC curve for h2 train data")
abline(0,1,lty=2)
as.numeric(performance(pred_ROC_train_log, "auc")@y.values)
city_train_matrix_lg <- table(h2_train$is_canceled_f, pred_train_log > 0.4)

#draw ROC curve and calculate area under the curve-test
pred_test_log <- predict(logmod2, type = "response", newdata = h2_test)
pred_ROC_log <- prediction(pred_test_log, h2_test$is_canceled_f)
perf_ROC_log <- performance(pred_ROC_log, "tpr", "fpr")
plot(perf_ROC_log , main = "ROC curve for h2 test data")
abline(0,1,lty=2)
city_auc_lg <- as.numeric(performance(pred_ROC_log, "auc")@y.values)
city_Matrix_lg <- table(h2_test$is_canceled_f, pred_test_log > 0.4)

#-------------------decision tree on h1 data------------------------
#cross validation for parameter cp
numFolds <- trainControl(method="cv", number=5)
set.seed(1725)
cpGrid <- expand.grid(.cp=seq(0.001, 0.01, 0.001))
resort_tree_cp <- train(is_canceled_f ~ ., data=h1_train, method="rpart", trControl= numFolds, tuneGrid= cpGrid)
#choosing the best cp
ggplot(resort_tree_cp) + geom_text(aes(label=cp))
resort_best_cp <- 0.009

set.seed(1725)
#make tree decision model
resort_tree_model <- rpart(is_canceled_f ~., data=h1_train, method="class", cp=resort_best_cp)
pdf("resort.pdf")
prp(resort_tree_model)
dev.off()
#most important factors
vip(resort_tree_model)

#draw ROC curve and calculate area under the curve-train
resort_pred_train_tr <- predict(resort_tree_model, type="prob")
resort_pred_ROC_train_tr <- prediction(resort_pred_train_tr[, 2], h1_train$is_canceled_f)
resort_perf_ROC_train_tr <- performance(resort_pred_ROC_train_tr, "tpr", "fpr")
plot(resort_perf_ROC_train_tr,print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.5,1) , colorize=TRUE
     , main = "ROC curve for h1 train data")
abline(0,1,lty=2)
as.numeric(performance(resort_pred_ROC_train_tr, "auc")@y.values)
#draw confusion matrix for train dataset in order to check overfitting
resort_train_matrix_tr = table(h1_train$is_canceled_f, resort_pred_train_tr[,2]>0.35)

#draw ROC curve and calculate area under the curve-test
resort_pred_test_tr <- predict(resort_tree_model, newdata = h1_test, type="prob")
resort_pred_ROC_tr <- prediction(resort_pred_test_tr[,2], h1_test$is_canceled_f)
resort_perf_ROC_tr <- performance(resort_pred_ROC_tr, "tpr", "fpr")
plot(resort_perf_ROC_tr, main = "ROC curve for h1 test data")
abline(0,1,lty=2)
resort_auc_tr <- as.numeric(performance(resort_pred_ROC_tr, "auc")@y.values)

#draw confusion matrix using the threshold
resort_Matrix_tr <- table(h1_test$is_canceled_f, resort_pred_test_tr[,2]>0.35)
#evaluation criteria
resort_accuracy_tr <- sum(diag(resort_Matrix_tr))/nrow(h1_test)
resort_specificity_tr <- resort_Matrix_tr[1,1]/(resort_Matrix_tr[1,1]+resort_Matrix_tr[1,2])
resort_sensitivity_tr <- resort_Matrix_tr[2,2]/(resort_Matrix_tr[2,1]+resort_Matrix_tr[2,2])
#-------------------decision tree on h2 data------------------------
#cross validation for parameter cp
set.seed(1725)
city_tree_cp <- train(is_canceled_f ~ ., data=h2_train, method="rpart", trControl= numFolds, tuneGrid= cpGrid)
#choosing the best cp
ggplot(city_tree_cp) + geom_text(aes(label=cp))
city_best_cp <- 0.007

set.seed(1725)  
#make tree decision model
city_tree_model <- rpart(is_canceled_f ~., data=h2_train, method="class", cp=city_best_cp)
pdf("city.pdf")
prp(city_tree_model)
dev.off()
#most important factors
vip(city_tree_model)

#draw ROC curve and calculate area under the curve-train
city_pred_train <- predict(city_tree_model, type="prob")
city_pred_ROC_train <- prediction(city_pred_train[, 2], h2_train$is_canceled_f)
city_perf_ROC_train <- performance(city_pred_ROC_train, "tpr", "fpr")
plot(city_perf_ROC_train,print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.5,1) , colorize=TRUE
     , main = "ROC curve for h2 train data")
abline(0,1,lty=2)
as.numeric(performance(city_pred_ROC_train, "auc")@y.values)

#draw confusion matrix for train dataset in order to check overfitting
city_train_matrix_tr = table(h2_train$is_canceled_f, city_pred_train[,2]>0.65)

#draw ROC curve and calculate area under the curve-test
city_pred_test_tr <- predict(city_tree_model, newdata = h2_test, type="prob")
city_pred_ROC_tr <- prediction(city_pred_test_tr[,2], h2_test$is_canceled_f)
city_perf_ROC_tr <- performance(city_pred_ROC_tr, "tpr", "fpr")
plot(city_perf_ROC_tr, main = "ROC curve for h2 test data")
abline(0,1,lty=2)
city_auc_tr <- as.numeric(performance(city_pred_ROC_tr, "auc")@y.values)

#draw confusion matrix using the threshold
city_Matrix_tr <- table(h2_test$is_canceled_f, city_pred_test_tr[,2]>0.65)
#evaluation criteria
city_accuracy_tr <- sum(diag(city_Matrix_tr))/nrow(h2_test)
city_specificity_tr <- city_Matrix_tr[1,1]/(city_Matrix_tr[1,1]+city_Matrix_tr[1,2])
city_sensitivity_tr <- city_Matrix_tr[2,2]/(city_Matrix_tr[2,1]+city_Matrix_tr[2,2])

#-------------------random forest on h1 data----------------
#set mtry
def_mtry <- sqrt(ncol(h1_train)-4)
#make random forest model
set.seed(1725)
resort_rfmod <- randomForest(is_canceled_f ~ . , data=h1_train, method="class", nodsize=10, ntree=400, mtry=def_mtry)
resort_rfmod #confusion matrix-train

resort_ROC_rf <- predict(resort_rfmod, newdata = h1_test, type="prob")
resort_ROC_pred <- prediction(resort_ROC_rf[,2], h1_test$is_canceled_f)
resort_auc_rf <- as.numeric(performance(resort_ROC_pred, "auc")@y.values)

resort_pred_test_rf <- predict(resort_rfmod, newdata = h1_test, type="class")
#draw confusion matrix-test
resort_Matrix_rf <- table(h1_test$is_canceled_f, resort_pred_test_rf)
#evaluation criteria
resort_accuracy_rf <- sum(diag(resort_Matrix_rf))/nrow(h1_test)
resort_specificity_rf <- resort_Matrix_rf[1,1]/(resort_Matrix_rf[1,1]+resort_Matrix_rf[1,2])
resort_sensitivity_rf <- resort_Matrix_rf[2,2]/(resort_Matrix_rf[2,1]+resort_Matrix_rf[2,2])
#-------------------random forest on h2 data--------------
#set mtry
def_mtry <- sqrt(ncol(h2_train)-4)
#make random forest model
set.seed(1725)
city_rfmod <- randomForest(is_canceled_f ~ . , data=h2_train, method="class", nodsize=10, ntree=400, mtry=def_mtry)
city_rfmod #confusion matrix-train

city_ROC_rf <- predict(city_rfmod, newdata = h2_test, type="prob")
city_ROC_pred <- prediction(city_ROC_rf[,2], h2_test$is_canceled_f)
city_auc_rf <- as.numeric(performance(city_ROC_pred, "auc")@y.values)

city_pred_test_rf <- predict(city_rfmod, newdata = h2_test, type="class")
#draw confusion matrix-test
city_Matrix_rf <- table(h2_test$is_canceled_f, city_pred_test_rf)
#evaluation criteria
city_accuracy_rf <- sum(diag(city_Matrix_rf))/nrow(h2_test)
city_specificity_rf <- city_Matrix_rf[1,1]/(city_Matrix_rf[1,1]+city_Matrix_rf[1,2])
city_sensitivity_rf <- city_Matrix_rf[2,2]/(city_Matrix_rf[2,1]+city_Matrix_rf[2,2])

