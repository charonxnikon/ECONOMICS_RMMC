library(randomForest)
library(gdata)

prepare_df_train <- function(therap_train, therap_predict)
{
    
    therap_train <- therap_train[1:56, ]

    covid_stat <- read.xls("covid_moscow.xlsx")
    therap_predict$covid_all <- covid_stat$all[1:185]
    therap_predict$covid_in_day <- covid_stat$in_day[1:185]

    therap_train$covid_all <- covid_stat$all[186:241]
    therap_train$covid_in_day <- covid_stat$in_day[186:241]


    therap_train4 <- therap_train[, -c(5, 6, 7)] 
    therap_train5 <- therap_train[, -c(4, 6, 7)] 
    therap_train6 <- therap_train[, -c(4, 5, 7)] 
    therap_train7 <- therap_train[, -c(4, 5, 6)] 

    return (therap_train)
}

prepare_df_predict<- function(therap_train, therap_predict)
{
    therap_train <- therap_train[1:56, ]

    covid_stat <- read.xls("covid_moscow.xlsx")
    therap_predict$covid_all <- covid_stat$all[1:185]
    therap_predict$covid_in_day <- covid_stat$in_day[1:185]

    therap_train$covid_all <- covid_stat$all[186:241]
    therap_train$covid_in_day <- covid_stat$in_day[186:241]


    therap_train4 <- therap_train[, -c(5, 6, 7)] 
    therap_train5 <- therap_train[, -c(4, 6, 7)] 
    therap_train6 <- therap_train[, -c(4, 5, 7)] 
    therap_train7 <- therap_train[, -c(4, 5, 6)] 

    return (therap_predict)
}


therap_train <- read.csv("therap_train2.csv")
therap_predict <- read.csv("therap_predict.csv")

therap_train <- prepare_df_train(therap_train, therap_predict)
therap_predict<- prepare_df_train(therap_train, therap_predict)

otorin_train <- read.csv("otorin_train2.csv")
otorin_predict <- read.csv("otorin_predict.csv")

print(otorin_train)
dim(otorin_train)
otorin_train <- prepare_df_train(otorin_train, otorin_predict)
otorin_predict<- prepare_df_train(otorin_train, otorin_predict)


ophtal_train <- read.csv("ophtal_train2.csv")
ophtal_predict <- read.csv("ophtal_predict.csv")

ophtal_train <- prepare_df_train(ophtal_train, otorin_predict)
ophtal_predict<- prepare_df_train(ophtal_train, ophtal_predict)

surgeon_train <- read.csv("surgeon_train2.csv")
surgeon_predict <- read.csv("surgeon_predict.csv")

surgeon_train <- prepare_df_train(surgeon_train, otorin_predict)
surgeon_predict<- prepare_df_train(surgeon_train, surgeon_predict)

urolog_train <- read.csv("urolog_train2.csv")
urolog_predict <- read.csv("urolog_predict.csv")

urolog_train <- prepare_df_train(urolog_train, otorin_predict)
urolog_predict<- prepare_df_train(urolog_train, urolog_predict)




head(therap_train)
head(therap_predict)
print(therap_train$pr_more4)

set.seed(42)

head(therap_train4)
head(therap_train5)
head(therap_train6)
head(therap_train7)


split_train_test <- function(therap_train4)
{
    split <- runif(dim(therap_train4)[1]) > 0.2
    train <- therap_train4[split, ]
    test <- therap_train4[!split, ]
    
    return (data.frame(train = train, test = test))
}

write_and_info <- function(pred, test, rf)
{
    print(sqrt(sum((as.vector(pred - test[, 4]))^2))/length(pred))
                

    test$predict <- pred
    print(test)

    print(importance(rf)/sum(importance(rf)))
    
    
    
}


predict_therap <- function(therap_train,  therap_predict)
{
    therap_train4 <- therap_train[, -c(5, 6, 7)] 
    therap_train5 <- therap_train[, -c(4, 6, 7)] 
    therap_train6 <- therap_train[, -c(4, 5, 7)] 
    therap_train7 <- therap_train[, -c(4, 5, 6)] 

    split <- runif(dim(therap_train4)[1]) > 0.2
    train4 <- therap_train4[split, ]
    test4 <- therap_train4[!split, ]

    split <- runif(dim(therap_train5)[1]) > 0.2
    train5 <- therap_train5[split, ]
    test5 <- therap_train5[!split, ]

    split <- runif(dim(therap_train6)[1]) > 0.2
    train6 <- therap_train6[split, ]
    test6 <- therap_train6[!split, ]

    split <- runif(dim(therap_train7)[1]) > 0.2
    train7 <- therap_train7[split, ]
    test7 <- therap_train7[!split, ]
    
    print("WELCOME TO HELL")
    print(train5)
    print(class(train5))
    print(names(train5))
    rf4 <- randomForest(good_pat ~ ., train4)
    rf5 <- randomForest(pr_day01 ~ ., train5)
    rf6 <- randomForest(pr_day23 ~ ., train6)
    rf7 <- randomForest(pr_more4 ~ ., train7)
    
    print("WELCOME TO HELL 2")
    pred4 <- predict(rf4, test4[, -4])

    pred5 <- predict(rf5, test5[, -4])

    pred6 <- predict(rf6, test6[, -4])
    pred7 <- predict(rf7, test7[, -4])

    write_and_info(pred4, test4, rf4)
    therap_predict$good_pat <- round(predict(rf4, therap_predict))
    mean(good_pat)
    write_and_info(pred5, test5, rf5)
    therap_predict$pr_day01 <- round(predict(rf5, therap_predict))

    write_and_info(pred6, test6, rf6)
    therap_predict$pr_day23 <- round(predict(rf6, therap_predict))

    write_and_info(pred7, test7, rf7)
    therap_predict$pr_more4 <- round(predict(rf7, therap_predict))
    
    write.table(therap_predict, "final_therap.csv", row.names = FALSE)
}


predict_other <- function(therap_train,  therap_predict, name)
{
    therap_train4 <- therap_train[, -c(5, 6, 7)] 
    therap_train5 <- therap_train[, -c(4, 6, 7)] 
    therap_train6 <- therap_train[, -c(4, 5, 7)] 
    therap_train7 <- therap_train[, -c(4, 5, 6)] 

    split <- runif(dim(therap_train4)[1]) > 0.2
    train4 <- therap_train4[split, ]
    test4 <- therap_train4[!split, ]

    split <- runif(dim(therap_train5)[1]) > 0.2
    train5 <- therap_train5[split, ]
    test5 <- therap_train5[!split, ]

    split <- runif(dim(therap_train6)[1]) > 0.2
    train6 <- therap_train6[split, ]
    test6 <- therap_train6[!split, ]

    split <- runif(dim(therap_train7)[1]) > 0.2
    train7 <- therap_train7[split, ]
    test7 <- therap_train7[!split, ]
    
    print("WELCOME TO HELL")
    print(train5)
    print(class(train5))
    print(names(train5))
    rf4 <- randomForest(good_pat ~ ., train4)
    rf5 <- randomForest(pr_day05 ~ ., train5)
    rf6 <- randomForest(pr_day67 ~ ., train6)
    rf7 <- randomForest(pr_more8 ~ ., train7)
    
    print("WELCOME TO HELL 2")
    pred4 <- predict(rf4, test4[, -4])

    pred5 <- predict(rf5, test5[, -4])

    pred6 <- predict(rf6, test6[, -4])
    pred7 <- predict(rf7, test7[, -4])

    write_and_info(pred4, test4, rf4)
    therap_predict$good_pat <- round(predict(rf4, therap_predict))
    mean(good_pat)
    write_and_info(pred5, test5, rf5)
    therap_predict$pr_day01 <- round(predict(rf5, therap_predict))

    write_and_info(pred6, test6, rf6)
    therap_predict$pr_day23 <- round(predict(rf6, therap_predict))

    write_and_info(pred7, test7, rf7)
    therap_predict$pr_more4 <- round(predict(rf7, therap_predict))
    
    write.table(therap_predict, name, row.names = FALSE)
}





predict_therap(therap_train, therap_predict)
predict_therap(otorin_train, otorin_predict, "otorin_final.csv")
predict_therap(ophtal_train, ophtal_predict, "ophtal_final.csv")
predict_therap(surgeon_train, surgeon_predict, "surgeon_final.csv")
predict_therap(urolog_train, urolog_predict, "urolog_final.csv")
