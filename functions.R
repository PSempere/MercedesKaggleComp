library(e1071)
library(caret)

computeR2 <- function(alg_name, actual_vector, preds_vector)
{
    #puntuar R2
    r2 <- R2_Score(y_pred = preds_vector, y_true = actual_vector)
    #mostrar la puntuacion 
    res <- paste(alg_name, 'R2 Score:', r2)
    return(res)
}

fit_model <- function(alg, form, gr, data_to_fit)
{
    model <- NULL

    rand_cntrl <- trainControl(method = "repeatedcv", repeats = 10, search = "random")

    set.seed(1234)

    rand_search <- train(form, data=data_to_fit, method="svmRadial", tuneLength=50, metric="Rsquared", trControl=rand_cntrl)

    #hypertune!

    model <- tune(alg, train.x = form, data = data_to_fit)
    
    return(model)
}