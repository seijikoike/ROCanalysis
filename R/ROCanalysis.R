#' Receiver Operating Characteristic Analysis
#'
#' This function will create an ROC curve to visually demonstrate the performance of a logistic model.
#' @param probList A vector of classification probabilities.
#' @param Outcome A vector of the responses (can either be factors or 0/1).
#' @param mat Option to include a confusion matrix when its value is in the interval [0, 1]. Defaults to NULL.
#' @param ROCcurve Creates an ROC curve with ggplot2. This includes the AUC. Defaults to TRUE.
#' @param ACCcurve Creates an optimal cutoff point graph. Defaults to TRUE.
#' @keywords ROC curve
#' @keywords classification
#' @export
#' @examples
#' library(ISLR)
#' data("Weekly")
#'
#'set.seed(1439)
#'train <- sample(nrow(Weekly), floor(nrow(Weekly)) * 0.75)
#'test.set <- Weekly[-train,]
#'#'
#'attach(Weekly)
#'log.model <- glm(Direction ~ Lag2, data = Weekly, subset = train, family = binomial)
#'
#'predicted <- predict.glm(log.model, newdata = test.set, type = "response")
#'
#'roc.log <- ROCanalysis(predicted, test.set$Direction, mat = 0.5)
#'roc.log$roccurve
#'roc.log$acccurve
#'roc.log$confusionMatrix

ROCanalysis <- function(probList, Outcome, mat = NULL, ROCcurve = TRUE, AccCurve = TRUE){
  require(ggplot2)
  if(is.factor(Outcome) == TRUE){
    lvl <- levels(Outcome)
    con <- contrasts(Outcome)
    Outcome <- ifelse(Outcome == lvl[1], con[1], con[2])}

  segments <- sort(c(1, probList))
  Sensitivity <- rep(NA, length(segments))
  Specificity <- rep(NA, length(segments))
  Accuracy <- rep(NA, length(segments))

  for(k in 1:length(segments)){
    TP <- sum(probList >= segments[k] & Outcome == 1)
    FN <- sum(probList < segments[k] & Outcome == 1)
    TN <- sum(probList < segments[k] & Outcome == 0)
    FP <- sum(probList >= segments[k] & Outcome == 0)

    Sensitivity[k] <- TP/(TP + FN)

    Specificity[k] <- TN/(FP + TN)

    Accuracy[k] <- (TP + TN)/(TP + TN + FP + FN)
  }

  maxAccuracy <- max(Accuracy)

  optimalCutoff <- segments[which(Accuracy == maxAccuracy)]

  ROC.data.frame <- data.frame(Cutoff = segments,
                               Sensitivity = Sensitivity,
                               Specificity = Specificity,
                               Accuracy = Accuracy)

  ROCList <- list(data = ROC.data.frame,
                  optimal = list("maxAccuracy" = maxAccuracy,
                                 "optimalCutoff" = optimalCutoff))

  Confusion.matrix <- matrix(NA, nrow = 2, ncol = 2)
  colnames(Confusion.matrix) <- c("True", "False")
  rownames(Confusion.matrix) <- c("True", "False")
  ACC <- NA

  if(is.null(mat) == FALSE){
    if(mat >= 0 & mat <= 1){
      Confusion.matrix[1, 1] <- sum(probList >= mat & Outcome == 1)
      Confusion.matrix[2, 1] <- sum(probList < mat & Outcome == 1)
      Confusion.matrix[1, 2] <- sum(probList >= mat & Outcome == 0)
      Confusion.matrix[2, 2] <- sum(probList < mat & Outcome == 0)
      ACC <- (Confusion.matrix[1, 1] + Confusion.matrix[2, 2]) / sum(Confusion.matrix)
    }
    else(print("Cutoff for confusion matrix must be between 0 and 1"))
    ROCList$confusionMatrix = list("confusionMatrix" = Confusion.matrix,
                                   "Accuracy" = ACC)
  }

  positive <- probList[Outcome == 1]
  negative <- probList[Outcome == 0]

  sim <- replicate(50000,sample(positive, 1) > sample(negative, 1))

  auc <- round(mean(sim), 4)

  roccurve <- ggplot(ROC.data.frame, aes(x = 1 - Specificity, y = Sensitivity, colour = Accuracy)) +
    geom_line(size = 1) +
    annotate("text", x = 1, y = Inf, parse = TRUE,
             label = paste("AUC %~~%", auc),
             hjust = 1, vjust = 1.5) +
    geom_abline(slope = 1, intercept = 0) +
    scale_x_continuous("False Positive Rate", seq(0, 1, 0.1)) +
    scale_y_continuous("True Positive Rate", seq(0, 1, 0.1)) +
    ggtitle("ROC Curve") +
    scale_colour_gradient2(low = "#0072B2", mid = "grey", high = "#E69F00", midpoint = 0.5) +
    theme(panel.grid.minor = element_line(colour = "grey"))

  acccurve <- ggplot(ROC.data.frame, aes(x = Cutoff, y = Accuracy, colour = Accuracy)) +
    geom_line(size = 1) +
    scale_x_continuous("Cutoff", seq(0, 1, 0.1)) +
    scale_y_continuous("Accuracy") +
    ggtitle("Accuracy/Cutoff Optimization") +
    scale_colour_gradient2(low = "#0072B2", mid = "grey", high = "#E69F00", midpoint = 0.5) +
    geom_vline(xintercept = optimalCutoff, linetype = "dashed") +
    theme(panel.grid.minor = element_line(colour = "grey")) +
    annotate("text", x = 1, y = Inf, parse = TRUE,
             label = paste("Maximum ~ Accuracy %~~%", round(maxAccuracy, 4)),
             hjust = 1, vjust = 1.5)

  if(ROCcurve == TRUE){ROCList$roccurve = roccurve}
  if(AccCurve == TRUE){ROCList$acccurve = acccurve}

  return(ROCList)
}

