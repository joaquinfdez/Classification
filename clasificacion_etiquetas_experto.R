library(RWeka)
library(caret)
library(class)
library(gmodels)
library(e1071)
library(klaR)
library(rpart)
library(pROC)
library(RXKCD)
library(NLP)
library(tm)
library(RColorBrewer)
library(wordcloud)
library(data.table)
library(SnowballC)
library(textstem)

imprime.csv <-function(m_train, accuracy, precision, recall, m_test, accuracy_test, AUC_test, ruta)
{
  
  csv <- data.frame(fix.empty.names = TRUE)
  
  # Empezamos con los datos de TRAIN
  csv[1,1] = "TRAIN"
  
  # Matriz de confusión
  csv[2,1] = "prediction-reference"
  csv[3,1] = "negative"
  csv[4,1] = "neutral"
  csv[5,1] = "positive"
  
  csv[2,2] = "negative"
  csv[2,3] = "neutral"
  csv[2,4] = "positive"
  
  for(i in 1:3)
  {
    for(j in 1:3)
    {
      csv[i+2,j+1] = as.integer(m_train[i,j])
    }
  }
  
  # Accucacy
  csv[7,1] = "Accuracy"
  csv[7,2] = accuracy
  
  # Precision
  csv[10,1] = "Precision"
  csv[9,2] = "negative"
  csv[9,3] = "neutral"
  csv[9,4] = "positive"
  
  csv[10,2] = precision["neutral"]
  csv[10,3] = precision["negative"]
  csv[10,4] = precision["positive"]
  
  # Recall
  csv[13,1] = "Recall"
  csv[12,2] = "negative"
  csv[12,3] = "neutral"
  csv[12,4] = "positive"
  
  csv[13,2] = recall["neutral"]
  csv[13,3] = recall["negative"]
  csv[13,4] = recall["positive"]
  
  # Comenzamos con el conjunto TEST
  csv[15,1] = "TEST"
  m_test = matriz_test$table
  accuracy_test = matriz_test$overall[1]
  accuracy_test
  
  # Matriz de confusión
  csv[2+14,1] = "prediction-reference"
  csv[3+14,1] = "negative"
  csv[4+14,1] = "neutral"
  csv[5+14,1] = "positive"
  
  csv[2+14,2] = "negative"
  csv[2+14,3] = "neutral"
  csv[2+14,4] = "positive"
  
  for(i in 1:3)
  {
    for(j in 1:3)
    {
      csv[i+2+14,j+1] = as.integer(m_test[i,j])
    }
  }
  
  csv[21,1] = "Accucacy TEST"
  csv[21,2] = accuracy_test[1]
  
  csv[23, 1] = "AUC"
  csv[23, 2] = AUC_test
  
  for(i in 1:nrow(csv))
  {
    for(j in 1:ncol(csv))
    {
      palabra = as.String("NA")
      if(as.String(csv[i,j]) == palabra)
      {
        csv[i,j] = " "
      }
    }
    
  }
  write.table(csv, file = ruta, row.names = FALSE, sep = ",")  
  
}


imprime.txt <- function(ruta, m_train, accucacy, precision, recall, fscore, gmeasure,
                        time2, time1, matriz_test, accuracy_test, AUC_test)
{
  sink(ruta)
  print("********************************************************")
  print(" ")
  print(c("Algoritmo usado: ", algoritmo, "+++++++++++++++++++"))
  print(c("Corpus: ", tema))
  print(" ")
  print("********************************************************")
  print("************************TRAIN****************************")
  print("********************************************************")
  print(" ")
  
  
  print(m_train)
  print(accuracy)
  
  # Precision: ************************************************************
  precision.positive <- precision["positive"]
  precision.negative <- precision["negative"]
  precision.neutral <- precision["neutral"]
  print(paste0("Precision: positive: ", precision.positive))
  print(paste0("Precision: negative: ", precision.negative))
  print(paste0("Precision: neutral: ", precision.neutral))
  
  # Recall: tp/(tp + fn):
  recall.positive <- recall["positive"]
  recall.negative <- recall["negative"]
  recall.neutral <- recall["neutral"]
  print(paste0("Recall: positive: ", recall.positive))
  print(paste0("Recall: negative: ", recall.negative))
  print(paste0("Recall: neutral: ", recall.neutral))
  
  # F-Score: 2 * precision * recall /(precision + recall):
  print(paste0("FscoreTRAIN: ", fscore))
  
  # G-measure: sqrt(precision*recall)
  
  print(paste0("GmeasureTRAIN: ", gmeasure))
  
  print(paste0("Tiempo de entrenamiento:", time2-time1))
  
  print(" ")
  print("********************************************************")
  print("************************TEST****************************")
  print("********************************************************")
  print(" ")
  # Predict the labels of the test set
  
  print( matriz_test)
  print(accuracy_test)
  print(paste0("AUC: ", AUC_test))
  
  sink()
}

multiClassSummary <- function (data, lev = NULL, model = NULL){
  
  #Load Libraries
  require(Metrics)
  require(caret)
  
  #Check data
  if (!all(levels(data[, "pred"]) == levels(data[, "obs"]))) 
    stop("levels of observed and predicted data do not match")
  
  #Calculate custom one-vs-all stats for each class
  prob_stats <- lapply(levels(data[, "pred"]), function(class){
    
    #Grab one-vs-all data for the class
    pred <- ifelse(data[, "pred"] == class, 1, 0)
    obs  <- ifelse(data[,  "obs"] == class, 1, 0)
    prob <- data[,class]
    
    #Calculate one-vs-all AUC and logLoss and return
    cap_prob <- pmin(pmax(prob, .000001), .999999)
    prob_stats <- c(auc(obs, prob), logLoss(obs, cap_prob))
    names(prob_stats) <- c('ROC', 'logLoss')
    return(prob_stats) 
  })
  prob_stats <- do.call(rbind, prob_stats)
  rownames(prob_stats) <- paste('Class:', levels(data[, "pred"]))
  
  #Calculate confusion matrix-based statistics
  CM <- confusionMatrix(data[, "pred"], data[, "obs"])
  
  #Aggregate and average class-wise stats
  #Todo: add weights
  # RES: support two classes here as well
  #browser() # Debug
  if (length(levels(data[, "pred"])) == 2) {
    class_stats <- c(CM$byClass, prob_stats[1,])
  } else {
    class_stats <- cbind(CM$byClass, prob_stats)
    class_stats <- colMeans(class_stats)
  }
  
  # Aggregate overall stats
  overall_stats <- c(CM$overall)
  
  # Combine overall with class-wise stats and remove some stats we don't want 
  stats <- c(overall_stats, class_stats)
  stats <- stats[! names(stats) %in% c('AccuracyNull', 
                                       'Prevalence', 'Detection Prevalence')]
  
  # Clean names
  names(stats) <- gsub('[[:blank:]]+', '_', names(stats))
  
  if (length(levels(data[, "pred"]) == 2)) {
    # Change name ordering to place most useful first
    # May want to remove some of these eventually
    stats <- stats[c("ROC", "Sensitivity", "Specificity", "Accuracy", "Kappa", "logLoss",
                     "AccuracyLower", "AccuracyUpper", "AccuracyPValue", "McnemarPValue",
                     "Pos_Pred_Value", "Neg_Pred_Value", "Detection_Rate",
                     "Balanced_Accuracy")]
  }
  
  return(stats)
}


herramientas <- c("BingSentiment", "CoreNLPSentiment", "MCSentiment", "MicrosoftSentiment", "SentiStrSentiment")
tecnicas = c("knn", "J48", "svmLinear", "rpart", "C5.0")
corpus <- c( "Airlines","ClintonTrump","Food", "Movies", "TripAdvisor", "TripAdvisor2", "Twitter")

tot_herramientas <- length(herramientas)

for(h in 1:tot_herramientas)
{
  # h = 1
  herramienta = herramientas[h]
  
  for(t in 1:length(corpus))
  {
  tema = corpus[t]
    
    # tema = "Movies"
    
    print(c("Tema: ", tema, " con herramienta: ", herramientas[h]))
    
    # *********************************************************************************
    #   Leemos los datos y trabajamos con las 30 palabras con mayor TF*IDF
    # *********************************************************************************
    
    Messages <- read.csv(file = toString(paste("./data/datasets/aprendizaje/dataset_train", herramientas[h], tema ,".csv", sep = " ")))
    
    colnames(Messages)[1:(ncol(Messages)-1)] <- paste0("X", 1:(ncol(Messages)-1)) #Cambiamos los nombres por variables X1,X2, ..., Xn
    colnames(Messages)[ncol(Messages)] = "Sentiment"
    Messages$Sentiment <- factor(Messages$Sentiment, c("positive", "neutral", "negative"))
    
    messages.test <- Messages[1:50,]
    # La parte de test llevará la etiqueta de experto 
    etiquetas_experto <- read.csv(file = toString(paste("./data/etiquetasExperto/mensajes_id", tema ,".csv", sep = " ")))
    messages.test$Sentiment = etiquetas_experto$Sentiment
    messages.test$Sentiment <- factor(messages.test$Sentiment, c("positive", "neutral", "negative"))
    
    # La parte del training se entrenará con las distintas etiquetas de maquina CoreNLP,BingSentiment,...
    messages.training <- Messages[51:nrow(Messages),]
    messages.training$Sentiment <- factor(messages.training$Sentiment, c("positive", "neutral", "negative"))
    
    
    for(t in 1:length(tecnicas))
    {
      algoritmo = tecnicas[t]
      # algoritmo = "rpart"
      print(c("Algoritmo usado: ", algoritmo))
      
      # *********************************************************************************
      #   aprendizaje del algoritmo
      # *********************************************************************************
      
      set.seed(12345)
      train_control<- trainControl(method="LGOCV", classProbs = TRUE,
                                   summaryFunction = multiClassSummary,
                                   allowParallel = TRUE)
      
      time1 <- Sys.time()
      model<- caret::train(Sentiment~., data=messages.training, trControl=train_control, 
                           method=algoritmo, metric = "ROC")
      time2 <- Sys.time()
      
      # *********************************************************************************
      #   Datos de Train
      # *********************************************************************************
      confMatrix<- confusionMatrix(model)
      m_train <- confMatrix$table
      confTable <- (nrow(Messages)/100)*(m_train)

      accuracy <- sum(diag(m_train)) / sum(m_train)
      precision <- diag(m_train) / rowSums(m_train)
      recall <- (diag(m_train) / colSums(m_train))
      fscore <- 2 * precision * recall /(precision + recall)
      gmeasure <- sqrt(precision * recall)
      
      # *********************************************************************************
      #   Datos de Test
      # *********************************************************************************
      levels=c("positive", "neutral", "negative")
      prediccion_probabilidad<-predict(object= model, messages.test[,1:(ncol(Messages)-1)], type="prob")

      write.table(prediccion_probabilidad, file = toString(paste("./data/experto/probabilidades", herramientas[h], tema, algoritmo,".csv", sep = " ")), row.names = FALSE, sep = ",")
      # 
      # 
      # predictions<-predict(object= model, messages.test[,1:(ncol(Messages)-1)])
      # ordered.status <- factor(predictions, levels, ordered = TRUE)
      # ordered.test <- factor(messages.test$Sentiment, levels, ordered = TRUE)
      # # predictions
      # AUC <- multiclass.roc( ordered.test, ordered.status, levels)  
      # matriz_test = confusionMatrix( predictions, messages.test$Sentiment)
      # 
      # m_test = matriz_test$table
      # accuracy_test = matriz_test$overall[1]
      # AUC_test = as.numeric(AUC$auc)
      # 
      # # *********************************************************************************
      # #   Guardamos datos en formato TXT del aprendizaje del algoritmo
      # # *********************************************************************************
      # ruta <- toString(paste("./data/result/etiquetasExperto/result",herramienta, tema, algoritmo,".txt", sep = " "))
      # imprime.txt(ruta, m_train, accucacy, precision, recall, fscore, gmeasure,
      #             time2, time1, matriz_test, accuracy_test, AUC_test)
      # 
      # # *********************************************************************************
      # #   Guardamos datos en formato CSV del aprendizaje del algoritmo
      # # *********************************************************************************
      # ruta <- toString(paste("./data/result/etiquetasExperto/result", herramienta, tema, algoritmo,".csv", sep = " "))
      # imprime.csv(m_train, accuracy, precision, recall, m_test, accuracy_test, AUC_test,ruta)
}
}
}
