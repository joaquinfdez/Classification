csv <- data.frame(fix.empty.names = TRUE)

corpus <- c("Airlines", "ClintonTrump", "Food", "Movies", "TripAdvisor", "TripAdvisor2", "Twitter")
herramientas <- c("BingSentiment", "CoreNLPSentiment", "MCSentiment", "MicrosoftSentiment", "SentiStrSentiment")
tecnicas = c("knn", "J48", "svmLinear", "rpart", "C5.0")

cont = 0

for(k in 1:length(tecnicas))
{
  tecnica = tecnicas[k]
  
  for(j in 1:length(corpus))
  {
    tema = corpus[j]
    
    accuracy_media = 0
    auc_media = 0
    
    accuracy = 0
    auc = 0
    
    for(i in 1:length(herramientas))
    {
      herramienta = herramientas[i]

    
      datos <- read.csv(file = toString(paste("./data/result/primer/", herramienta," ", tema," ", tecnica," ", ".csv", sep = "")), stringsAsFactors = FALSE)
      accuracy = accuracy + as.numeric(datos[21,2]) 
      auc = auc + as.numeric(datos[23, 2])
    }
    accuracy_media = accuracy / length(herramientas)
    auc_media = auc / length(herramientas)
    

    csv[1+cont,1] = tecnica
    csv[1+cont,2] = tema
    csv[1+cont,3] = accuracy_media
    csv[1+cont,4] = auc_media

    cont = cont + 1
    
  }
}
write.table(csv, file = toString("./data/result/valoresMedios.csv"), row.names = FALSE, sep = ",")