

corpus <- c("Airlines", "ClintonTrump", "Food", "Movies", "TripAdvisor", "TripAdvisor2", "Twitter")
herramientas <- c("BingSentiment", "CoreNLPSentiment", "MCSentiment", "MicrosoftSentiment", "SentiStrSentiment")
tecnicas = c("knn", "J48", "svmLinear", "rpart", "C5.0")

abreviacion <- function(herramienta, tema, tecnica)
{
  if(herramienta == "BingSentiment")
  {
    h = "BS"
  }
  else if (herramienta == "CoreNLPSentiment")
  {
    h = "C-NLP"
  }
  else if (herramienta == "MCSentiment")
  {
    h = "MC"
  }
  else if (herramienta == "MicrosoftSentiment")
  {
    h = "MS"
  }
  else if (herramienta == "SentiStrSentiment")
  {
    h = "SSS"
  }
  
  if(tema == "Airlines")
  {
    t = "A"
  }
  else if (tema == "ClintonTrump")
  {
    t = "C&T"
  }
  else if (tema == "Food")
  {
    t = "F"
  }
  else if (tema == "Movies")
  {
    t = "M"
  }
  else if (tema == "TripAdvisor")
  {
    t = "TA1"
  }
  else if (tema == "TripAdvisor2")
  {
    t = "TA2"
  }
  else if (tema == "Twitter")
  {
    t = "Tw"
  }
  
  if(tecnica == "knn")
  {
    tc = "knn"
  }
  else if (tecnica == "J48")
  {
    tc = "J48"
  }
  else if (tecnica == "svmLinear")
  {
    tc = "svm"
  }
  else if (tecnica == "C5.0")
  {
    tc = "C5"
  }
  else if (tecnica == "rpart")
  {
    tc = "rp"
  }
  
  etiqueta = paste(h,t,tc, sep = "/")
  return(etiqueta)
}

# Etiquetas de máquina
ruta <- toString(paste0("./data/graficas/All_AUC-Accuracy.txt"))
cont = 1
sink(ruta)
print(toString(paste("ID", "AUC", "Accuracy", "Exp")))
for(i in 1:length(herramientas))
{
  herramienta = herramientas[i]
  
  for(j in 1:length(corpus))
  {
    tema = corpus[j]
    
    for(k in 1:length(tecnicas))
    {
      tecnica = tecnicas[k]
      
      datos <- read.csv(file = toString(paste("./data/result/20Word/", herramienta," ", tema," ", tecnica," ", ".csv", sep = "")))
      accuracy_test = datos[21,2]
      auc_test = datos[23, 2]
      
      a = abreviacion(herramienta, tema, tecnica)
      print(toString(paste(cont, auc_test, accuracy_test, a)))
      
      cont = cont + 1
      
    }
  }
}
sink()


# Etiquetas de experto

ruta <- toString(paste0("./data/graficas/All_AUC-Accuracy-SimpleModelEtiqExperto2.txt"))
cont = 1
sink(ruta)
print(toString(paste("ID", "AUC", "Accuracy", "Exp")))
for(i in 1:length(herramientas))
{
  herramienta = herramientas[i]
  
  for(j in 1:length(corpus))
  {
    tema = corpus[j]
    
    if(!(tema == "Movies" && herramienta=="BingSentiment"))
    {
      for(k in 1:length(tecnicas))
      {
        tecnica = tecnicas[k]
        
        datos <- read.csv(file = toString(paste("./data/result/etiquetasExperto/result ", herramienta," ", tema," ", tecnica," ", ".csv", sep = "")))
        accuracy_test = datos[21,2]
        auc_test = datos[23, 2]
        
        a = abreviacion(herramienta, tema, tecnica)
        print(toString(paste(cont, auc_test, accuracy_test, a)))
        
        cont = cont + 1
        
      }
    }
  }
}
sink()


# Segunda parte


# etiquetas experto
ruta <- toString(paste0("./data/graficas/graficaBarrilEtiqExperto2.txt"))
sink(ruta)
print(toString(paste("ID", "AUC", "Accuracy","HerramientaTrain", "Algorithm", "Corpus")))
for(j in 1:length(corpus))
{
  tema = corpus[j]
  
  
  cont = 1
  
  for(i in 1:length(herramientas))
  {
    herramienta = herramientas[i]
    if(!(tema == "Movies" && herramienta=="BingSentiment"))
    {
      for(k in 1:length(tecnicas))
      {
        tecnica = tecnicas[k]
        
        datos <- read.csv(file = toString(paste("./data/result/etiquetasExperto/result ", herramienta," ", tema," ", tecnica," ", ".csv", sep = "")))
        accuracy_test = datos[21,2]
        auc_test = datos[23, 2]
        
        a = abreviacion(herramienta, tema, tecnica)
        print(toString(paste(cont, auc_test, accuracy_test, a, tecnica, tema)))
        
        cont = cont + 1
        
      }
    }
  }
}
sink()


# Etiquetas maquina
ruta <- toString(paste0("./data/graficas/graficaBarrilEtiqMaquina.txt"))
sink(ruta)
print(toString(paste("ID", "AUC", "Accuracy","HerramientaTrain", "Algorithm", "Corpus")))
for(j in 1:length(corpus))
{
  tema = corpus[j]
  
  
  cont = 1
  
  for(i in 1:length(herramientas))
  {
    herramienta = herramientas[i]
    
    for(k in 1:length(tecnicas))
    {
      tecnica = tecnicas[k]
      
      datos <- read.csv(file = toString(paste("./data/result/20Word/", herramienta," ", tema," ", tecnica," ", ".csv", sep = "")))
      accuracy_test = datos[21,2]
      auc_test = datos[23, 2]
      
      a = abreviacion(herramienta, tema, tecnica)
      print(toString(paste(cont, auc_test, accuracy_test, a, tecnica, tema)))
      
      cont = cont + 1
      
    }
  }
}
sink()







