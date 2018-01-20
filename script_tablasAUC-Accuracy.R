library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
# TABLA 1
f = toString("./data/graficas/AUC-Accuracy-EtiqExperto2.png")
png(file = f, width = 800, height = 500 )
AucAccuracy <- read.delim("./data/graficas/All_AUC-Accuracy-SimpleModelEtiqExperto2.txt", sep = " ")
ggplot(AucAccuracy, aes(x=AUC, y=Accuracy)) + geom_point(aes(size = 0.5*(AUC +Accuracy), colour=0.5*(AUC +Accuracy))) + geom_text(aes(label=ifelse(Accuracy>0.5,as.character(ID),'')), hjust=-2, vjust=0.5) +
  labs(title="Mejor Modelo (AUC vs Accuracy)", x="AUC Test", y = "Accuracy Test") + 
  theme(axis.text.x = element_text(hjust = 1, size=12), 
        axis.text.y = element_text(size=12),
        axis.title=element_text(size=15,face="bold"),
        title=element_text(size=16,face="bold")) +
  scale_colour_gradient(low="#ffaaaa", high="#ff0000")+
  guides(color=guide_legend(), size = guide_legend())
dev.off()

f = toString("./data/graficas/AUC-Accuracy-EtiqExperto.png")
png(file = f, width = 800, height = 500 )
AucAccuracy <- read.delim("./data/graficas/All_AUC-Accuracy-SimpleModelEtiqExperto.txt", sep = " ")
ggplot(AucAccuracy, aes(x=AUC, y=Accuracy)) + geom_point(aes(size = 0.5*(AUC +Accuracy), colour=0.5*(AUC +Accuracy))) + geom_text(aes(label=ifelse(Accuracy>0.5,as.character(ID),'')), hjust=-2, vjust=0.5) +
  labs(title="Mejor Modelo (AUC vs Accuracy)", x="AUC Test", y = "Accuracy Test") + 
  theme(axis.text.x = element_text(hjust = 1, size=12), 
        axis.text.y = element_text(size=12),
        axis.title=element_text(size=15,face="bold"),
        title=element_text(size=16,face="bold")) +
  scale_colour_gradient(low="#ffb89e", high="#ff4805")+
  guides(color=guide_legend(), size = guide_legend())
dev.off()

# TABLA 2 Expertos y Maquina
corpus <- c("Airlines", "ClintonTrump", "Food", "Movies", "TripAdvisor", "TripAdvisor2", "Twitter")
AlgorE <- read.delim("./data/graficas/graficaBarrilEtiqExperto2.txt", sep = " ", stringsAsFactors = FALSE)
AlgorM <- read.delim("./data/graficas/graficaBarrilEtiqMaquina.txt", sep = " ", stringsAsFactors = FALSE)
for(i in 1:nrow(AlgorM))
{
  AlgorM$Algorithm[i] = paste0(toString(AlgorM$Algorithm[i]), "-M")
}

for(i in 1:nrow(AlgorE))
{
  AlgorE$Algorithm[i] = paste0(toString(AlgorE$Algorithm[i]), "-E")
}
union = data.frame(matrix(data = 0, nrow = (nrow(AlgorE) + nrow(AlgorM)), ncol = ncol(AlgorE)), stringsAsFactors = FALSE)
colnames(union)[1] = "ID"
colnames(union)[2] = "AUC"
colnames(union)[3] = "Accuracy"
colnames(union)[4] = "HerramientaTrain"
colnames(union)[5] = "Algorithm"
colnames(union)[6] = "Corpus"
cont = 1
for(i in 1:nrow(AlgorE))
{
  union[cont,] = AlgorE[i,]
  cont = cont + 1
}
for(i in 1:nrow(AlgorM))
{
  union[cont,] = AlgorM[i,]
  cont = cont + 1
}


# Algor <- read.delim("./data/graficas/graficaBarrilEtiqExpertoMaquina.txt", sep = " ")
Algor <- union
# for(j in 1:length(corpus))
# {
# j = 7
#   tema = corpus[j]
tema = "Movies"
  f = toString(paste("./data/graficas/gBEtiqExperto", tema, ".png", sep = " "))
  titulo = toString(paste("AUC of", tema, "Experiments", sep = " "))
  png(file = f, width = 600, height = 300 )

    ggplot(Algor[Algor$Corpus == tema,], aes(factor(Algorithm), AUC)) + 
    geom_boxplot(aes(fill = factor(Algorithm))) +
    scale_fill_manual(values=c("chartreuse3", "chartreuse3", "coral", "coral", "cyan", "cyan", "brown1", "brown1", "magenta4", "magenta4")) +
    labs(title = titulo , x="Algorithm", y = "AUC Test") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size=12), 
          axis.text.y = element_text(size=12),
          axis.title=element_text(size=15,face="bold"),
          title=element_text(size=12,face="bold")) 
    
    dev.off()

# }

  
  # TABLA 3 maquina
  corpus <- c("Airlines", "ClintonTrump", "Food", "Movies" ,"TripAdvisor", "TripAdvisor2", "Twitter")
  Algor <- read.delim("./data/graficas/graficaBarrilEtiqExperto.txt", sep = " ")
  
  # for(j in 1:length(corpus))
  # {
  j = 7
  tema = corpus[j]
  f = toString(paste("./data/graficas/gBEtiqExperto", tema, ".png", sep = ""))
  titulo = toString(paste("AUC of", tema, "Experiments", sep = " "))
  
  png(file = f, width = 600, height = 300 )
  
  ggplot(Algor[Algor$Corpus == tema,], aes(factor(Algorithm), AUC)) + 
    geom_boxplot(aes(fill = factor(Algorithm))) +
    scale_fill_manual(values=c("chartreuse3", "coral","cyan", "brown1", "magenta4")) +
    labs(title = titulo , x="Algorithm", y = "AUC Test") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size=12), 
          axis.text.y = element_text(size=12),
          axis.title=element_text(size=15,face="bold"),
          title=element_text(size=12,face="bold")) 
  dev.off()
  # ggsave(filename = f, width = 600, height = 300)
  # }
