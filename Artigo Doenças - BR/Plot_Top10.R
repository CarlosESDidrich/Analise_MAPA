#Bibliotecas
library(readr)
library(tidyverse)
library(stringdist)
library(readxl)
library(writexl)


#Somar quantidade de animais por diagnósticos
soma<- c()  
for (i in unique(dados$Diagnosticos_N)) {
  soma[i] <- sum(dados$QUANTIDADE[dados$Diagnosticos_N == i])
}
dados$somaDIAG <- soma[dados$Diagnosticos_N]
dados <-dados[order(dados$somaDIAG, decreasing = TRUE),]

#TOP 10 diagósticos por quantidade de animai
dados %>% distinct(dados$Diagnosticos_N,dados$somaDIAG) 


## Filtro dos 10 diagnosticos com maiores quantidades de animais
diags10<-dados %>% 
  filter(Diagnosticos_N == 128 | 
           Diagnosticos_N == 125 | 
           Diagnosticos_N ==115 | 
           Diagnosticos_N ==98 | 
           Diagnosticos_N == 140 | 
           Diagnosticos_N == 47| 
           Diagnosticos_N == 23 | 
           Diagnosticos_N == 242| 
           Diagnosticos_N == 374 | 
           Diagnosticos_N == 54 )

# Distict para ficar só 1 de cada para gráfico
diags10<- distinct(diags10, Diagnosticos_N, .keep_all = TRUE)

diags10$Diagnosticos_N <- recode (diags10$Diagnosticos_N,
                                   "128" ="Contusão",
                                   "125" ="Contaminação",
                                   "115" ="Colibacilose",
                                   "98" ="Celulite",
                                   "140" =  "Dermatose",
                                   "47" =  "Artrite",
                                   "23"=  "Aerossaculite",
                                   "242"=  "Lesão Traumática",
                                   "374"=  "Salmonelose",
                                   "54"=  "Aspecto Repu")


library(ggplot2)
library(esquisse)
#esquisser(diags10)

diags10 %>% 
  ggplot(aes(x = Diagnosticos_N, y = somaDIAG,fill =somaDIAG))+
  geom_col()+
  scale_fill_gradient(low = "#3999F9", high = "#072F4D") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))



### Estados com mais animais  diagnosticados

#Somar quantidade de animais por estado
soma<- c()  
for (i in unique(dados$UF_N)) {
  soma[i] <- sum(dados$QUANTIDADE[dados$UF_N == i])
}
dados$somaUF <- soma[dados$Diagnosticos_N]
dados <-dados[order(dados$somaUF, decreasing = TRUE),]

# 10 mais 
dados %>% distinct(dados$UF_PROCEDENCIA,dados$somaUF) 



## Filtro dos 10 diagnosticos com maiores quantidades de animais
UF10<-dados %>% 
  filter(UF_ESTABELECIMENTO == "PR" | 
           UF_ESTABELECIMENTO == "SC" | 
           UF_ESTABELECIMENTO =="RS" | 
           UF_ESTABELECIMENTO =="GO" | 
           UF_ESTABELECIMENTO == "MG" | 
           UF_ESTABELECIMENTO == "SP"| 
           UF_ESTABELECIMENTO == "MS" | 
           UF_ESTABELECIMENTO == "MT"| 
           UF_ESTABELECIMENTO == "TO" | 
           UF_ESTABELECIMENTO == "BA" )

# Distict para ficar só 1 de cada para gráfico
UF10<- distinct(UF10, UF_ESTABELECIMENTO, .keep_all = TRUE)
UF10$log<-log(UF10$somaUF)

UF10 %>% 
  ggplot(aes(x = UF_ESTABELECIMENTO, y = somaUF,fill =somaUF))+
  geom_col()+
  scale_fill_gradient(low = "#3999F9", high = "#072F4D") +
  theme_minimal()

