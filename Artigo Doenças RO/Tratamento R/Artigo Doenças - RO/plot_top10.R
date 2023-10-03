#Bibliotecas
library(readr)
library(tidyverse)
library(stringdist)
library(readxl)
library(writexl)
library(esquisse)


### Diagnósticos


### Plotar as 10 doenças mais incidentes

# Quantidade de animais por doenças
soma<- c()  
for (i in unique(doenças$Diagnostico_N)) {
  soma[i] <- sum(doenças$QUANTIDADE[doenças$Diagnostico_N == i])
}
doenças$soma <- soma[doenças$Diagnostico_N]

doenças <-doenças[order(doenças$soma, decreasing = TRUE),]

doenças %>% distinct(doenças$Diagnostico_N) 
#2,22,50,29,21,109,11,111,106,18
doencas10<-doenças %>% 
  filter(Diagnostico_N == 2 | 
           Diagnostico_N == 22 | 
           Diagnostico_N ==50 | 
           Diagnostico_N ==29 | 
           Diagnostico_N == 21 | 
           Diagnostico_N == 109| 
           Diagnostico_N == 11 | 
           Diagnostico_N == 111| 
           Diagnostico_N == 106 | 
           Diagnostico_N == 18 )




doencas10$Diagnostico_N <- recode (doencas10$Diagnostico_N,
                                   "2" ="Abscesso",
                                   "22" ="Aspecto Repugnante (Post Mortem)",
                                   "50" ="Dermatose",
                                   "29" =  "Caquexia",
                                   "21" =  "Ascite",
                                   "109" =  "Septicemia",
                                   "11"=  "Aerossaculite",
                                   "111"=  "Sindrome Ascitica",
                                   "106"=  "Salpingite",
                                   "18"=  "Artrite")




library(ggplot2)
doencas10 %>% 
  ggplot(aes(x = Diagnostico_N, y = QUANTIDADE))+
  geom_col(fill = "#112446")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))

#### Quantidade de casos por cidade

Doencas10<- dados %>% select(Municipio_N,UF,Diagnostico_N)




soma<- c()  
for (i in unique(dados$Municipio_N)) {
  soma[i] <- sum(dados$QUANTIDADE[dados$Municipio_N == i])
}
Doencas10$soma <- soma[dados$Municipio_N]
Doencas10<- distinct(Doencas10, Municipio_N, .keep_all = TRUE)


### recode ####
Doencas10$Municipio_N<-Doencas10$Municipio_N %>% recode(
  "1" = "Porto Velho",
  "2" = "Guajará-Mirim",
  "3" = "Vilhena",
  "4" = "São Francisco do Guaporé",
  "5" = "Nova Mamoré",
  "6" = "Machadinho d'Oeste",
  "7" = "São Miguel do Guaporé",
  "8" = "Alta Floresta d'Oeste",
  "9" = "Ji-Paraná",
  "10" = "Candeias do Jamari",
  "11" = "Pimenta Bueno",
  "12" = "Pimenteiras do Oeste",
  "13" = "Chupinguaia",
  "14" = "Governador Jorge Teixeira",
  "15" = "Costa Marques",
  "16" = "Espigão d'Oeste",
  "17" = "Ariquemes",
  "18" = "Itapuã do Oeste",
  "19" = "Alto Alegre dos Parecis",
  "20" = "Cujubim",
  "21" = "Cacoal",
  "22" = "Seringueiras",
  "23" = "Campo Novo de Rondônia",
  "24" = "Buritis",
  "25" = "Vale do Anari",
  "26" = "Corumbiara",
  "27" = "Alvorada d'Oeste",
  "28" = "Jaru",
  "29" = "Cerejeiras",
  "30" = "Alto Paraíso",
  "31" = "Parecis",
  "32" = "Theobroma",
  "33" = "Ouro Preto do Oeste",
  "34" = "Cacaulândia",
  "35" = "Monte Negro",
  "36" = "Presidente Médici",
  "37" = "Rio Crespo",
  "38" = "Nova Brasilândia d'Oeste",
  "39" = "Rolim de Moura",
  "40" = "Colorado do Oeste",
  "41" = "Cabixi",
  "42" = "Santa Luzia d'Oeste",
  "43" = "Mirante da Serra",
  "44" = "Vale do Paraíso",
  "45" = "Castanheiras",
  "46" = "Novo Horizonte do Oeste",
  "47" = "Urupá",
  "48" = "Nova União",
  "49" = "Ministro Andreazza",
  "50" = "Primavera de Rondônia",
  "51" = "São Felipe d'Oeste",
  "52" = "Teixeirópolis")

Doencas10<-Doencas10 %>% 
  filter(Municipio_N == "Espigão d'Oeste" | 
           Municipio_N == "Cacoal" | 
           Municipio_N =="Rolim de Moura" | 
           Municipio_N =="Pimenta Bueno" | 
           Municipio_N == "São Felipe d'Oeste" | 
           Municipio_N == "Ministro Andreazza"| 
           Municipio_N == "Presidente Médici" | 
           Municipio_N == "Chupinguaia"| 
           Municipio_N == "Porto Velho" | 
           Municipio_N == "Corumbiara" )



library(ggplot2)
Doencas10 %>% 
  ggplot(aes(x = Municipio_N, y = soma))+
  geom_col(fill = "#112446")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))
