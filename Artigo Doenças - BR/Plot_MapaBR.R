#Bibliotecas
library(readr)
library(tidyverse)
library(stringdist)
library(readxl)
library(writexl)
library(esquisse)

# Selecionar os top 3 diagnósticos
df_doenças<- dados %>% 
  filter(Diagnosticos_N == 134  |
           Diagnosticos_N == 388 |
           Diagnosticos_N ==  326)

table(df_doenças$Diagnosticos_N)

df_doenças$Diagnosticos_N <- recode (df_doenças$Diagnosticos_N,
                                  "134" ="Contaminação",
                                  "388" ="Prevenção de E.E.B.",
                                  "326" ="Nefrite")

#Separação Contaminação por UF

df_conta <-df_doenças %>% 
  filter(Diagnosticos_N == "Contaminação") %>% 
  select(UF_ESTABELECIMENTO,DIAGNOSTICO,QUANTIDADE,Diagnosticos_N) %>% 
  rename("uf" = UF_ESTABELECIMENTO)


soma<- c()  
for (i in unique(df_conta$uf)) {
  soma[i] <- sum(df_conta$QUANTIDADE[df_conta$uf == i])
}
df_conta$soma <- soma[df_conta$uf]
df_conta<-df_conta %>% distinct( uf, .keep_all = TRUE)
df_conta<-df_conta %>%  na.omit()
#Separação PREVENÇÃO DE E.E.B. por UF

df_Prev <-df_doenças %>% 
  filter(Diagnosticos_N == "Prevenção de E.E.B.") %>% 
  select(UF_ESTABELECIMENTO,DIAGNOSTICO,QUANTIDADE,Diagnosticos_N) %>% 
  rename("uf" = UF_ESTABELECIMENTO)


soma<- c()  
for (i in unique(df_Prev$uf)) {
  soma[i] <- sum(df_Prev$QUANTIDADE[df_Prev$uf == i])
}
df_Prev$soma <- soma[df_Prev$uf]
df_Prev<-df_Prev %>% distinct( uf, .keep_all = TRUE)
df_Prev<-df_Prev %>%  na.omit()

#Separação Nefrite. por UF

df_Nefrite <-df_doenças %>% 
  filter(Diagnosticos_N == "Nefrite") %>% 
  select(UF_ESTABELECIMENTO,DIAGNOSTICO,QUANTIDADE,Diagnosticos_N) %>% 
  rename("uf" = UF_ESTABELECIMENTO)


soma<- c()  
for (i in unique(df_Nefrite$uf)) {
  soma[i] <- sum(df_Nefrite$QUANTIDADE[df_Nefrite$uf == i])
}
df_Nefrite$soma <- soma[df_Nefrite$uf]
df_Nefrite<-df_Nefrite %>% distinct( uf, .keep_all = TRUE)
df_Nefrite<-df_Nefrite %>%  na.omit()

# dataset long e lat


BR <-read.csv("C:/Users/carlo/Documents/Projetos/Artigo Doenças RO/Tratamento R/Artigo Doenças - RO/Banco de dados/geoinfo-master/latitude-longitude-cidades.csv",sep = ";")

#vetor para filtrar as capitais
capitais <- c("Rio Branco", "Maceió", "Macapá", "Manaus", "Salvador", "Fortaleza", 
              "Brasília", "Vitória", "Goiânia", "São Luís", "Cuiabá", "Campo Grande", 
              "Belo Horizonte", "Belém", "João Pessoa", "Curitiba", "Recife", 
              "Teresina", "Rio de Janeiro", "Natal", "Porto Alegre", "Porto Velho", 
              "Boa Vista", "Florianópolis", "São Paulo", "Aracaju", "Palmas")
# Filtra capitais
BR <-BR %>% 
  filter(municipio %in% capitais) %>% 
  distinct( uf, .keep_all = TRUE) %>% 
  select(uf,longitude,latitude)




BR_conta<- full_join(BR,df_conta,by = "uf")
BR_conta$soma <- as.double( BR_conta$soma)
BR_conta$somaChar <- as.character(BR_conta$soma) 
BR_conta<-BR_conta %>%  na.omit()


BR_Prev<- full_join(BR,df_Prev,by = "uf")
BR_Prev$soma <- as.double( BR_Prev$soma)
BR_Prev$somaChar <- as.character(BR_Prev$soma) 
BR_Prev<-BR_Prev %>%  na.omit()


BR_Nefrite<- full_join(BR,df_Nefrite,by = "uf")
BR_Nefrite$soma <- as.double( BR_Nefrite$soma)
BR_Nefrite$somaChar <- as.character(BR_Nefrite$soma) 
BR_Nefrite<-BR_Nefrite %>%  na.omit()

## Mapa das 3 diagnósticos mais incidentes 
library(leaflet)
leaflet(BR_conta) %>%
  addTiles() %>%
  addCircles(lng = ~longitude, 
             lat = ~latitude, 
             weight = 1,
             popup =BR_conta$uf, BR_conta$somaChar,
             radius = BR_conta$soma/2000,color = "red" )

library(leaflet)
leaflet(BR_Prev) %>%
  addTiles() %>%
  addCircles(lng = ~longitude, 
             lat = ~latitude, 
             weight = 1,
             popup = BR_Prev$uf,BR_Prev$somaChar,
             radius = BR_Prev$soma/2000, color = "red")

library(leaflet)
leaflet(BR_Nefrite) %>%
  addTiles() %>%
  addCircles(lng = ~longitude, 
             lat = ~latitude, 
             weight = 1,
             popup = BR_Nefrite$uf,BR_Nefrite$somaChar,
             radius = BR_Nefrite$soma/2000,color = "red")



##esquisser(Contaminação por cidade)

library(ggplot2)
ggplot(df_conta) +
  aes(x =uf , y = soma, fill = soma) +
  geom_col() +
  scale_fill_gradient(low = "#3999F9", high = "#072F4D") +
  theme_minimal()


library(ggplot2)
ggplot(df_Prev) +
  aes(x =uf , y = soma, fill = soma) +
  geom_col() +
  scale_fill_gradient(low = "#3999F9", high = "#072F4D") +
  theme_minimal()



library(ggplot2)
ggplot(df_Nefrite) +
  aes(x =uf , y = soma, fill = soma) +
  geom_col() +
  scale_fill_gradient(low = "#3999F9", high = "#072F4D") +
  theme_minimal()
