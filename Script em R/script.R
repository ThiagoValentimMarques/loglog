#########################################################
#------------------ URL dos dados ----------------------#
#########################################################

# Carregar o pacote tidyverse

require(tidyverse)
require(lubridate)

# Casos confirmados
url1 <- "https://data.humdata.org/hxlproxy/api/data-preview.csv?url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_confirmed_global.csv&filename=time_series_covid19_confirmed_global.csv" 

# Óbitos 
url2 <- "https://data.humdata.org/hxlproxy/api/data-preview.csv?url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_deaths_global.csv&filename=time_series_covid19_deaths_global.csv"

#########################################################
#-------------- Preparação dos dados -------------------#
#########################################################

casos <- read.csv(url1,header=TRUE)
obitos <- read.csv(url2,header=TRUE)

casos$Country.Region <- as.character(casos$Country.Region)
obitos$Country.Region <- as.character(obitos$Country.Region)

casos$Country.Region[108]<-"French Guiana"
obitos$Country.Region[108]<-"French Guiana"

# Países que desejo fazer a análise
paises <- c("Brazil","India","Germany","Italy","Spain","Iran",
            "Peru","Mexico","US")

# Nomemclatura que serão exibidas nas análises
sel <- c("Brasil","Índia","Alemanha","Itália","Espanha","Irã",
         "Peru","México","EUA")

##############################################################################
################ Início da rotina para os casos ##############################
##############################################################################

casos <- casos %>%
  filter(Country.Region %in% paises)

n<-dim(casos[,-c(1,2,3,4)])[2]

matriz<-matrix("NA",ncol=length(paises),nrow=n)
matriz2<-matrix("NA",ncol=length(paises),nrow=n)
matriz3<-matrix("NA",ncol=length(paises),nrow=n)
j<-0
for(i in paises){
  N <- length(as.vector(apply(casos[casos$Country.Region=="Brazil",-c(1,2,3,4)],2,sum)))
  valor <- as.vector(apply(casos[casos$Country.Region==i,-c(1,2,3,4)],2,sum))
  cem <- valor[valor>=100] 
  cem[(length(cem)+1):N] <- rep(NA_character_,(N-length(cem)))
  cem <- as.numeric(cem)
  last_point <- rep(NA_character_,N)
  last_point[length(na.omit(cem))]<-sel[j+1]
  
  diario<-NULL
  diario[1]<-cem[1]
  for(k in 2:length(cem)){
    diario[k] <- cem[k]-cem[k-1]
  }
  
  j<-j+1
  matriz[,j]<-cem
  matriz2[,j]<-last_point
  matriz3[,j]<-diario
  
} 

point<-as.vector(matriz2)
casos <- as.vector(as.numeric(matriz))
diario <- as.vector(as.numeric(matriz3))
País <- rep(sel,each=n)
dia <- rep(1:dim(matriz)[1],length(paises))
corona <- data.frame(dia,País,casos,point,diario)
corona <- as.tibble(corona)

##############################################################################
################# Final da rotina para os casos ##############################
##############################################################################

##############################################################################
################ Início da rotina para os óbitos #############################
##############################################################################

obitos <- obitos %>%
  filter(Country.Region %in% paises)

n<-dim(obitos[,-c(1,2,3,4)])[2]

matriz<-matrix("NA",ncol=length(paises),nrow=n)
matriz2<-matrix("NA",ncol=length(paises),nrow=n)
matriz3<-matrix("NA",ncol=length(paises),nrow=n)
j<-0
for(i in paises){
  N <- length(as.vector(apply(obitos[obitos$Country.Region=="Brazil",-c(1,2,3,4)],2,sum)))
  valor <- as.vector(apply(obitos[obitos$Country.Region==i,-c(1,2,3,4)],2,sum))
  dez <- valor[valor>=10] 
  dez[(length(dez)+1):N] <- rep(NA_character_,(N-length(dez)))
  dez <- as.numeric(dez)
  last_point <- rep(NA_character_,N)
  last_point[length(na.omit(dez))]<-sel[j+1]
  
  diario<-NULL
  diario[1]<-dez[1]
  for(k in 2:length(dez)){
    diario[k] <- dez[k]-dez[k-1]
  }
  
  j<-j+1
  matriz[,j]<-dez
  matriz2[,j]<-last_point
  matriz3[,j]<-diario
  
} 

point<-as.vector(matriz2)
obitos <- as.vector(as.numeric(matriz))
diario <- as.vector(as.numeric(matriz3))
País <- rep(sel,each=n)
dia <- rep(1:dim(matriz)[1],length(paises))
cor_obt <- data.frame(dia,País,obitos,point,diario)
cor_obt <- as.tibble(cor_obt)

##############################################################################
################  Final da rotina para os óbitos  ############################
##############################################################################

#### Gráfico 1

ggplot(corona,aes(x=log10(casos),y=log10(diario),group=País,
                  colour=País))+geom_point()+
  facet_wrap(~País)+
  labs(x = "Total de casos (escala log10)", y = "Casos novos (escala log10)", colour = "País",
       title="Figura 1: Gráfico log-log dos casos com Covid-19",
       caption="Fonte: Ministério da Saúde        Autor: Thiago Valentim") +
  theme(legend.position="none", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=0,hjust=0.5),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))

#### Gráfico 2

ggplot(cor_obt,aes(x=log10(obitos),y=log10(diario),group=País,
                   colour=País))+geom_point()+
  facet_wrap(~País)+
  labs(x = "Total de óbitos (escala log10)", y = "Óbitos novos (escala log10)", colour = "País",
       title="Figura 2: Gráfico log-log dos óbitos por Covid-19",
       caption="Fonte: Ministério da Saúde        Autor: Thiago Valentim") +
  theme(legend.position="none", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=0,hjust=0.5),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))


#Carregar os dados do ministério da saúde

caminho<-"C:\\Users\\Thiago\\Documents\\Git\\meublog\\static\\datasets\\COVIDBR_07jul2020.csv"

dados<-read.csv(caminho,header=TRUE,sep=";")

dados<-as.tibble(dados) #visualização melhor do conjunto de dados

# Gráfico 3

analise <- dados%>%
  filter(regiao != "Brasil",is.na(codmun)==TRUE)%>%
  mutate(data=dmy(data))%>%
  mutate(semanaEpi=as.factor(semanaEpi))

analise%>%
  filter(regiao=="Nordeste")%>%
  ggplot(.,aes(x=log10(casosAcumulado),y=log10(casosNovos),group=estado,
               colour=estado))+geom_point()+
  facet_wrap(~estado)+
  labs(x = "Total de casos (escala log10)", y = "Casos novos (escala log10)", colour = "Estado",
       title="Figura 3: Gráfico log-log para os casos de Covid-19 na Região Nordeste",
       caption="Fonte: Ministério da Saúde        Autor: Thiago Valentim") +
  theme(legend.position="none", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=0,hjust=0.5),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))

# Gráfico 4

analise%>%
  filter(regiao=="Nordeste")%>%
  ggplot(.,aes(x=log10(obitosAcumulado),y=log10(obitosNovos),group=estado,
               colour=estado))+geom_point()+
  facet_wrap(~estado)+
  labs(x = "Total de óbitos (escala log10)", y = "Óbitos novos (escala log10)", colour = "Estado",
       title="Figura 4: Gráfico log-log para os óbitos por Covid-19 na Região Nordeste",
       caption="Fonte: Ministério da Saúde        Autor: Thiago Valentim") +
  theme(legend.position="none", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=0,hjust=0.5),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))
