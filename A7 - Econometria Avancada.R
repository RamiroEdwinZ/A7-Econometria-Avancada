 # Econometris Avancada A7 - Critérios de Informação

library(readxl)                                                        #Carregando os pacotes
variacao_PIB <- read.table("c:/Econometria/Variacao.xls", header = T)  #Carregando os arquivos
var_PIB <- ts(variacao_PIB$variacao_PIB, start =1951, frequency = 1 )  #Mostrando os dados a serie temporal var_PIB
View(var_PIB)
plot(var_PIB, main="Variacao do PIB Brasileiro", col="Blue", ylab="%PIB", xlab="Ano")

acf(var_PIB)         #Cria a FAC -FunÃ§Ã£o de AutocorrelaÃ§Ã£o (ACF)
pacf(var_PIB)        #cria a FACP - FunÃ§ao de AutocorrelaÃ§Ã£o Parcial (PACF)

AR1 <- arima(var_PIB, order = c(1,0,0))   #Estima um modelo autoreressivo de ordem p=1 ,AR(1)
MA1 <- arima(var_PIB, order = c(0,0,1))   #Estima um modelo de mÃ©dias mÃ³veis ordem q=1 , MA(1)
ARMA11 <- arima(var_PIB, order = c(1,0,1))#Estima um modelo autoregressivo de mÃ©dias mÃ³veis ordem p=1 e q=1 ARMA(1,1)

AIC(AR1) #Extrai a estatistica AIC do modelo AR1
BIC(AR1) #Extrai a estatistica BIC Ddo modelo AR1


AR2 <- arima(var_PIB, order = c(2,0,0))

MA2 <- arima(var_PIB, order = c(0,0,2))
MA3 <- arima(var_PIB, order = c(0,0,3))
MA4 <- arima(var_PIB, order = c(0,0,4))
MA5 <- arima(var_PIB, order = c(0,0,5))
MA6 <- arima(var_PIB, order = c(0,0,6))
MA7 <- arima(var_PIB, order = c(0,0,7))
MA8 <- arima(var_PIB, order = c(0,0,8))
MA9 <- arima(var_PIB, order = c(0,0,9))

ARMA12 <- arima(var_PIB, order = c(1,0,2))
ARMA13 <- arima(var_PIB, order = c(1,0,3))
ARMA14 <- arima(var_PIB, order = c(1,0,4))
ARMA15 <- arima(var_PIB, order = c(1,0,5))
ARMA16 <- arima(var_PIB, order = c(1,0,6))
ARMA17 <- arima(var_PIB, order = c(1,0,7))
ARMA18 <- arima(var_PIB, order = c(1,0,8))
ARMA19 <- arima(var_PIB, order = c(1,0,9))

ARMA21 <- arima(var_PIB, order = c(2,0,1))
ARMA22 <- arima(var_PIB, order = c(2,0,2))
ARMA23 <- arima(var_PIB, order = c(2,0,3))
ARMA24 <- arima(var_PIB, order = c(2,0,4))
ARMA25 <- arima(var_PIB, order = c(2,0,5))
ARMA26 <- arima(var_PIB, order = c(2,0,6))
ARMA27 <- arima(var_PIB, order = c(2,0,7))
ARMA28 <- arima(var_PIB, order = c(2,0,8))
ARMA29 <- arima(var_PIB, order = c(2,0,9))

#Exemplo aplicação mulltipla - Extra (Deve-se completar as estimações antes de executar esse código) #COMPLETADO

estimacoes <- list(AR1, AR2, MA1, MA2, MA3, MA4, MA5, MA6, MA7, MA8, MA9, 
                   ARMA11,ARMA12, ARMA13, ARMA14,ARMA15, ARMA16,ARMA17,ARMA18,ARMA19,
                   ARMA21,ARMA22,ARMA23,ARMA24,ARMA25,ARMA26,ARMA27,ARMA28,ARMA29)      #Cria uma lista com os estimadores
sapply(estimacoes, AIC)                 #Aplica o comando AIC na lista
sapply(estimacoes, BIC)                 #Aplica o comando BIC na lista

#Exemplo de criação de tabela com resultados - Extra

AIC <- sapply(estimacoes, AIC) 
BIC <- sapply(estimacoes, BIC)
Modelo <- c("AR1", "AR2", "MA1", "MA2", "MA3", "MA4", "MA5", "MA6", "MA7", "MA8", "MA9", "ARMA11","ARMA12", "ARMA13", "ARMA14","ARMA15", "ARMA16","ARMA17","ARMA18","ARMA19","ARMA21","ARMA22","ARMA23","ARMA24","ARMA25","ARMA26","ARMA27","ARMA28","ARMA29")

Resultados <- data.frame(Modelo, AIC, BIC)
View(Resultados)

ARMA29

#Salvar CNVAZQUEZ