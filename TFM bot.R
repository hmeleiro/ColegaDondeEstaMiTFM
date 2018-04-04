library(rtweet)

setwd("Ruta-a-la-carpeta-donde-esté-df_def.csv") # Añadir la ruta al csv

tweets <- read.csv("df_def.csv")
tweets$tweet <- as.character(tweets$tweet)


appname <- # Añadir nombre de la app de Twitter

key <- # Añadir credenciales

secret <- # Añadir credenciales


twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret)


Sys.sleep(240)
##  Primer tuit del día con un contador de los dias que lleva Cristina Cifuentes sin enseñar su TFM
start <- Sys.time()

inicio <- as.Date("2018-03-21")  # Fecha publicación exclusiva elDiario.es
dias <- Sys.Date() - inicio # Saco la diferencia respecto al dia de hoy
dias <- as.numeric(as.difftime(dias, units = "days"))  #Me aseguro de que la diferencia se establece en días
contador <- paste("Han pasado", dias, "días y Cristina Cifuentes aún no ha encontrado su Trabajo de Fin de Master.")  # Compongo el tweet

post_tweet(status = contador, token = twitter_token)  #  Lanzo el tweet


## Duerme durante 4 minutos (240)
Sys.sleep(240)


## Entra en el bucle durante 14 horas (50.400 segundos) tuiteando un TFM cada hora (3.600 segundos)
repeat {
  n <- sample(1:length(tweets$tweet), 1)

  post_tweet(status = tweets$tweet[n], token = twitter_token)
  
  Sys.sleep(3600)
  
  stop <- Sys.time() 
  if (difftime(stop, start, units = "secs") >= 50400) {
    break
  }
}
