library(rtweet)

dir <- getwd()
setwd(dir)

tweets <- read.csv("df_def.csv")
tweets$tweet <- as.character(tweets$tweet)


appname <- # Añadir nombre de la app de Twitter

key <- # Añadir credenciales

secret <- # Añadir credenciales


twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret)



##  Primer tuit del día con un contador de los dias que lleva Cristina Cifuentes sin enseñar su TFM
start <- Sys.time()

inicio <- as.Date("2018-03-21")
dias <- Sys.Date() - inicio
dias <- as.numeric(as.difftime(dias, units = "days"))
contador <- paste("Han pasado", dias, "días y Cristina Cifuentes aún no ha encontrado su Trabajo de Fin de Master.")

post_tweet(status = contador, token = twitter_token)


## Duerme durante 4 minutos
Sys.sleep(240)


## Entra en el bucle durante 14 horas tuiteando un TFM cada hora
repeat {
  n <- sample(1:length(tweets$tweet), 1)

  post_tweet(status = tweets$tweet[n], token = twitter_token)
  
  Sys.sleep(3600)
  
  stop <- Sys.time() 
  if (stop - start >= 50400) {
    break
  }
}
