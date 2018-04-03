library(httr)
library(rvest)
library(stringr)

start <- Sys.time()


## URJC

url <- "https://eciencia.urjc.es/handle/10115/13043/recent-submissions?offset="

paginas <- seq(0,79,20)
urls <- paste0(url, paginas)

urls <- c(urls, "https://eciencia.urjc.es/handle/10115/12993", "https://eciencia.urjc.es/handle/10115/12989")


desktop_agents <-  c('Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.99 Safari/537.36',
                     'Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.99 Safari/537.36',
                     'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.99 Safari/537.36',
                     'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_1) AppleWebKit/602.2.14 (KHTML, like Gecko) Version/10.0.1 Safari/602.2.14',
                     'Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.71 Safari/537.36',
                     'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.98 Safari/537.36',
                     'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.98 Safari/537.36',
                     'Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.71 Safari/537.36',
                     'Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.99 Safari/537.36',
                     'Mozilla/5.0 (Windows NT 10.0; WOW64; rv:50.0) Gecko/20100101 Firefox/50.0')

links_tot <- NA


for (p in 1:length(urls)) {
  
  x <- GET(urls[p], add_headers('user-agent' = desktop_agents[sample(1:10, 1)]))
  
  links <- x %>% read_html() %>% html_nodes(".artifact-title a") %>% html_attr(name = "href")
  links <- paste0("https://eciencia.urjc.es",links)
  
  links_tot <- c(links_tot, links)
  print(links)

}

links_tot <- links_tot[-1]



df <- data.frame(autor = NA, titulo = NA, depositado = NA, links = NA)

p <- 1

for (p in 1:length(links_tot)) {
  x <- GET(links_tot[p], add_headers('user-agent' = desktop_agents[sample(1:10, 1)]))
  
  autor <- x %>% read_html() %>% html_nodes(".simple-item-view-authors") %>% html_text()
  titulo <- x %>% read_html() %>% html_nodes(".page-header") %>% html_text()
  depositado <- x %>% read_html() %>% html_nodes(".simple-item-view-date") %>% html_text()
  link <- links_tot[p]
  
  autor <- str_split(autor, ";", simplify = TRUE)

  if (length(autor) > 1) {
    autor <- autor[1]
  }
  
  line <- data.frame("autor" = autor, "titulo" = titulo, "depositado" = depositado, "links" = links_tot[p])
  df <- rbind(df, line)
  print(line)
}

df <- df[-1,]

###LIMPIEZA  


df$depositado <- str_replace(df$depositado, "\nDate:\n", "")
df$depositado <- substr(str_trim(df$depositado), 1, 4)
df$depositado <- as.Date.character(df$depositado, format = "%Y")
df$depositado <- format(df$depositado, format = "%Y")

df$apellidos <- str_split(df$autor, pattern = ", ", simplify = TRUE, n = 2)[,1]
df$apellidos <- str_split(df$apellidos, " ", simplify = TRUE)[,1]

df$nombre <- str_split(df$autor, pattern = ",", simplify = TRUE, n = 2)[,2]
df$nombre_completo <-  paste(df$nombre, df$apellidos)

año <- df$depositado

df$tweet <- paste0("En ", año, ", ", df$nombre_completo, " entregó su TFM titulado ", "'", df$titulo, "'.", " Cristina Cifuentes sigue sin encontrar el suyo.", " ", df$links)
df$long <- df$long <- nchar(df$tweet) - nchar(as.character(df$links)) + 23
df_def <- subset(df, long <= 280)

df_def_urjc <- df_def[duplicated(df_def) == FALSE,]





### UCM

url <- "http://eprints.ucm.es/cgi/search/archive/advanced?exp=0%7C1%7C-date%2Fcreators_name%2Ftitle%7Carchive%7C-%7Ctype%3Atype%3AANY%3AEQ%3Amaster%7C-%7Ceprint_status%3Aeprint_status%3AANY%3AEQ%3Aarchive%7Cmetadata_visibility%3Ametadata_visibility%3AANY%3AEQ%3Ashow&_action_search=1&order=-date%2Fcreators_name%2Ftitle&screen=Search&cache=50297039&search_offset="


paginas <- seq(0,1288,20)

urls <- paste0(url, paginas)

df <- data.frame(autor = NA, titulo = NA, depositado = NA, links = NA)

desktop_agents <-  c('Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.99 Safari/537.36',
                     'Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.99 Safari/537.36',
                     'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.99 Safari/537.36',
                     'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_1) AppleWebKit/602.2.14 (KHTML, like Gecko) Version/10.0.1 Safari/602.2.14',
                     'Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.71 Safari/537.36',
                     'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.98 Safari/537.36',
                     'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.98 Safari/537.36',
                     'Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.71 Safari/537.36',
                     'Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.99 Safari/537.36',
                     'Mozilla/5.0 (Windows NT 10.0; WOW64; rv:50.0) Gecko/20100101 Firefox/50.0')

links_tot <- NA

for (p in 1:length(urls)) {
  
  x <- GET(urls[p], add_headers('user-agent' = desktop_agents[sample(1:10, 1)]))
  
  links <- x %>% read_html() %>% html_nodes(".ep_search_result a") %>% html_attr(name = "href")
  links <- links[str_detect(string = links, pattern = "http://eprints.ucm.es/") == TRUE]
  links <- links[str_detect(string = links, pattern = ".pdf") == FALSE]
  links_tot <- c(links, links_tot)
  print(links_tot)
}

links_tot <- links_tot[!is.na(links_tot) == TRUE]


for (n in 1:length(links_tot)) {
  url <- links_tot[n]
  x <- GET(url, add_headers('user-agent' = desktop_agents[sample(1:10, 1)]))
  
  autor <- x %>% read_html() %>% html_nodes(".ep_block > .person_name") %>% html_text()
  titulo <- x %>% read_html() %>% html_nodes(".ep_block > em") %>% html_text()
  depositado <- x %>% read_html() %>% html_nodes("td.ep_row") %>% html_text()
  depositado <- depositado[length(depositado)-1]
  
  line <- data.frame("autor" = autor, "titulo" = titulo, "depositado" = depositado, "links" = links_tot[n])
  df <- rbind(df, line)
  print(line)
  
}

print(line)
df <- rbind(df,line)


df <- df[-1,]
end <- Sys.time()
print(end - start)




#### LIMPIEZA


df$depositado <- str_replace(string = df$depositado, pattern = " Ene ", replacement = "-01-")
df$depositado <- str_replace(string = df$depositado, pattern = " Feb ", replacement = "-02-")
df$depositado <- str_replace(string = df$depositado, pattern = " Mar ", replacement = "-03-")
df$depositado <- str_replace(string = df$depositado, pattern = " Abr ", replacement = "-04-")
df$depositado <- str_replace(string = df$depositado, pattern = " May ", replacement = "-05-")
df$depositado <- str_replace(string = df$depositado, pattern = " Jun ", replacement = "-06-")
df$depositado <- str_replace(string = df$depositado, pattern = " Jul ", replacement = "-07-")
df$depositado <- str_replace(string = df$depositado, pattern = " Aug ", replacement = "-08-")
df$depositado <- str_replace(string = df$depositado, pattern = " Sep ", replacement = "-09-")
df$depositado <- str_replace(string = df$depositado, pattern = " Oct ", replacement = "-10-")
df$depositado <- str_replace(string = df$depositado, pattern = " Nov ", replacement = "-11-")
df$depositado <- str_replace(string = df$depositado, pattern = " Dic ", replacement = "-01-")

df$fecha <- substr(df$depositado, 1, 10)
df$fecha <- as.Date(x = df$fecha, format = "%d-%m-%Y")


df$apellidos <- str_split(df$autor, pattern = ", ", simplify = TRUE, n = 2)[,1]
df$apellidos <- str_split(df$apellidos, " ", simplify = TRUE)[,1]
df$nombre <- str_split(df$autor, pattern = ", ", simplify = TRUE, n = 2)[,2]

df$nombre_completo <-  paste(df$nombre, df$apellidos)

dia <- format.Date(df$fecha, format = "%d")
mes <- format.Date(df$fecha, format = "%B")
año <- format.Date(df$fecha, format = "%Y")


df$tweet <- paste0("El ", dia, " de ", mes, " de ", año, ", ", df$nombre_completo, " entregó su TFM titulado ", "'", df$titulo, "'.", " Cristina Cifuentes sigue sin encontrar el suyo.", " ", df$links)
df$long <- df$long <- nchar(df$tweet) - nchar(as.character(df$links)) + 23
df_def <- subset(df, long <= 280)

df_def_UCM <- df_def[-1,]



####   UC3M


url <- "https://e-archivo.uc3m.es/handle/10016/17686/browse?order=ASC&rpp=300&sort_by=2&etal=-1&offset=0&type=dateissued"


x <- GET(url, add_headers('user-agent' = desktop_agents[sample(1:10, 1)]))

links <- x %>% read_html() %>% html_nodes(".artifact-title a") %>% html_attr(name = "href")
links <- paste0("https://e-archivo.uc3m.es", links)
titulos <- x %>% read_html() %>% html_nodes(".artifact-title") %>% html_text()
fecha <- x %>% read_html() %>% html_nodes(".publisher-date") %>% html_text()
autores <- x %>% read_html() %>% html_nodes(".artifact-info") %>% html_text(trim = TRUE)

odd <- seq(from = 1, to = 228, by = 2)
autores <- autores[-odd]
autores <- str_split(autores, "\n \n", simplify = TRUE)[,1]

titulos <- str_remove_all(titulos, "\\(masterThesis\\)")

df <- data.frame("autor" = autores, "titulo" = titulos, "depositado" = fecha, "links" = links)


df$autor[str_detect(df$autor, ";") == TRUE] <- str_split(df$autor, ";")[1]

df$apellidos <- str_split(df$autor, pattern = ", ", simplify = TRUE, n = 2)[,1]
df$nombre <- str_split(df$autor, pattern = ", ", simplify = TRUE, n = 2)[,2]
df$apellidos <- str_split(df$apellidos, " ", simplify = TRUE)[,1]



df$nombre_completo <-  paste(df$nombre, df$apellidos)




df$depositado <- substr(str_trim(df$depositado), 1, 4)
df$depositado <- as.Date.character(df$depositado, format = "%Y")
df$depositado <- format(df$depositado, format = "%Y")

año <- df$depositado

df$tweet <- paste0("En ", año, ", ", df$nombre_completo, " entregó su TFM titulado ", "'", df$titulo, "'.", " Cristina Cifuentes sigue sin encontrar el suyo.", " ", df$links)
df$long <- nchar(df$tweet) - nchar(as.character(df$links)) + 23



df_def_uc3m <- subset(df, long <= 280)




##  Fusion

df_def_urjc$fecha <- NA
df_def_urjc <- subset(df_def_urjc, select = c(1,2,3,4,10,5,6,7,8,9))

df_def_uc3m$fecha <- NA
df_def_uc3m <- subset(df_def_uc3m, select = c(1,2,3,4,10,5,6,7,8,9))


df_definitiva <- rbind(df_def_urjc, df_def_uc3m, df_def_UCM)

write.csv(df_definitiva, file = "/Users/HECTOR/Dropbox/MASTER/DATOS/R/TFMscraper UCM/df_def.csv", row.names = FALSE)



