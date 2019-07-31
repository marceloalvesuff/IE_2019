# Escola de Inverno do IESP
# Mineração de dados das mídias sociais
# Marcelo Alves 

# Aula 3



# Pacotes -----------------------------------------------------------------
library(pacman)

p_load("rtweet")
p_load("tidyverse")
p_load("RSQLite") # pacote banco de dados
p_load("gmailr") # disparo automatico de email
p_load("leaflet")


# Tarefa em dupla ---------------------------------------------------------

# Defina um tema de pesquisa
# Elabore uma lista de consultas
# Realize a extração e armazene em um banco de dados
# 


# Extracao ---------------------------------------------------------------
db <- dbConnect(SQLite(), dbname="dados_rtweet.sqlite") # banco de dados

q <- c("bolsonaro OR @jairbolsonaro", "#QuartaFeiraSemBolsonaro")

c <- 0 # contador

for (x in q){
  
  # extracao
  print(paste("Coletando dados com a query", x, "em", Sys.time())) 
  
  tweets <- search_tweets(x, n = 5000,
                          type = "recent", 
                          include_rts = TRUE,
                          parse = TRUE, 
                          retryonratelimit = T,
                          lang = "pt")
  
  ### preparacao das listas para db
  
  
  tmp <- sapply(tweets, class)
  tmp <- grep("list", tmp)
  
  # selecionar vetores
  
  data <-   tweets %>%
    lat_lng() %>%
    select(-tmp) %>%
    add_column(query = x, 
               q_total =  paste0(x)) %>%
    mutate(mes =  format(created_at,format="%B"),
           mes_dia = as.Date(paste0(format(created_at,format="%d-%m")),"%y-%m-%d"))
  
  # selecionar listas
  data_list <- tweets %>%
    select(c(2, tmp))  %>%
    unnest(urls_expanded_url, .drop = F) %>%
    unnest(hashtags, .drop = F) %>%
    unnest(mentions_screen_name, .drop = T)
  
  
  # armazenar
  dbWriteTable(conn = db, name = "full_data", value = data, row.names = FALSE, append = T)
  dbWriteTable(conn = db, name = "tweets_meta", value = data_list, row.names = FALSE, append = T)
  
  # contar
  
  l <- length(tweets$status_id)
  c <- c +l 
  print(paste("Total momentaneo em", c))
  
  
} 

# disparar email



text_msg<-  mime() %>%
  to("marceloalves.ufsj@hotmail.com") %>%
  from("malvesjor@gmail.com") %>%
  subject("Registro de coleta de dados Twitter") %>%
  text_body(paste("Coleta finalizada em", Sys.time(), "com retorno aproximado de", c)) 


send_message(text_msg)



# Analise ---------------------------------------------------------------
rm(data, data_list, tweets, c, l)

# importar dados do banco
tweets <- dbReadTable(conn = db, name = "full_data")
meta <- dbReadTable(conn = db, name = "tweets_meta")

# eliminar duplicatas
tweets <- subset(tweets, !duplicated(status_id))


meta <- meta %>% 
  distinct(status_id, hashtags, urls_expanded_url, mentions_screen_name, .keep_all = TRUE)

# Controle de datas

tweets <- tweets %>%
  mutate(data=as.POSIXct(created_at, origin='1970-01-01'),
         dia = as.Date(data, "%d-%m-%y"), 
         mes_dia = format(data,format="%y-%m"), 
         mes_dia = as.Date(paste0(mes_dia, "-01"),"%y-%m-%d"))

tweets$dia_hora <- as.POSIXct(trunc(tweets$data, "hours"))
### Qual n de cada q coletado?

### Gere a serie temporal de tweets por dia

### Quais usuários mais engajados na produção de tweets?

### Quais perfis tiveram mais favoritos nos tweets?

#### Quais perfis foram mais retuitados?

### Gere um gráfico com as 10 hashtags mais utilizadas

# Quais contas foram mencionadas mais vezes?

#### quais principais urls?

#### quais principais cidades pelos metadados?


# Menções as consultas por dia


tweets %>%
  group_by(dia_hora, query) %>%
  summarise(n = n()) %>%
  mutate(query = str_to_title(query)) %>%
  ggplot(aes(x=dia_hora, y=n, group=query)) +
  geom_line(aes(color=query), size = .7)+
  labs(title = "Serie temporal das consultas",
       subtitle = "Grafico de linhas por dia",
       x = "", y = "", 
       caption = "Fonte: Twitter Search API") +
  facet_wrap(~ query) 



# gerar mapa


tweet_locations <- tweets %>%
  filter(!is.na(lat))

# plot points on top of a leaflet basemap

leaflet(tweet_locations) %>%
  addTiles() %>%
  addCircleMarkers(lng = ~lng, lat = ~lat, popup = ~paste0("Perfil: ", screen_name, "\tTweet: \t", text),
                   radius = 5, stroke = FALSE, color = "#ac3973")



# Atividade 2 -------------------------------------------------------------


