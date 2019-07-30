# --------------------------- Coleta e sistematização de dados na web ---------------------
# --------------------------- IESP --------------------------------------------------------
# --------------------------- Marcelo Alves -----------------------------------------------
# --------------------------- TWITTER -----------------------------------------------


########## instalar pacotes

install.packages("pacman") # este é um gestor de pacotes, vai se encarregar da instalação e carregamento dos demais
library(pacman)

p_load(rtweet) # conexão à API do twitter
p_load(RSQLite) # armazenamento
p_load(tidyr) # tratamento de dados
p_load(tidyverse) # framework para datascience
p_load(ggplot2) # dataviz

# Primeiro, é importante limpar nosso ambiente com a função rm()

rm(list = ls())

## definir diretório de trabalho

# atalho CTRL SHIFT H


############################### Começando com o rtweet

#### vamos conhecer as funções do pacote

help("rtweet")


####### Autorização

# Criar aplicativo em apps.twitter.com

# O website pode ser seu Twitter https://twitter.com/username
# Não esquecer ***Callback URL***: http://127.0.0.1:1410

### autenticar via token


token <- create_token(
  app = "XXXXXXXXXX",
  consumer_key = "XXXXXXXXXXX",
  consumer_secret = "XXXXXXXXXXXXX",
  access_token = "XXXXXXXXXXXXXXXXXXXXXXX", 
  access_secret = "XXXXXXXXXXXXXXXXX")

## gerar chave e salvar
twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret,
  access_token = access_token,
  access_secret = access_secret
)

### fazer primeira busca

search_tweets(
  "#lulalivre", n = 10, include_rts = FALSE
)


# se houve retorno no console, estamos com token validado e prontos para começar a coletar e analisar dados do twitter


# Search tweets é responsável por acessar a busca no passado, vamos ver quais parametros recebe

help("search_tweets")

# vamos fazer uma extração de 100 tweets mencionando uma hashtag

lulalivre <- search_tweets(
  "#lulalivre", n = 100, include_rts = FALSE, retryonratelimit = F
)

# há 88 variáveis no dataset de retorno. Por padrão, os dados estão parseados
# vamos ver essas variáveis

glimpse(lulalivre)


# vamos checar a evolução no tempo
help(ts_plot)


ts_plot(lulalivre, "1 hour") +
 theme_classic()  + labs(title= "Horário do dia com mais tweets", x = "", y = "")



## Criar coluna dia

lulalivre$dia <- strftime(lulalivre$created_at, format="%d")


#### Quais usuários mais engajados na produção de tweets?

# gerar tabela
tuite <- data.frame(table(lulalivre$screen_name)) %>%
  rename(Perfil = Var1,
         N_tweets = Freq) %>%
  arrange(-N_tweets) %>%
  head(20)


ggplot(tuite, aes(reorder(Perfil, N_tweets), N_tweets)) + 
  geom_bar(stat = "identity") + coord_flip()

rm(tuite)

## Quais urls estão circulando?

# percebam que urls_expanded_url é uma lista

class(lulalivre$urls_expanded_url)

url <- lulalivre %>%
  unnest(urls_expanded_url) %>% # unnest transforma colunas-lista em novas linhas
  group_by(urls_expanded_url) %>%
  summarise(n = n()) %>%
  na.omit()  %>% # não quero os casos sem url
  arrange(-n) %>%
  head(20)

print(url)


### quais perfis tiveram mais favoritos nos tweets?

lulalivre %>%
  group_by(screen_name) %>%
  summarise(fav_total = sum(favorite_count))  %>%
  na.omit()  %>% # não quero os casos sem url
  arrange(-fav_total) %>%
  head(20) %>%
  ggplot(aes(reorder(screen_name, fav_total), fav_total)) + 
  geom_bar(stat = "identity") + coord_flip()


### quais as hashtags mais tuitadas?

lulalivre %>%
  unnest(hashtags) %>% # unnest transforma colunas-lista em novas linhas
  group_by(hashtags) %>%
  summarise(n = n()) %>%
  na.omit()  %>% # não quero os casos sem url
  arrange(-n) %>%
  head(20)  %>%
  ggplot(aes(reorder(hashtags, n), n)) + 
  geom_bar(stat = "identity") + coord_flip()


### Agora vcs. Com base na lógica dos scripts anteriores, respondam:


# Qual o total de rts recebidos por usuário?

# Quais os 10 tuites mais retuitados?

# Quais os usuários mais mencionados?

# Quais as cidades mais frequentes na amostra?



#### Outros elementos da busca

## Não rodar - vai buscar até 100 mil tweets com a palavra brasil ou copa
search_tweets(
  "brasil OR copa", n = 100000, retryonratelimit = TRUE
)

###### buscar tweets no brasil

tw_br <- search_tweets(
  "lang:pt", geocode =lookup_coords("brasil"), n = 100
)


# plotar no mapa
## create lat/lng variables using all available tweet and profile geo-location data
tw_br <- lat_lng(tw_br)

## instalar httpuv
if (!require("devtools")) install.packages("devtools")
devtools::install_github("rstudio/httpuv")

### instalar leaflet
p_load(leaflet)

tweet_locations <- tw_br %>%
  filter(!is.na(lat))

# plot points on top of a leaflet basemap

site_locations <- leaflet(tweet_locations) %>%
  addTiles() %>%
  addCircleMarkers(lng = ~as.numeric(lng), lat = ~as.numeric(lat), popup = ~paste0("Perfil: ", screen_name, "\tTweet: \t", text),
                   radius = 5, stroke = FALSE, color = "#ac3973")

site_locations


### acionar conexão stream por um minuto

## stream london tweets for a week (60 secs x 60 mins * 24 hours *  7 days)
stream_tweets(
  "#lulalivre",
  timeout = 60,
  file_name = "lulalivre_tweets.json",
  parse = FALSE
)

## read in the data as a tidy tbl data frame
lula_stream <- parse_stream("lulalivre_tweets.json")



###### Coletar dados das timelines

## get user IDs of accounts followed by CNN
tw_perfil <- get_timelines(c("flamengo", "botafogo", "vascodagama", "fluminensefc"), n = 100)

## plotar a frequência de tweets

tw_perfil %>%
 group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
 geom_point() +
 theme_minimal() 


### como podemos plotar a quantidade de favoritos por dia?



# Quem um perfil segue?


doria_flws <- get_friends("jdoriajr")

## adicionar metadados
doria_flws <- lookup_users(doria_flws$user_id)


### perfis com mais seguidores

doria_flws %>% 
  select(screen_name, followers_count) %>%
  arrange(-followers_count) %>%
  head(20)
  

### percebam que por padrão a função extrai o último tweet. para deixar apenas os usuários

doria_per <- users_data(doria_flws)


### o que doria favoritou?

doria_favs <- get_favorites("jdoriajr", n = 100)


## Quem segue o perfil?
doria_seguidores <- get_followers("jdoriajr", n = 100)


## adicionar metadados
doria_seguidores <- lookup_users(doria_seguidores$user_id)
doria_seguidores <- users_data(doria_seguidores) # deixar apenas seguidores

# extrair os últimos 10 tweets de 100 seguidores de Doria

tw_perfil <- get_timelines(doria_seguidores$user_id, n = 10)


# Quais hashtags os seguidores de dória usam???


# Como podemos comparar com as hashtags dos seguidores de lula?

## procurar usuários com string na bio

usrs <- search_users("ciencia política", n = 100)
usrs <- users_data(usrs) # deixar apenas usuarios


## Vamos estudar quem os perfis brasileiros seguem?

usrs <- usrs %>%
  filter(account_lang == "pt")


cp_flws <- get_friends(usrs$user_id, n = 100)

### top trends

fortaleza <- get_trends("fortaleza")

############## Exercício

# Elabore uma busca de tweets

# Encontre as principais hashtags, usuários mais favoritados, urls mais retuitadas, cidades mais frequentes

### Compare o ritmo de postagem de cinco perfis

## Plote o total de retuites por dia


### Quem segue o perfil de lula e bolsonaro compartilha quais urls?
# Quais hashtags eles usam?


######## plotar rede de RTs

p_load(igraph) # analise de redes sociais
p_load(networkD3) # dataviz
p_load(janitor) # tabulação cruzada
p_load(stringi) # tratamento textual

doria <- search_tweets("jdoriajr", n=500)

# checar quantidade de rts

doria %>% 
  tabyl(is_retweet)

## criar lista de arestas de rts e contar

rede_doria <- doria %>% 
  filter(is_retweet == T) %>% 
  mutate(source = str_to_lower(screen_name),
         target = str_to_lower(retweet_screen_name))   %>% 
           count(source, target) %>% 
  add_column(relationship = "RT")


### criar lista de menções

rede_doria <- doria %>%
  unnest(mentions_screen_name) %>%
  filter(!is.na(mentions_screen_name)) %>%
  mutate(source = str_to_lower(screen_name)) %>% 
  mutate(target = str_to_lower(mentions_screen_name)) %>% 
  count(source, target) %>% 
  add_column(relationship = "mention") %>%
  bind_rows(rede_doria)



## transformar em objeto de redes

rt_graph <- graph_from_data_frame(d=rede_doria, directed=T)

# visualização simples
plot(rt_graph)

## identificar clusters

wc <- cluster_walktrap(rt_graph)

members <- membership(wc)

members

## criar visualização interativa

d3_rt <- igraph_to_networkD3(rt_graph, group = members)

forceNetwork(Links = d3_rt$links, Nodes = d3_rt$nodes, 
             Source = 'source', Target = 'target', 
             NodeID = 'name', Group = 'group', fontSize = 16, zoom = T)
