# Escola de Inverno do IESP
# Mineração de dados das mídias sociais
# Marcelo Alves 

# Aula 3 - Youtube

# Youtube Data Tools

# https://tools.digitalmethods.net/netvizz/youtube/

# Módulo Video List
# Search Query
# Relevance language = pt
# region code: BR


# Pacotes, Dados e Tratamento -----------------------------------------------------------------
options(scipen = 999)

library(tidyverse)
pacman::p_load("hrbrthemes") # tema ggplot
#pacman::p_load("ggrepel") # ordenar labels do plot
import_roboto_condensed() # baixar fonte
extrafont::loadfonts() # carregar fonte

# Carregar dados

read_plus <- function(file) {
  read.delim(file) %>% 
    mutate(query = file)
}

dados <- list.files(pattern = "*.tab", 
             full.names = T) %>% 
  map_df(~read_plus(.))


dados$query <- gsub(".tab|./","",dados$query )


# Limpeza de data


dados <- dados %>%
  mutate(data = as.Date(publishedAtSQL),
         dia = as.Date(data, "%d-%m-%y"), 
         mes_dia = format(data,format="%y-%m"), 
         mes_dia = as.Date(paste0(mes_dia, "-01"),"%y-%m-%d"), 
         ano = format(data,format="%Y"))


# Análise -----------------------------------------------------------------

glimpse(dados)



# Quais canais tiveram mais visualizações, likes, dislikes e comentários?

tmp <- dados %>% 
  group_by(channelTitle) %>% 
  summarise(video = n(),
            views = sum(viewCount, na.rm = T),
            likes = sum(likeCount, na.rm = T),
            dislikes = sum(dislikeCount, na.rm = T), 
            comments = sum(commentCount, na.rm = T)) %>% 
  arrange(-views)

# gráfico de barras 10 principais canais

tmp %>% 
  top_n(10, views) %>% 
  ggplot(aes(x = reorder(channelTitle, 
                         views), 
             y = views)) +
  geom_col(fill = "mediumpurple4", 
           alpha = .7) +
  geom_hline(yintercept = median(tmp$views), 
             linetype = "dashed") +
  theme_minimal() +
  coord_flip() +
  labs(title = "Canais mais visualizados",
       subtitle = "Total de visualizações recebidas nos vídeos extraídos",
       x =  "", 
       y = "Visualizações") + 
  geom_text(aes(label = prettyNum(views, big.mark = ".")), 
            hjust = -.02, 
            color = "mediumpurple4", 
            fontface = "bold") +
  expand_limits(y=max(tmp$views) *1.2) +
  theme_ipsum_rc(grid = "X") 


# Atividade: Crie um gráfico de barras exibindo os títulos dos vídeos mais negativados


# Associação entre likes e dislikes

top_n(tmp, 5, likes)$channelTitle
tmp  %>% 
  ggplot(aes(log(dislikes), log(likes))) +
  geom_point()+ 
  geom_text(data= top_n(tmp, 50, likes), 
            aes(label = channelTitle), 
            hjust = -.02, 
            color = "black", 
            check_overlap = T) +
  geom_smooth(method=lm, se = T, color = "skyblue", fill = "skyblue") +
  theme_ipsum_rc(grid = "none") 



# Qual consulta tem maior média de interações?·


tmp <- dados %>% 
  group_by(query) %>% 
  summarise(video = n(),
            views = mean(viewCount, na.rm = T),
            likes = mean(likeCount, na.rm = T),
            dislikes = mean(dislikeCount, na.rm = T), 
            comments = mean(commentCount, na.rm = T)) %>% 
  arrange(-views)

# Atividade: Exiba um gráfico com a associaçaõ entre comentarios e visualizacoes


# Atividade: Crie um gráfico de colunas com as categorias de vídeos
# com maior média de views


# Série temporal das visualizações

dados %>% 
  group_by(ano) %>% 
  summarise(views = sum(viewCount, na.rm = T)) %>% 
  ggplot(aes(x=ano, 
             y=views, 
             group=1)) +
  geom_line(color = "mediumpurple4", 
            size = 3) +
  geom_point(color = "mediumpurple4", 
             fill = "white", 
             size = 4,
             shape = 21) +
  theme_ipsum_es(grid = "Y")


# Desagregar a série temporal comparando as visualizações
# por consulta (query)


dados %>% 
  group_by(ano, query) %>% 
  summarise(views = sum(viewCount, na.rm = T)) %>% 
  ggplot(aes(x=ano, y=views, group=query, color = query)) +
  geom_line( size = 3) +
  geom_point( fill = "white", size = 4, shape = 21) +
  theme_ipsum_es(grid = "Y")


# Atividade: Gere a série temporal anual 
# dos dislikes por query



### Comparando canais

# Encontrar id 
# https://commentpicker.com/youtube-channel-id.php

# https://www.youtube.com/user/jbolsonaro - UC8hGUtfEgvvnp6IaHSAg1OQ
# https://www.youtube.com/user/olavodeca - UC6RQhzm93SterWntL7GzqYQ

# demorado
# https://www.youtube.com/user/estadao - UCrtOL8bJsh-UC6RQhzm93SterWntL7GzqYQ

# salvar dados em nova pasta com o nome canal

filenames <- list.files(path = "canal",
                        pattern = ".tab", 
                        all.files = FALSE, 
                        full.names = FALSE,
                        ignore.case = FALSE)

dados <- do.call("rbind", sapply(paste0("canal/", filenames),
                                 read.delim, 
                                 simplify = FALSE,  
                                 encoding = "UTF-8"))

dados$query <- gsub(".tab*.|[0-9]+","",
                    rownames(dados))


# Limpeza de data


dados <- dados %>%
  mutate(data = as.Date(publishedAtSQL),
         dia = as.Date(data, "%d-%m-%y"), 
         mes_dia = format(data,format="%y-%m"), 
         mes_dia = as.Date(paste0(mes_dia, "-01"),"%y-%m-%d"), 
         ano = format(data,format="%Y"))

# Contagem de vídeos por canal

dados %>% 
  group_by(channelTitle) %>% 
  summarise(video = n())


# Série temporal visualizações

dados %>% 
  group_by(ano, channelTitle) %>% 
  summarise(views = sum(viewCount, na.rm = T)) %>% 
  na.omit %>% 
  ggplot(aes(x=ano, y=views, 
             group=channelTitle, 
             color = channelTitle)) +
  geom_line( size = 3) +
  geom_point( fill = "white", size = 4, shape = 21) +
  theme_ipsum_es(grid = "Y")


# Atividade = encontre o sumário descritivo dos canais



### Final

# Realize uma busca por termos ou compare dois canais de seu interesse
# Seguir os scripts acima
