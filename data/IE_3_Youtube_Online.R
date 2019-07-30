
# --------------------------- Coleta e sistematização de dados na web ---------------------
# --------------------------- IESP --------------------------------------------------------
# --------------------------- Marcelo Alves -----------------------------------------------
# --------------------------- Youtube -----------------------------------------------


########## instalar pacotes
devtools::install_github("soodoku/tuber")

library(pacman)



p_load(tuber) # conexão à API do youtube
p_load(RSQLite) # armazenamento
p_load(tidyr) # tratamento de dados
p_load(tidyverse) # framework para datascience
p_load(ggplot2) # dataviz
p_load(janitor) # tabulações
options(scipen = 999) # retirar notação científica


# Primeiro, é importante limpar nosso ambiente com a função rm()

rm(list = ls())

## definir diretório de trabalho

# atalho CTRL SHIFT H





################## Youtube Analytics


##### Para criar o token, visite a página https://developers.google.com/youtube/v3/getting-started

### Vá para o console de desenvolvedores https://console.developers.google.com/?pli=1

# Crie um projeto e na aba APIs & Services selecione Youtube Data API v3 e FreeBase API - Enable


# Em Credentials, crie o Oauth Client IC

# Gere a autentificação
client_id <- "XXXXXXXXXXX"
client_secret <- "XXXXXXXXXXX"

yt_oauth(client_id, client_secret)



############ vamos testar a consulta abaixo

get_stats(video_id = "N708P-A45D0")


# Se apareceu resultado no console, a chave está aprovada e estamos prontos para começar a baixar dados

# Documentação 
# https://cran.r-project.org/web/packages/tuber/tuber.pdf

# Get Information About a Video

get_video_details(video_id = "N708P-A45D0")


# Vamos começar usando o módulo de busca de vídeos

help(yt_search)

lula_vid <- yt_search(term = "Lula", # Importante notar que a busca é case sensitive
                      max_results = 10, # quantidade de resultados a serem iterados
          published_before = "2015-01-01T00:00:00Z") # parâmetro de data, antes de 2015



### a search não é tão precisa como das demais plataformas, vou checar quais são os canais que mais postaram

lula_vid %>%
  tabyl(channelTitle) %>%
  arrange(-n) %>%
  head(10)


## vamos manter apenas alguns

lula_vid <- lula_vid %>%
  filter(channelTitle %in% c("Ficha Social", "Instituto Lula", "Ricardo Noblat", "Planalto"))

### passar a lista para retornar as métricas

# precisamos rodar uma função lapply de repetição ou podemos criar um loop para iterar pelos ids
videostats = lapply(as.character(lula_vid$video_id), function(x){
  get_stats(video_id = x)
})

## transformar lista em df
p_load(data.table) # precisamos da função rbindlist para unir listas com números de colunas distintos

# aplicando rbindlists, as listas que não possuírem as colunas retornam NA

lula_stats = rbindlist(videostats, fill = T)

# percebam que as stats estão em caracteres (strings), precisamos transformar para numerico

sapply(lula_stats, class)

# então, vou criar um loop iterando pelas colunas, exceto id para transformar
lula_stats <- as.data.frame(lula_stats)

for (x in 2:ncol(lula_stats)) {
  lula_stats[, x] <- as.numeric(unlist(lula_stats[, x]))
}

# checar se funcionou
sapply(lula_stats, class)


### agora vamos mesclar os dados


lula_final <- merge(lula_vid, lula_stats, 
                    by.x = "video_id", by.y = "id") # especifiquei as colunas a serem mescladas por estarem com nomes distintos

rm(lula_vid, lula_stats, videostats)

# Manter apenas colunas pertinetes à análise

lula_final <- lula_final %>%
  select(-grep(pattern = "thumbnail", names(lula_final))) %>%
  droplevels()

glimpse(lula_final)





### Qual canal possui mais views?

lula_final %>%
  group_by(channelTitle) %>%
  summarise(views_media = mean(viewCount, na.rm = T)) %>%
  ggplot(aes(x = reorder(channelTitle, -views_media), y = views_media)) + 
  geom_bar(stat = "identity", fill = "red", alpha = .5) + theme_classic()  + theme(
    axis.line = element_blank()) +
  labs(title = "Média de views por canal", x = "", y = "")


### Agora, vamos fazer um exercício para estudar quais canais possuem mais favorites e comments



#################### Qual a relação entre likes e dislikes count?
p_load(reshape2) # transformar formato de wide em long
p_load(scales) # adicionar porcentagem



# primeiro é necessário criar row percentages 

tmp <- lula_final %>%
  group_by(channelTitle) %>%
  summarise(likes_media = mean(likeCount, na.rm = T),
            dislikes_media = mean(dislikeCount, na.rm = T)) %>% # até aqui fiz a média dos valores
  gather(variable, value, -channelTitle) %>% # formato longo
  group_by(channelTitle) %>% 
  mutate(percentage = value/sum(value)) %>% # transformando em proporções
  select(-value) 


View(tmp)
# criar níveis para ordenar
p_load(magrittr) # extract2

lvls <- tmp %>% filter(variable == "dislikes_media") %>% arrange(-percentage) 
o <- tmp %>% filter(variable == "dislikes_media") %>% arrange(percentage) %>% extract2("channelTitle")

# retornar para o dataset
lvls$plotOrder <- rownames(lvls)
lvls <- lvls %>% select(channelTitle, plotOrder)
tmp <- merge(tmp, lvls, by = "channelTitle", all.x = T)

View(tmp)

# plotar

tmp %>% 
  mutate(channelTitle = factor(channelTitle, levels = as.character(o))) %>% 
  ggplot() +
  aes(x = channelTitle, y = percentage, fill = reorder(variable, plotOrder), label = percent(tmp$percentage)) +
  geom_bar(position = "fill", stat = "identity", alpha = .7) +
  coord_flip() +
  labs(title = "Porcentagem de curtidas e descurtidas nos vídeos", x = "", y = "") + 
  guides(fill=guide_legend(title="Legenda")) +theme_classic() + theme(
    axis.line = element_blank()) +
  geom_text(size = 5,  position = position_stack(vjust = 0.5), color="white")


rm(lvls, o, x, tmp)


#################### Vamos estudar um canal específico



### Usar o link para pegar o id do canal 

# anitta

# http://johnnythetank.github.io/youtube-channel-name-converter/

id <- "UCqjjyPUghDSSKFBABM_CXMw"


# estatisticas gerais do canal
chstat = get_channel_stats(id)

genstat = data.frame(Channel=chstat$snippet$title, 
                     Subcriptions=chstat$statistics$subscriberCount,
                     Views = chstat$statistics$viewCount,
                     Videos = chstat$statistics$videoCount, 
                     Likes = sum(videostats$likeCount),
                     Dislikes = sum(videostats$dislikeCount), 
                     Comments = sum(videostats$commentCount))


########## Estudar os videos

### baixar videos
anitta <- yt_search(term="", type="video", channel_id = id)


videostats = lapply(as.character(anitta$video_id), function(x){
  get_stats(video_id = x)
})

videostats = do.call(rbind.data.frame, videostats)


### transformar

videostats$channelTitle = anitta$channelTitle
videostats$title = anitta$title
videostats$date = as.Date(anitta$publishedAt)
anitta_stats = select(videostats, date, title, viewCount, likeCount, dislikeCount, commentCount) %>%
  as.tibble() %>%
  mutate(viewCount = as.numeric(as.character(viewCount)),
         likeCount = as.numeric(as.character(likeCount)),
         dislikeCount = as.numeric(as.character(dislikeCount)),
         commentCount = as.numeric(as.character(commentCount)))


######## Views por mes do ano

p_load(lubridate)

anitta_stats %>%
  mutate(mes = format(date, "%Y/%m")) %>% 
  group_by(mes) %>%
  summarise(views = sum(viewCount)) %>%
  ggplot(aes(x = mes, y = views, group = 1)) +
  geom_line(aes(group=1), colour="#7dd4db") +
  geom_point(colour="#7dd4db") +
  labs(title = "Total de visualizações por mês", x = "", y = "") + 
  guides(fill=guide_legend(title="Legenda")) +theme_classic() + theme(
    axis.line = element_blank()) + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# qual clipe com mais visualizações?

anitta_stats %>%
  mutate(mes = format(date, "%Y/%m")) %>%
  filter(mes == "2015/10")


### Como podemos comparar mais de um canal?

ids <- c("UCqjjyPUghDSSKFBABM_CXMw", "UCffDXn7ycAzwL2LDlbyWOTw", "UCSCB1IQUmNa8Gn5VfSUAUpg")

### criar dfs a serem preenchidos com rbind
videos_total <- data.frame()
canais_stats <- data.frame()
stats_total <- data.frame()


# rodar iteração
for (x in seq_along(ids)) {
  canais = get_channel_stats(ids[x]) # iteração para baixar os canais
  videos <- yt_search(term="", type="video", channel_id = ids[x]) # iteração para baixar os videos
  
  # registrar
  videos_total <- rbind(videos_total, videos) # unir dataset de videos
  print(paste("Videos coletados", nrow(videos_total)))
  
  # pegar as estatísticas dos vídeos
  tmp = lapply(as.character(videos$video_id), function(x){
    get_stats(video_id = x)
  })
  
  # gerar o data.frame final com rbind
  videostats = do.call(rbind.data.frame, tmp)
  
  
  # preparar 
  videostats$channelTitle = videos$channelTitle # buscando info no dataset de videos
  videostats$id = videos$video_id # buscando info no dataset de videos
  videostats$title = videos$title # buscando info no dataset de videos
  videostats$date = videos$publishedAt  # buscando info no dataset de videos
  videostats = select(videostats, channelTitle, id, date, title, viewCount, likeCount, dislikeCount, commentCount) %>%
    as.tibble() %>%
    mutate(viewCount = as.numeric(as.character(viewCount)),
           likeCount = as.numeric(as.character(likeCount)),
           dislikeCount = as.numeric(as.character(dislikeCount)),
           commentCount = as.numeric(as.character(commentCount)))
  
  # unir dataset de tats
  stats_total <- rbind(stats_total, videostats)
  
  # criar dados finais do canal na iteração
  tmp <- data.frame(Channel=canais$snippet$title, 
                                                 Subcriptions=canais$statistics$subscriberCount,
                                                 Views = canais$statistics$viewCount,
                                                 Videos = canais$statistics$videoCount,
                                                 Likes = sum(videostats$likeCount),
                                                 Dislikes = sum(videostats$dislikeCount), 
                                                 Comments = sum(videostats$commentCount))
  # unir
                        
     canais_stats <- rbind(canais_stats, tmp)                   
}

rm(tmp, videostats, chstat, genstat, videos, canais)



##################### Extrair comentários dos videos

### ùltimo vídeo
anitta_comments <- get_comment_threads(filter = c(video_id = "wlS6Ix7mA0w"))



com <- get_comment_threads(filter = "video_id", anitta$video_id[1])
# Iteração com apply para extrair de todo o canal
comments = lapply(as.character(anitta$video_id), function(x){
  get_comment_threads(c(video_id = x), max_results = 1000)
})


######### Quais são os vídeos recomendados?

anitta_related <- get_related_videos(video_id = "wlS6Ix7mA0w", max_results = 50,
                   safe_search = "none")

######### Inscritos no canal

get_subscriptions()