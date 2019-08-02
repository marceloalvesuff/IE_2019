
########## instalar pacotes


library(pacman)

p_load(tidyverse) # framework para datascience




rm(list = ls())


############################### Preparação dos dados


#### listar arquivos

arquivos_zip <- list.files(pattern = "*.zip", full.names = TRUE)

# Aplicar função unzip ao vetor dos arquivos

sapply(arquivos_zip, unzip)
rm(arquivos_zip)

### Ler arquivos 

arquivos_tab <- list.files(pattern = "*.tab", full.names = TRUE)

# Deixar apenas os posts
# Excluir stats per day
arquivos_tab <- subset(arquivos_tab, !grepl("stats", arquivos_tab))


### Carregar

lista <- sapply(arquivos_tab, read.delim)

### Unir
# tbl <- lapply(arquivos_tab, read.delim) %>% bind_rows()

posts <- do.call(rbind, lapply(arquivos_tab, read.delim))

rm(lista, arquivos_tab)

### Inserir variável nome da página
ids <- read.csv2("ids_netvizz.csv")
ids$from_name <- as.character(ids$from_name)

# limpar variavel em posts

posts$by <- gsub(pattern = "post_page_", "", posts$by)

## junção

posts <- merge(posts, ids, by.x = "by", by.y = "from_id")

# preparar os dados

posts <- posts %>%
  rename(from_id = by, 
         message = post_message)  %>%
  mutate(created_time = as.Date(post_published_sql),
         dia = as.Date(posts$post_published_sql),
         dia_mes = format(as.Date(posts$post_published_sql), "%d/%m"),
         mes = format(as.Date(posts$post_published_sql), "%B"))


# Qual o total de postagens dos candidatos?

# Compare a média de curtidas recebidas pelos posts e plote em gráfico de barras




### Quantas publicações por mês?
# ordenar fator

posts$mes <- factor(posts$mes, levels = c("janeiro", "fevereiro", "março",
                                          "abril", "maio", "junho", "julho"))

posts %>%
  group_by(mes) %>%
  summarise(n = n()) %>%
  ggplot(aes(x=mes, y=n, group=1)) +
  geom_line(aes(group=1), colour="#7dd4db")+
  geom_point(colour="#7dd4db")+ theme_classic() +
  labs(x = "", y = "", title = "Quantidade de publicações por mês")


### Qual a variação diária das publicações?


# adicionar ao ggplot 
#+theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#  scale_x_date(date_labels="%d/%m",date_breaks  ="1 week")

## Comparativo das publicações por mês entre candidatos


posts %>%
  group_by(mes, from_name) %>%
  summarise(n = n()) %>%
  ggplot(aes(x=mes, y=n, group=from_name)) +
  geom_line(aes(color=from_name))+
  geom_point(aes(color=from_name))+ theme_classic() +
  labs(x = "", y = "", title = "Quantidade de publicações por mês")


### Quais posts foram mais compartilhados?

## Compare a média de comentários mensais entre as páginas



## Quais tipos de posts foram mais compartilhados?


posts %>% 
  group_by(type) %>% 
  summarise(media = sum(shares_count_fb, na.rm = T)) %>%
  filter(media > 0) %>%
  ggplot(aes(type, media)) +
  geom_col(fill="#7dd4db") +
  labs(x = "", title = "Média de compartilhamento por tipo de publicação") +
  coord_flip() + theme_classic()


## Compare as reações entre os candidatos
p_load(reshape2)

posts %>% 
  select(from_name,  rea_LOVE, rea_HAHA, rea_ANGRY, rea_SAD, rea_THANKFUL, 
         rea_WOW)%>% 
  group_by(from_name) %>% 
  summarise(Love = sum(rea_LOVE,  na.rm = T),
            Wow = sum(rea_WOW,  na.rm = T),
            Sad = sum(rea_SAD,  na.rm = T),
            Angry = sum(rea_ANGRY,  na.rm = T),
            Thankful = sum(rea_THANKFUL,  na.rm = T),
            Haha = sum(rea_HAHA,  na.rm = T)) %>% 
  melt(id.vars = c("from_name"), measure.vars = c("Love", "Thankful","Haha" ,"Wow", "Sad", "Angry")) %>% 
  ggplot(aes(reorder(variable, value), value, fill = from_name)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(x = "", y = "", title = paste("Reações por Candidato - "), 
       subtitle = "Reações desagregadas: Love, Haha, Angry, Sad, Wow, Thankful", 
       caption = "Anotações a cada 1 mil") + labs(color='Total') + 
  theme_minimal() +
  theme(legend.position="bottom") + coord_flip() +
  facet_wrap(~ from_name, scales = "free", drop = T) +
  geom_text(aes(label=round(value/1000,2)), vjust=0, color="#616366", size = 2.8,
            hjust=-.1) 


## Compare as reações entre os candidatos por mês


posts %>% 
  select(from_name, mes, rea_LOVE, rea_HAHA, rea_ANGRY, rea_SAD, rea_THANKFUL, 
         rea_WOW)%>% 
  group_by(from_name, mes) %>% 
  summarise(Love = sum(rea_LOVE,  na.rm = T),
            Wow = sum(rea_WOW,  na.rm = T),
            Sad = sum(rea_SAD,  na.rm = T),
            Angry = sum(rea_ANGRY,  na.rm = T),
            Thankful = sum(rea_THANKFUL,  na.rm = T),
            Haha = sum(rea_HAHA,  na.rm = T)) %>% 
  melt(id.vars = c("from_name", "mes"), measure.vars = c("Love", "Thankful","Haha" ,"Wow", "Sad", "Angry")) %>% 
  ggplot(aes(x=mes, y=value, group=variable)) +
  geom_line(aes(color=variable)) +
  labs(x = "", y = "", title = paste("Reações por Candidato - "), 
       subtitle = "Reações desagregadas: Love, Haha, Angry, Sad, Wow, Thankful", 
       caption = "Anotações a cada 1 mil") + labs(color='Total') + 
  theme_minimal() +
  theme(legend.position="bottom") +
  facet_wrap(~ from_name, scales = "free", drop = T) 


# Encontre os cinco posts com mais love para cada candidato


# Encontre os cinco posts com mais grr para cada candidato


# Plote um gráfico de pontos para mostrar a associação 
# entre curtidas e comentários das postagens
# Marque as cores dos pontos de acordo com os candidatos




###### Para finalizar, baixem as postagens referentes a três fan-pages
# Utilize canais do seu interesse de pesquisa 
# Comecem uma sessão nova e faça o processamento dos dados
# Faça perguntas aos dados e elabore o script para respondê-las