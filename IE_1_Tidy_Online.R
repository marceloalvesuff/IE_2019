# Escola de Inverno do IESP
# Mineração de dados das mídias sociais
# Marcelo Alves 

# Aula 1


# Pacotes e dados ---------------------------------------------------------
options(scipen = 999)

# instalar pacotes, caso não tenha no sistema
install.packages("tidyverse")
install.packages("janitor")
install.packages("reshape2")


# Carregar pacotes
library(tidyverse)
library(janitor)
library(reshape2)

# Carregar dados
load("deputados_facebook.Rdata")


# Tratamento de data
posts$mes <- NULL
posts$dia <- as.Date(gsub("T.*", "",posts$created_time))
posts$mes_dia <- format(posts$dia ,format="%y-%m")
posts$mes_dia <-  as.Date(paste0(posts$mes_dia, "-01"),"%y-%m-%d")
posts$ano <-  format(posts$dia ,format="%Y")



# Tidyverse ---------------------------------------------------------------

# Encontre a média de curtidas, comentários e compartilhamentos

posts %>% 
  summarise(curtidas = mean(likes_count), 
            comentarios = mean(comments_count),
            compartilhamentos = mean(shares_count))


# Atividade - crie uma tabela com total de love, haha, wow, sad, angry

# Liste os 50 deputados mais mais seguidores no Facebook
# Exiba na tabela nome, partido, estado e seguidores


deputados_facebook %>%
  arrange(-fan_count) %>%
  select(nomeParlamentar, uf, partido, fan_count) %>%
  rename(Seguidores = fan_count) %>%
  head(20)


# Atividade - Como exibir uma lista de deputados com maior núméro de posts?



# Calcule o numero de deputados no Facebook por partido

tmp <- deputados_facebook %>% 
  group_by(partido) %>% 
  summarise(n = n()) %>% 
  arrange(-n)


# Calcule o numero de deputados percentual por partido

deputados%>% 
  group_by(partido) %>% 
  summarise(total = n()) %>% 
  inner_join(tmp) %>% 
  mutate(p = n/total) %>% 
  arrange(-p)


# Atividade - crie uma tabela com o percentual de dpt. no Face por Estado


# Criar tabela com sumário descritivo de seguidores

tmp <- deputados_facebook %>%
  group_by(partido) %>%
  summarise(N = n(),
            Media = round(mean(fan_count),2),
            Mediana = median(fan_count),
            Maximo = max(fan_count),
            Minimo = min(fan_count), 
            Desvio = sd(fan_count),
            Total = sum(fan_count)) %>%
  arrange(-Mediana)

# Atividade - gere o sumário descritivo do número de posts por sexo


# Partidos mais amados e odiados

deputados_facebook %>%
  group_by(partido) %>%
  summarise(Amor = median(Amor_perc, na.rm = T),
            Bravo = median(Bravo_perc, na.rm = T)) %>% 
  arrange(-Bravo)




# Visualização com ggplot -------------------------------------------------


####### Gráfico de Barras

# Média de Curtidas por sexo


deputados_facebook %>% 
  group_by(sexo) %>% 
  summarise(curtidas_media = mean(fan_count),
            curtidas_mediana = median(fan_count)) %>% 
  ggplot(aes(x = sexo, y = curtidas_media)) + 
  geom_col()


# Crie um gráfico de barras com os tipos de posts mais utilizados

# Ordenamento, inversão do eixo, controle de cores e transparencia



p <- deputados_facebook %>% 
  group_by(sexo) %>% 
  summarise(curtidas_media = mean(fan_count),
            curtidas_mediana = median(fan_count)) %>% 
  ggplot(aes(x = reorder(sexo, curtidas_mediana), y = curtidas_mediana,
             color = sexo, fill = sexo)) + 
  geom_col(alpha = .7) + coord_flip() + theme_minimal()

p
# salvando p no computador
ggsave(filename = "meugrafico.png",  height = 6, width = 6)


# Aplique os comandos acima ao gráfico de tipos de posts


# Cor manual


deputados_facebook %>% 
  group_by(sexo) %>% 
  summarise(curtidas_media = mean(fan_count),
            curtidas_mediana = median(fan_count)) %>% 
  ggplot(aes(x = sexo, y = curtidas_media)) + 
  geom_col(color = "indianred", fill = "indianred", alpha = .7)


# Rotulagem: titulo, subtitulo, caption e labels


p  +
  labs(title="Comparação de curtidas por sexo",x="", 
       y = "Curtidas (Mediana)", 
       subtitle = "Deputadas possuem mediana de seguidores superior", 
       caption = "Fonte: Facebook Graph API")



# Atividade = Crie um gráfico de barras com os 10 deputados
# com maior número de seguidores
# Adicione a rotulagem completa


# labels


p +
  geom_text(aes(label =  prettyNum(round(curtidas_mediana, 0), big.mark = "."), color=sexo,), # round arredonda o valor, prettyNum adiciona marcas
            size = 6,  hjust = -.1, fontface= "bold") +
  expand_limits(y = 50000) +# expandir o limite de y para caber a anotação
  theme(legend.position = "none") + # eliminar legenda
  theme_classic()



# Dispersão


# Dispersão básica
deputados_facebook %>% 
  ggplot(aes(x=Curtidas, 
             y= Comentários)) + geom_point() + theme_minimal()


# Adicionando cores

deputados_facebook %>% 
  ggplot(aes(x=Curtidas, 
             y= Comentários, color = sexo)) + geom_point() + theme_minimal()


# Crie um gráfico de dispersão entre percentagem de reações amor X bravo




# Gráfico de bolhas

# Alterar tamanho de acordo com parametro
deputados_facebook %>% 
   ggplot(aes(x=Curtidas, 
                y= Compartilhamentos)) +
  geom_point(aes(size=N_posts), alpha = .3) + 
  theme_classic() +
  theme(legend.position = "none")



## adicionar linha de regressão = geom_smooth

deputados_facebook %>% 
  ggplot(aes(x=Curtidas, 
             y= Compartilhamentos)) +
  geom_point(aes(size=N_posts), alpha = .3) + 
  theme_classic() +
  theme(legend.position = "none") +
  geom_smooth(method=lm, se = T, color = "skyblue", fill = "skyblue")




# Boxplot

deputados_facebook %>%
  ggplot(aes(x=uf, y=N_posts)) + 
  geom_boxplot() + coord_flip()

# reordenando para facilitar leitura

deputados_facebook %>%
  na.omit %>% 
  ggplot(aes(x=reorder(uf, N_posts, FUN = median), y=N_posts)) + 
  geom_boxplot() + coord_flip() 


# Adicionando os pontos


deputados_facebook %>%
  na.omit %>% 
  ggplot(aes(x=reorder(uf, N_posts, FUN = median), y=N_posts)) + 
  geom_boxplot() + 
  geom_jitter(alpha = .4, color = "skyblue") +
  coord_flip() + theme_classic()



## Crie um box plot ordenado relacionando partidos e love (perc)

## Gráfico de linha

# Evolução diária de posts


posts %>% 
  group_by(dia) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x=dia, y=n, group=1)) +
  geom_line(color = "skyblue") + theme_minimal()


# Estilizando o gráfico
install.packages("ggrepel")
library(ggrepel)

res <- posts %>% 
  group_by(dia) %>% 
  summarise(n = n()) 

res %>% 
  ggplot(aes(x=dia, y=n, group=1)) +
  geom_line(color = "#436685", size = 3) +
  geom_point(color = "#436685", fill = "white", size = 4, shape = 21)+
  geom_label_repel(data=top_n(res, 1), 
                   aes(label=paste("Dia", format(dia, "%d"), "ocorreram", n, "posts")),
                   direction = "y" , fill = "#436685", color = "white", size = 4.5, alpha = .9
  ) + theme_minimal() +
  expand_limits( y = 1500)

# Linhas multiplas

res <- posts %>% 
  group_by(dia, type) %>% 
  summarise(n = n()) %>% 
  na.omit() 


res %>% 
  ggplot(aes(x=dia, y=n, group = type, color=type))  +
  geom_line(size = 3) +
  geom_point(fill = "white", size = 4, shape = 21) +
  geom_label_repel(data=tail(res, 4), aes(label=paste(type, n),
                                           fill = type),
                   direction = "x" ,nudge_x = .5, color = "white", size = 4.5, alpha = .9
  ) + theme_minimal()+ 
  theme(legend.position="none")


# Atividade
# Criar gráfico de linhas multiplas com posts por sexo
# a tabela já foi gerada de acordo com o pipe abaixo

dep <- deputados_facebook %>% group_by(sexo) %>% summarise(n = n())

res <- posts %>% 
  inner_join(select(deputados_facebook, id_facebook, sexo), 
             by = c("from_id" = "id_facebook")) %>% 
  group_by(dia, sexo) %>% 
  summarise(posts = n()) %>% 
  inner_join(dep, by ="sexo") %>% 
  mutate(media = posts/n)


# grafico
res



# Facetas

res <- posts %>% 
  inner_join(select(deputados_facebook, id_facebook, uf), 
             by = c("from_id" = "id_facebook")) %>% 
  filter(uf %in% c("RJ", "SP", "SC", "ES")) %>% 
  group_by(dia, uf) %>% 
  summarise(n = n()) %>% 
  na.omit() 


res %>% 
  ggplot(aes(x=dia, y=n, group = uf, color=uf))  +
  geom_line(size = 3) +
  geom_point(fill = "white", size = 4, shape = 21) +
  facet_wrap(~ uf) +
  theme_minimal() +
  theme(legend.position="none") 

# Crie um gráfico de linhas utilizado facetas de quatro partidos



