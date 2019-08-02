# --------------------------- Script Coleta Facebook ---------------------------
 # --------------------------- Marcelo Alves ---------------------------
 
rm(list=ls())

######## pacote

pacman::p_load(Rfacebook)
pacman::p_load(tidyverse)


# Obter autorizacao dos logs do Facepager
token <- "XXXXXXXXXXXXX"


# Listar ids das paginas

# Colar os links das paginas no site
# https://findmyfbid.com

# O Globo https://www.facebook.com/jornaloglobo/ - 115230991849922
# MBL https://www.facebook.com/mblivre/ - 204223673035117
# Estadao https://www.facebook.com/estadao/ 115987058416365
# Brasil247 https://www.facebook.com/Brasil247/ - 167637636622585
# intercept Brasil https://www.facebook.com/TheInterceptBr/ - 1754956431459064

paginas <- c("115230991849922", 
             "204223673035117", 
             "115987058416365",
             "167637636622585", 
             "1754956431459064")

### Extrair posts

posts <- data.frame()


# Rotina de extração
for (x in paginas) {
         try(
         posts <- rbind(posts,
                        getPage(page = x, 
                                      token=token, 
                                      reactions=T,
                                      since = "2019-01-01",
                                      until =  "2019-08-02",
                                      n=100))
         )
  
       print(paste(x, " Coleta = ", nrow(posts)))
      
}
   
   
# Tratamento data
posts$dia <- as.Date(gsub("T.*", "",posts$created_time))
posts$mes_dia <- format(posts$dia ,format="%y-%m")
posts$mes_dia <-  as.Date(paste0(posts$mes_dia, "-01"),"%y-%m-%d")
posts$ano <- lubridate::year(posts$dia)

# Exportar
   
write.csv2(posts, paste0("extração_face_", Sys.Date(), ".csv"))



# Analise -----------------------------------------------------------------


# Gere o sumario descritivo da variável compartilhamentos


# Faça uma série temporal dos compartilhamentos por mes



# Crie um boxplot com a log de curtidas por pagina


# Plote a associação entre o total de curtidas e comentarios
# agrupados por pagina


## Gere a serie temporal dos tipos 
# de posts  mais comentados


# Quais sao os links mais publicados?


# Quais fontes de sites mais compartilhadas?


## Compare as reações entre as paginas
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
  melt(id.vars = c("from_name"), 
       measure.vars = c("Love", "Thankful","Haha" ,"Wow", "Sad", "Angry")) %>% 
  ggplot(aes(reorder(variable, value), value, fill = from_name)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(x = "", y = "", title = paste("Reações por pagina - "), 
       subtitle = "Reações desagregadas: Love, Haha, Angry, Sad, Wow, Thankful", 
       caption = "Anotações a cada 1 mil") + labs(color='Total') + 
  theme_minimal() +
  theme(legend.position="bottom") + coord_flip() +
  facet_wrap(~ from_name, scales = "free", drop = T) +
  geom_text(aes(label=round(value/1000,2)), vjust=0, color="#616366", size = 2.8,
            hjust=-.1) 


## Compare as reações entre as paginas por mês


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
  melt(id.vars = c("from_name", "mes"), 
       measure.vars = c("Love", "Thankful","Haha" ,"Wow", "Sad", "Angry")) %>% 
  ggplot(aes(x=mes, y=value, group=variable)) +
  geom_line(aes(color=variable)) +
  labs(x = "", y = "", title = paste("Reações por pagina - "), 
       subtitle = "Reações desagregadas: Love, Haha, Angry, Sad, Wow, Thankful", 
       caption = "Anotações a cada 1 mil") + labs(color='Total') + 
  theme_minimal() +
  theme(legend.position="bottom") +
  facet_wrap(~ from_name, scales = "free", drop = T) 





# Comentarios -------------------------------------------------------------

comentarios <- data.frame()


for (x in 1:nrow(posts)) {
  ct <- getPost(post=posts$id[x], 
                comments = T, 
                likes = F, 
                token=token, 
                n.comments =  1000)
  ct <- ct$comments
  ifelse(is.null( nrow(ct)), next, 
         
     comentarios <- rbind(comentarios, 
                              cbind(rep(posts$from_name[x], 
                                        nrow(ct)), ct)))
  
}  

# Utilizando o quanteda, encontre as hashtags
# mais citadas nos comentarios


# Gere o grafico de associacao das hashtags


# Bonus Experimental ------------------------------------------------------

# Extrair quantas vezes um link 
# foi compartilhado no Facebook todo

tmp <- posts %>%  filter(link != "" & caption != "facebook.com")

shares <- data.frame()

for (x in tmp$link[1:nrow(tmp)]){ 
  url = paste0("https://graph.facebook.com/v3.2/?ids=", 
               x, 
               "&fields=engagement","&access_token=",
               token)
  API_call <- try(callAPI_2(url, token = token))
  if(class(API_call) == "try-error")
    next 
  if(ncol(as.data.frame(API_call)) != 5) next
  else 
    API_call <- as.data.frame(API_call)
  names(API_call)<- c("reaction_count",
                      "comment_count",
                      "share_count", 
                      "comment_plugin_count", 
                      "link")
  
  shares <- rbind(shares, API_call)
  print(nrow(shares))
  Sys.sleep(.5)
  
  
  
} 