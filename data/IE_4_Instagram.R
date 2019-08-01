
# Instagram Scraper ----------------------------------------------------


# Marcelo Alves -----------------------------------------------------------
options(scipen = 999)
rm(list = ls())
pacman::p_load("jsonlite")
pacman::p_load("tidyverse")


############## Coletor do Instagram

instaScraper <- function(hashtag, n) {
  final <- data.frame()
  for (y in 1:n) {
    # paginar
    
    if (y == 1){
      url <- paste0("https://www.instagram.com/explore/tags/", hashtag, "/?__a=1")
    } else{
      url <- paginar
    }
    print(paste("Coletando da pagina", y, "com a query", hashtag, "em", Sys.time()))
    
    
    # read url and convert to data.frame
    document <- fromJSON(txt=url)
    s <- document$graphql$hashtag$edge_hashtag_to_media$edges$node$shortcode
    
    
    for (x in s) {
      print(paste("Rodando a iteracao", x))
      url <- paste0("https://www.instagram.com/p/",  x, "/?__a=1")
      # read url and convert to data.frame
      document2 <- fromJSON(txt=url)
      
      post_id <- document2$graphql$shortcode_media$id
      
      ### nome
      user_id <- document2$graphql$shortcode_media$owner$id
      user_name <- document2$graphql$shortcode_media$owner$username
      nome <- document2$graphql$shortcode_media$owner$full_name
      verified <- document2$graphql$shortcode_media$owner$is_verified
      
      # link
      
      link <- document2[["graphql"]][["shortcode_media"]][["display_url"]]
      #text
      text <- document2$graphql$shortcode_media$edge_media_to_caption$edges$node$text
      
      if (is.null(text)) {
        text <- NA
      }
      
      # comment
      comment <- document2$graphql$shortcode_media$edge_media_to_comment$count
      if (is.null(comment)) {
        comment <- NA
      }
      # plive
      preview_like <- document2$graphql$shortcode_media$edge_media_preview_like$count
      
      # is video
      is_video <- document2$graphql$shortcode_media$is_video
      
      #timestamp
      timestamp <- document2$graphql$shortcode_media$taken_at_timestamp
      
      # ad
      ad <- document2$graphql$shortcode_media$is_ad
      
      
      #
      location <- document2$graphql$shortcode_media$location
      
      if (!is.null(location)) {
        location_id <- location$id
        location_name <- location$name
      } else {
        location_id <- NA
        location_name <- NA
      }
      
      df <- data.frame(user_id, user_name, nome, verified, text, 
                       comment, preview_like, is_video, timestamp, ad, location_id, location_name, x, link)
      final <- rbind(final, df)
      
      
      
    }
    
    
    end_cursor <- document$graphql$hashtag$edge_hashtag_to_media$page_info$end_cursor
    paginar <- paste0("https://www.instagram.com/explore/tags/", hashtag, "/?__a=1&max_id=", end_cursor)
    ## limpar caracteres de quebra de linha
    
    final$text <- gsub(";", "", final$text)
    final$text <- gsub("[\r\n]", "", final$text)
    final$location_name <- gsub("[,-].*", "", final$location_name)
    final$link_post <- paste0("www.instagram.com/p/", final$x)
    
    # Tratar data
    final$timestamp <- as.POSIXct(final$timestamp, origin="1970-01-01")
    final$dia <- as.Date(as.POSIXct(final$timestamp, origin="1970-01-01"))
    final$hora <- as.POSIXct(trunc(final$timestamp, "hours"))
    final$minuto <-  as.POSIXct(trunc(final$timestamp, "mins"))
    final <- final %>% 
      add_column(query = hashtag)
    
    return(final)
    
  }
}


# Inserir hashtag - sem usar o caractere #

hashtag <- "lulalivre"

# total de imagens = n * 70 
n <- 1

######## rodar função
dados <- instaScraper(hashtag = hashtag, n = n)

# exportar
write.csv2(final, paste0("instagram_", hashtag,".csv"))



# Análise -----------------------------------------------------------------

# Tabela dos perfis que postaram mais vezes

# Sumário descritivo de curtidas e comentarios

# Serie temporal

# Associacao entre curtidas e comentarios


# Encontre os locais com mais curtidas



# Analise de hashtags
pacman::p_load(quanteda)

# gerar corpus

f <- dados %>%  select(text, user_name)
c <- corpus(f) # criar corpus
summary(c)

# matriz
dfm <- dfm(c, remove_punct = TRUE) # criar matriz de frequencia
tag_dfm <- dfm_select(dfm, pattern = ("#*")) # criar matriz de hashtags

# contar
tmp <- textstat_frequency(tag_dfm)



# filtrar
toptag <- names(topfeatures(tag_dfm, 50))
head(toptag)

# criar matriz de associacao
tag_fcm <- fcm(tag_dfm)
topgat_fcm <- fcm_select(tag_fcm, pattern = toptag) # aplicar filtro


textplot_network(topgat_fcm,
                 #vertex_size = filter(tmp, feature %in% toptag)$frequency, 
                 #vertex_color = "darkblue",
                 min_freq = 0.1, 
                 edge_alpha = 0.3, 
                 edge_size = 5) 



# Grafico dinamico --------------------------------------------------------


pacman::p_load(igraph)
pacman::p_load(sigmajs)


# criar arquivo de rede
g <- graph_from_adjacency_matrix(topgat_fcm, mode = "directed", weighted = TRUE)
g <- simplify(g)
vertex_attr(g, "label") <- V(g)$name

# gerar grafico
sigmajs() %>%
  sg_from_igraph(g) %>%
  sg_settings(drawLabels = T, drawEdgeLabels = FALSE) %>% 
  sg_neighbours() %>% 
  sg_cluster()



# Atividade ---------------------------------------------------------------

# Pratique os scripts acima a partir de uma extracao 
# de um tema de seu interesse
# utilize entre uma e tres hashtags
# no maximo n = 3

# ######## criar repeticao

hashtag <- c("lulalivre", "bolsonaro")
n = 1

dados <- data.frame()

for(y in hashtag) {
  dados <- rbind(dados, instaScraper(hashtag = y, n = n))
}

# Qual hashtag recebeu mais likes em media?

# Compare o total de likes por dia



