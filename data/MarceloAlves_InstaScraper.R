# Instagram Scraper ----------------------------------------------------
# Marcelo Alves -----------------------------------------------------------
# Script atualizado em 2021
# Pode apresentar instabilidade em decorrência de alterações no Instagram

# instalar e carregar pacotes
pacotesNecessarios = c('jsonlite', "dplyr","magrittr", "quanteda", "igraph", "tibble")


for(p in pacotesNecessarios){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}


# Funcao InstaScraper -----------------------------------------------------
instaScraper <- function(hashtag, n) {
  
  for (y in 1:n) {
    # paginar
    print(y)
    if (y == 1){
      final <- data.frame()
      url <- paste0("https://www.instagram.com/explore/tags/", hashtag, "/?__a=1")
    } else{
      url <- paginar
    }
    print(paste("Coletando da pagina", y, "com a query", hashtag, "em", Sys.time(), nrow(final)))
    
    
    # read url and convert to data.frame
    document <- fromJSON(txt=url)
    s <- document$graphql$hashtag$edge_hashtag_to_media$edges$node$shortcode
    
    
    for (x in s) {
      print(paste("Rodando a iteracao", x))
      
      url <- try(paste0("https://www.instagram.com/p/",  x, "/?__a=1"))
      if(class(url) == "try-error")
        next 
      
     
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
      comment <- document2$graphql$shortcode_media$edge_media_preview_comment$count
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
      
      ## limpar caracteres de quebra de linha
      
      df$text <- gsub(";", "", df$text)
      df$text <- gsub("[\r\n]", "", df$text)
      df$location_name <- gsub("[,-].*", "", df$location_name)
      df$link_post <- paste0("www.instagram.com/p/", df$x)
      
      # Tratar data
      df$timestamp <- as.POSIXct(df$timestamp, origin="1970-01-01")
      df$dia <- as.Date(as.POSIXct(df$timestamp, origin="1970-01-01"))
      df$hora <- as.POSIXct(trunc(df$timestamp, "hours"))
      df$minuto <-  as.POSIXct(trunc(df$timestamp, "mins"))
      df <- df %>% 
        add_column(query = hashtag)
      
      final <- rbind(final, df)
      
      
      
    
    }
    
    
    end_cursor <- document$graphql$hashtag$edge_hashtag_to_media$page_info$end_cursor
    paginar <- paste0("https://www.instagram.com/explore/tags/", hashtag, "/?__a=1&max_id=", end_cursor)
    
 
    
   
    
  }
  return(final)
}



# Aplicar -----------------------------------------------------------------
# Inserir hashtag - sem usar o caractere #

######## rodar função
hashtag = "politica"
dados <- instaScraper(hashtag = hashtag, n = 3)

# exportar
write.csv2(dados, paste0("instagram_", hashtag,".csv"))



