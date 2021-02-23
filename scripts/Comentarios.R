############---Paquetes----############

require(RSelenium)
require(rvest)
require(lubridate)
require(magrittr)
require(dplyr)
require(purrr)
require(tibble)
require(stringr)
require(magick)

#################---links---############

vector_links <- readRDS('./data/LC_notas_covid_ciudad_2020.rds') %>%  select(link) %>% as_vector() %>% .[1000:1005]

# preparar links
links_comentarios_fb <- str_remove_all(vector_links,"https://www.lacapitalmdp.com/")
link_parte_1 <- "https://www.facebook.com/v2.5/plugins/comments.php?app_id=551960851645342&channel=https%3A%2F%2Fstaticxx.facebook.com%2Fx%2Fconnect%2Fxd_arbiter%2F%3Fversion%3D46%23cb%3Df30998e135adef4%26domain%3Dwww.lacapitalmdp.com%26origin%3Dhttps%253A%252F%252Fwww.lacapitalmdp.com%252Ff171cd682eab44%26relation%3Dparent.parent&container_width=1203&height=100&href=https%3A%2F%2Fwww.lacapitalmdp.com%2F"
link_parte_3 <- "&locale=es_LA&numposts=100&sdk=joey&version=v2.5&width="
links_coment_lc <- paste0(link_parte_1, links_comentarios_fb, link_parte_3)

# Usamos RSelenium
rd <- rsDriver(browser = "firefox", port = 2805L)
ffd <- rd$client

# creamos la funcion
leer_fb <- function (x){
  tryCatch({
    ffd$navigate(x)
    Sys.sleep(2)
    ffd$getPageSource()[[1]]
    n <- ffd$findElement("class name", "_50f7")$getElementText()[[1]] %>% str_remove(" comentarios") %>% as.integer() / 10
    n <- if( n < 11 | is.na(n) ) {
      0
    } else if( n >= 11 & n < 21 ) {
      1
    } else if( n >= 21 & n < 31 ) {
      2
    } else if( n >= 31 & n < 41 ) {
      3
    } else if( n >= 41 & n < 51 ) {
      4
    } else if( n >= 51 & n < 61 ) {
      5
    } else if( n >= 61 & n < 71 ) {
      6
    } else if( n >= 71 & n < 81 ) {
      7
    } else if( n >= 81 & n < 91 ) {
      8
    } else if( n >= 91 & n < 101 ) {
      9
    } else if( n >= 101 & n < 111 ) {
      10
    }
    Sys.sleep(2)
    if(n > 0){
      for (i in 1:n){
        print(i)
        ffd$findElement("class name", "_1gl3")$clickElement()
        Sys.sleep(3.5)
      }
    }
    Sys.sleep(2)
    masrespuestas <- ffd$findElements("class name", "_2pir")
    n <- length(masrespuestas)
    if(n > 0){
      for (i in 1:n){
        print(i)
        ffd$findElement("class name", "_2pir")$clickElement()
        Sys.sleep(3.5)
      }
    }
    Sys.sleep(2)
    leermas <- ffd$findElements("class name", "_5v47")
    n <- length(leermas)
    if(n > 0){
      for (i in 1:n){
        print(i)
        ffd$findElement("class name", "_5v47")$clickElement()
        Sys.sleep(3.5)
      }
    }
    fb <-  ffd$getPageSource()[[1]] %>% read_html()
    tibble(
      fecha = html_nodes(fb, ".UFISutroCommentTimestamp.livetimestamp") %>% html_text(),
      utime = html_nodes(fb, ".UFISutroCommentTimestamp.livetimestamp") %>% html_attr("data-utime"),
      autor = html_nodes(fb, ".UFICommentActorName") %>% html_text(),
      autor_fb = html_nodes(fb, ".UFICommentActorName") %>% html_attr("href"),
      post_char = html_nodes(fb, "._30o4") %>% as.character(),
      post = html_nodes(fb, "div._30o4") %>% html_text(),
      post_img = str_match(post_char, 'data-ploi="(.*?)" class=') %>% .[,2] %>% str_remove_all("amp;") %>% str_remove_all('" data-pl.*'),
      megustas = html_nodes(fb, "._2vq9.fsm.fwn.fcg") %>% html_text(),
      bajado = Sys.time(),
      nota = x
    )
  }, error = function(e){
    NULL
  })
} 

# aplicamos la funcion
comentarios_fb <- map_df(links_coment_lc, leer_fb)

# guardamos los comentarios
saveRDS(comentarios_fb,"./data/comentarios_fb.rds")

############### limpiamos y normalizamos los datos
# limpiamos
comentarios_fb_limpios <- comentarios_fb %>% 
  mutate(fecha_posixct = as.POSIXct(as.integer(utime), origin="1970-01-01")) %>% 
  select(11,1:10) %>% 
  mutate(megustas = case_when(str_detect(megustas, '· \\d{1,3} ·') ~ stringr::str_extract(megustas,'· \\d{1,3} ·'), TRUE ~ "0" )) %>% 
  mutate(megustas = case_when(str_detect(megustas, '\\d{1,3}') ~ stringr::str_extract(megustas,'\\d{1,3}'), TRUE ~ "0" )) %>% 
  mutate(megustas = as.integer(megustas)) %>% 
  mutate(id_img = str_extract(post_img, '....................\\.jpg'))

# guardamos los comentarios normalizados
saveRDS(comentarios_fb_limpios,"./data/comentarios_fb_limpios.rds")

# guardamos las imágenes y memes publicados en los comentarios

imagenes_links <- comentarios_fb_limpios %>% filter(!is.na(post_img)) %>% select(post_img) %>% as_vector()

for (i in imagenes_links) {
  image_read(i) %>% 
    image_write(path = paste0('./fotos_fb/',str_extract(i, "....................\\.jpg")))
}

# Fin -- Hay mucho por mejorar, se aceptan sugerencias 

