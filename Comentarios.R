############---Librerias----############
require(RSelenium)
require(rvest)
require(lubridate)
require(V8)
require(XML)
require(xml2)
require(wdman)
require(magrittr)
require(dplyr)
require(purrr)
require(tibble)
require(stringr)
require(tidyr)

################# links ############

vector_links <- c("https://www.lacapitalmdp.com/luis-brandoni-esta-internado-con-coronavirus/",
                  "https://www.lacapitalmdp.com/despues-de-once-meses-los-alumnos-volvieron-a-las-escuelas/",
                  "https://www.lacapitalmdp.com/segunda-semana-con-descenso-en-los-contagios-de-coronavirus/",
                  "https://www.lacapitalmdp.com/gines-gonzalez-garcia-estamos-empezando-un-ritmo-vacunatorio-mas-acelerado/",
                  "https://www.lacapitalmdp.com/confirman-145-nuevos-casos-de-coronavirus-en-mar-del-plata/",
                  "https://www.lacapitalmdp.com/detallan-que-trabajadores-estaran-exceptuados-de-concurrir-a-las-aulas/",
                  "https://www.lacapitalmdp.com/el-cema-dos-polideportivos-y-salas-municipales-funcionaran-como-centros-de-vacunacion/",
                  "https://www.lacapitalmdp.com/coronavirus-se-sumaron-diez-muertes-y-171-casos-en-mar-del-plata/",
                  "https://www.lacapitalmdp.com/kicillof-brindara-una-conferencia-de-prensa-en-miramar-tras-analizar-la-situacion-epidemiologica-de-la-provincia/",
                  "https://www.lacapitalmdp.com/ya-aplican-la-vacuna-en-escuelas-como-es-el-proceso-desde-el-turno-hasta-la-dosis/")

# preparar links
links_comentarios_fb <- str_remove_all(vector_links,"https://www.lacapitalmdp.com/")
links_comentarios_fb <- str_remove_all(links_comentarios_fb,"/")
links_coment_lc <- paste0("https://www.facebook.com/v2.5/plugins/comments.php?app_id=551960851645342&channel=https%3A%2F%2Fstaticxx.facebook.com%2Fx%2Fconnect%2Fxd_arbiter%2F%3Fversion%3D46%23cb%3Df30998e135adef4%26domain%3Dwww.lacapitalmdp.com%26origin%3Dhttps%253A%252F%252Fwww.lacapitalmdp.com%252Ff171cd682eab44%26relation%3Dparent.parent&container_width=1203&height=100&href=https%3A%2F%2Fwww.lacapitalmdp.com%2F",links_comentarios_fb,"%2F&locale=es_LA&numposts=100&sdk=joey&version=v2.5&width=")

# Usamos Rselenium
rd <- rsDriver(browser = "firefox", port = 1234L)
ffd <- rd$client

# creamos la funcion
leer_fb <- function (x){
  tryCatch({
    ffd$navigate(x)
    Sys.sleep(2)
    ffd$findElement("class name", "_2pir")$clickElement()
    ffd$findElement("class name", "_2pir")$clickElement()
    fb <-  ffd$getPageSource()[[1]] %>% read_html()
    tibble(
      fecha = html_nodes(fb, ".UFISutroCommentTimestamp.livetimestamp") %>% html_text(),
      utime = html_nodes(fb, ".UFISutroCommentTimestamp.livetimestamp") %>% html_attr("data-utime"),
      autor = html_nodes(fb, ".UFICommentActorName") %>% html_text(),
      autor_fb = html_nodes(fb, ".UFICommentActorName") %>% html_attr("href"),
      post = html_nodes(fb, "div._30o4") %>% html_text(),
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
saveRDS("comentarios_fb.rds")

############### limpiamos y normalizamos los datos
# limpiamos
comentarios_fb_limpios <- comentarios_fb %>% 
  mutate(fecha_posixct = as.POSIXct(as.integer(utime), origin="1970-01-01")) %>% 
  select(9,1:8) %>% 
  mutate(megustas = case_when(str_detect(megustas, '· \\d{1,3} ·') ~ stringr::str_extract(megustas,'· \\d{1,3} ·'), TRUE ~ "0" )) %>% 
  mutate(megustas = case_when(str_detect(megustas, '\\d{1,3}') ~ stringr::str_extract(megustas,'\\d{1,3}'), TRUE ~ "0" )) %>% 
  mutate(megustas = as.integer(megustas))

# guardamos los comentarios normalizados
saveRDS(comentarios_fb_limpios,"comentarios_fb_limpios.rds")
