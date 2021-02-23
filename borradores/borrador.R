library(rvest)
#> Loading required package: xml2
library(magrittr)
"https://www.lacapitalmdp.com/el-joven-cordobes-que-dio-positivo-de-coronavirus-confiesa-a-la-capital-vine-a-ver-a-mi-novia-me-mande-una-macana-y-ahora-tengo-miedo/" %>%
  read_html() %>%
  html_nodes(".comments") %>%
  html_attr("src") %>% 
  read_html() %>% 
  html_node("#searchResultsTable") %>% 
  html_table() %>%
  head()

coments <- "https://www.lacapitalmdp.com/el-joven-cordobes-que-dio-positivo-de-coronavirus-confiesa-a-la-capital-vine-a-ver-a-mi-novia-me-mande-una-macana-y-ahora-tengo-miedo/" %>%
  read_html() %>%
  html_nodes("#comentarios.comments") %>% 
  html_nodes(".fb-comments.fb_iframe_widget.fb_iframe_widget_fluid_desktop") %>%
  html_nodes("fb_iframe_widget") %>%
  html_nodes(".fb-comments.fb_iframe_widget.fb_iframe_widget_fluid_desktop") %>%
  as.character()
  html_elements("fb-iframe-plugin-query")
  html_nodes("fb-iframe-plugin-query") %>%
  html_attr("src")

class(coments[[1]])

str_detect(coments[[1]], "iframe")

estructura <- "https://www.lacapitalmdp.com/el-joven-cordobes-que-dio-positivo-de-coronavirus-confiesa-a-la-capital-vine-a-ver-a-mi-novia-me-mande-una-macana-y-ahora-tengo-miedo/" %>%
  read_html() %>% as.character() %>% as_tibble()

estructura %>% str_view_all(estructura$value, 'iframe')

ffd$navigate("https://www.lacapitalmdp.com/ya-aplican-la-vacuna-en-escuelas-como-es-el-proceso-desde-el-turno-hasta-la-dosis/")
fb <-  ffd$getPageSource()[[1]] %>% read_html()
post_img =  html_nodes(fb, "div._3-8m") %>% html_attr("href")
