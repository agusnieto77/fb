#Loading magick package
library(magick)

#reading a png image
(img2 <- image_read('https://scontent.faep9-1.fna.fbcdn.net/v/t1.0-0/p235x165/150564725_700237940620309_102173776589240521_n.jpg?_nc_cat=109&ccb=3&_nc_sid=1480c5&_nc_ohc=-qlaajkPtsYAX8nr-Dg&_nc_ht=scontent.faep9-1.fna&tp=6&oh=78ad1151cbf47b92f3ccf590bf0e68a9&oe=605A45FB'))

image_info(img2)

print(img2)

image_write(img2, path = 'img2.svg', format = 'svg')

#Applying Charcoal effect to Obama's image 
#and compositing it with frink's image
#and finally annotating it with a text
image_charcoal(img2) %>% 
  image_composite(img2)  %>%
  image_annotate("CONFIDENTIAL", size = 20, color = "red", boxcolor = "pink",
                 degrees = 30, location = "+100+100") %>%
  image_rotate(30) %>%
  image_write('img2_with_img2.png','png')

img2svg <- image_read("img2.svg")

#########################################################

imagenes_links <- comentarios_fb_limpios %>% filter(!is.na(post_img)) %>% select(post_img) %>% as_vector()

for (i in imagenes_links) {
  image_read(i) %>% 
    image_write(path = paste0('./fotos_fb/',str_extract(i, "........$"),'.jpeg'))
}
