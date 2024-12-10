#- https://stability.ai/
#- imágenes desde R con Stability: https://github.com/schochastics/stabilityAI

pak::pak("schochastics/stabilityAI")

#- vamos a: https://platform.stability.ai/
#- continuamos con nuestra Google account  y aceptamos
#- se nos creará una API key. La copias: xx-sbbbbbsbsbbsbbsbsbbsbsbbsbsbsbbsbsbsbbsbsbsbsbU

usethis::edit_r_environ()  #- has de poner:
#- STABILITYAI_TOKEN="tu-API-key"


#- ya podemos usar Stability desde R
library(stabilityAI)
stabilityAI::get_engines_list()
stabilityAI::generate_txt2img()

# img <- stabilityAI::generate_txt2img(
#   text_prompts = "A dude with no hair and a beard sitting in front of his laptop in a dark room",
#   style_preset = "pixel-art")


my_IA_img <- stabilityAI::generate_txt2img(
  text_prompts = "un campo de ababoles rojos con el sol al fondo",
  #style_preset = "pixel-art"
  #- https://platform.stability.ai/docs/api-reference#tag/v1generation/operation/textToImage
)



# API returns the image base64 encoded. Save it as png with
stabilityAI::base64_to_img(my_IA_img ,"./imagenes/my_IA_img_5")
