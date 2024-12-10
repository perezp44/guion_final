#- guardando ggplot's (para BigData)
#- con ggplot2::ggsave()
#- https://sscc.wisc.edu/sscc/pubs/using-r-plots/saving-plots.html#:~:text=If%20you%20are%20creating%20plots,colleagues%20(PDF%20or%20PNG)


#- PLOTs base ------------------------------------------------------------------
library(tidyverse)

#- plot base -
p <- ggplot(iris, aes(Sepal.Length, Petal.Length, color = Species)) + 
  geom_point()
p

#- segundo plot base (con títulos) -
p_t <- p + labs(title = "Gráfico 1: Longitud del sépalo frente al pétalo",
                subtitle = "(diferenciando por especie de lirio)",
                caption = "Datos provenientes del Iris dataset",
                x = "Longitud del sépalo",
                y = "Longitud del pétalo",
                color = "Especie de lirio",
                tag = "Plot 1")

p_t


#- PREPARANDO los plots --------------------------------------------------------
#- antes de exportar/grabar un plot hay que prepararlo 
#- para que se vea bien en el tamaño que hayamos elegido para su publicación

#- Hay otras formas de hacerlo: 
#- https://ecoevo.social/@noamross/113205497973132730
#- editar tamaño gg sin running the code: https://github.com/wenjie1991/ggfigdone
#- PERO, yo te recomiendo que hagas lo siguiente:

#- para que cuando grabes un plot, se vea bien, has de tener en cuenta que ...
#- los tamaños de los geoms, títulos etc ... están especificados en milímetros, 
#- así que según el tamaño en que guardes el gráfico, puede que no se vean bien

#- Veámoslo con p_t

p_t


#- ¿Qué tengo que hacer? 2 cosas

##- 1) Ajustar el tamaño (I) ------------------
#- tienes que ajustar el tamaño de la ventana de RStudio 
#- hasta tener el tamaño que quieres que tenga el plot en tu publicación

#- si publicas en A4, ten en cuenta que:
#- tamaño de un A4: 21cm (ancho) x 29,7cm (alto)
#- sí quitamos los márgenes, 
#- el ancho de un plot podría estar entre de 13-16 cm
#- el alto el que tú quieras, pero yo creo que entre 6 y 12

#- puedes ver el tamaño de la ventana de RStudio con:
dev.size(units = "cm")

##- 2) Ajustar el tamaño (II) ------------------
#- Una vez tengas en la ventana de RStudio el tamaño que quieres para tu plot,
#- has de cambiar el tamaño de los títulos hasta que el plot se vea bien en RStudio


#- para cambiar los tamaños de títulos puedes 
#- ir cambiando el valor de "my_base_size" hasta que veas bien el plot
my_base_size <- 9

p_t + theme_minimal() + 
  theme(text = element_text(size = my_base_size)) 


#- Cuando ya estés satisfecho/a sobre cómo se ve el plot,
#- solo nos queda exportarlo a un Word, ¿Cómo? Te lo cuento



#- EXPORTANDO ggplot's ---------------------------------------------------------
#- Pues tienes 2 formas de hacerlo

##- A) usando el panel "Plots > Export" de RStudio --------

#- usa "Save as image" para guardar como .png, .jpeg, .tiff, .bmp, .pdf, .svg

#- tb puedes usar "Copy to clipboard" para copiar el plot y pegarlo en un documento
#- (esto no lo he usado nunca)




##- B) con código (ggplot2::ggsave() -------------------------------------------

#- almacenando tamaño del visor de RStudio
my_dev_size <- dev.size(units = "cm")
my_dev_size

#- exportando el plot a un fichero .png
ggplot2::ggsave(filename = "./plots/my_plot.png", 
                #plot = last_plot(), 
                dpi = 300,
                width = my_dev_size[1], 
                height = my_dev_size[2], 
                units = "cm" ) 


#- guardando el plot en formato  SVG (es editable)
ggplot2::ggsave(filename = "./plots/my_plot.svg", 
                #plot = last_plot(), 
                dpi = 300,
                width = my_dev_size[1], 
                height = my_dev_size[2], 
                units = "cm" ) 

                

##- C) exportar directamente a WORD



last_plot()
##- C) exportando directamente a Word o PowerPoint -----------------------------
#- https://ardata-fr.github.io/officeverse/

library(tidyverse)
library(officer)  

###- 1. a Word (.docx) -------------------------

#- definimos/almacenamos el tamaño de mi ggplot

my_dev_size <- dev.size(units = "cm")
my_dev_size


my_anchura_en_cm <- my_dev_size[1]

my_altura_en_cm <- my_dev_size[2]

p_t

#- "creamos" un objeto .docx llamado my_docx
my_docx <- officer::read_docx()

#- añadimos texto y p4
my_docx <- my_docx %>% 
  body_add_par("Gráfico 1: Mi grafiquito") %>% 
  body_add_par(" ") %>% 
  body_add_gg(value = p_t, 
              width = my_anchura_en_cm*0.393701, 
              height = my_altura_en_cm*0.393701, 
              style = "centered")

#- grabamos my_docx en un archivo .docx
print(my_docx, "./plots/my_docx.docx")


##- extra: añadimos una imagen en .png

my_2_docx <- officer::read_docx()

#- añadimos una imagen (previamente grada en un archivo)
my_2_docx <- my_2_docx %>% 
  body_add_img(src = "./plots/p4.png",
               width = 14.1*0.393701, 
               height = 12*0.393701, 
               style = "centered")

#- grabamos my_2_docx en un archivo .docx
print(my_2_docx, "./plots/my_2_docx.docx")





###- 2. a PowerPoint (.pptx) -------------------------
#- https://stackoverflow.com/questions/65197102/best-way-to-export-ggplot2-graphs-to-a-word-documentç
library(tidyverse)
library(officer)

my_plot <- p_t
#- "creamos" un objeto .pptx llamado my_pptx
my_pptx <- officer::read_pptx()

my_pptx %>% 
  #- imagen en formato .png (estática)
  add_slide() %>% ph_with(my_plot, location = ph_location_type(type = "body")) %>% 
  #- svg graph (editable): https://es.wikipedia.org/wiki/Gr%C3%A1ficos_vectoriales_escalables
  add_slide() %>% 
  ph_with(rvg::dml(ggobj = my_plot),
          width = my_anchura_en_cm*0.393701,
          height = my_altura_en_cm*0.393701, 
          location = ph_location_type(type = "body"))

#- exportamos el objeto .pptx a un archivo .pptx
print(my_pptx, "./plots/my_pptx.pptx")



###- 3. a formato EMF ----------------------------------------------------------------
#- https://www.online-convert.com/es/formato-de-archivo/emf

my_plot <- p_t

library(devEMF)
ggsave(file = "./plots/p_t.emf",
       plot = my_plot,
       width = my_anchura_en_cm, 
       height = my_altura_en_cm, 
       units = "cm",
       device = {function(filename, ...) devEMF::emf(file = filename, ...)})




###- 4. a EXCEL ---------------------

#- exportar un ggplot a excel: https://stackoverflow.com/questions/74373120/how-can-i-export-a-ggplot-graph-in-excel-with-writexl-package-in-r
library(openxlsx)


#- iniciamos la creación de un libro de Excel
my_wb <- createWorkbook() 
#- creamos 2 hojas
addWorksheet(my_wb, "Los datos") 
addWorksheet(my_wb, "El plot")

#- guardamos iris en el libro de Excel  llamado "Los datos"
writeData(my_wb, "Los datos", iris)

#- guardamos un ggplot en el libro de Excel llamado "El plot"
p1
insertPlot(my_wb, "El plot")

#- guardamos el libro de Excel en nuestro ordenador
saveWorkbook(my_wb, "./plots/plot.xlsx", overwrite = TRUE)



#- TAMAÑO de los títulos (un ejemplo) ------------------------------------------
##- un ejemplo más detallado ------------

#- como sabes, en realidad, se puede tunear cada elemento de texto

my_base_size <- 11

my_size_theme <- theme(
  plot.title = element_text(size = my_base_size),
  plot.subtitle = element_text(size = my_base_size*0.8),
  plot.caption = element_text(size = my_base_size*0.6),
  plot.tag = element_text(size = my_base_size*0.6),
  #- titulo de los ejes
  axis.title.x = element_text(size = my_base_size*0.5),
  axis.title.y = element_text(size = my_base_size*0.5),
  #- las marcas de los ejes
  axis.text.x = element_text(size = my_base_size*0.5),
  axis.text.y = element_text(size = my_base_size*0.5),
  #- el grosor de los ejes
  axis.line = element_line(size = 0),
  #- posición leyenda
  # legend.position = "none",
  #- leyenda
  legend.title = element_text(size = my_base_size*0.5),
  legend.text = element_text(size = my_base_size*0.5))



p_t + theme_minimal() + my_size_theme




#- almacenando tamaño del dispositivo
my_dev_size <- dev.size(units = "cm")
my_dev_size


#- con código ggplot2::ggsave() ---
ggplot2::ggsave(filename = "./plots/my_plot_99.png", 
                #plot = last_plot(), 
                dpi = 300,
                width = my_dev_size[1], 
                height = my_dev_size[2], 
                units = "cm" ) 


###- otro ejemplito: ---------------------
#- vamos a guardar una composición de 4 plots

my_size_theme <- theme(
  plot.title = element_text(size = 6),
  plot.subtitle = element_text(size = 5),
  plot.caption = element_text(size = 5),
  plot.tag = element_text(size = 5),
  #- titulo de los ejes
  axis.title.x = element_text(size = 4),
  axis.title.y = element_text(size = 4),
  #- las marcas de los ejes
  axis.text.x = element_text(size = 4),
  axis.text.y = element_text(size = 4),
  #- el grosor de los ejes
  axis.line = element_line(size = 0.5),
  #- leyenda
  legend.title = element_text(size = 4),
  legend.text = element_text(size = 4),
  legend.position = "none" )


p_t_size <- p_t + my_size_theme

library(patchwork)
p4 <- (p_t_size + p_t_size) / (p_t_size + p_t_size)
p4

#- guardándolo con un tamaño definido
ggsave("./plots/p4.png", plot = p4, width = 14.1, height = 12, units = "cm")

#- guardando lo en formato  SVG (es editable)
ggsave("./plots/p4.svg", plot = p4, width = 14.1, height = 12, units = "cm")

#- guardándolo ajustando el tamaño de la ventana de RStudio
#- hayque dejarla en el tamaño que queremos que tenga el plot
#- queremos: 14,1 cm de ancho y 12 cm de alto
#- viendo el tamaño del dispositivo

dev.size()
dev.size(units = "cm")

ggsave("./plots/p4x.png", plot = p4)




