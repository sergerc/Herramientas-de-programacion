#-------------------------------------------------------------------------------
# PRACTICA FINAL. HERRAMIENTAS DE PROGRAMACIÓN.
#-------------------------------------------------------------------------------
# PRACTICA EN GRUPO.
# ALUMNO: SERGIO RANZ CASADO.
# ALUMNO: MARCOS MEDINA COGOLLUDO.
#-------------------------------------------------------------------------------
# FUNCIONAMIENTO DEL SCRIPT: 
# # -> Enunciado del ejercicio.
# ### -> Comentario del alumno. 
#-------------------------------------------------------------------------------
# ENUNCIADO.
# You are preparing a dataset for a posterior Marketing Mix model. The main goal
# of MMM is quantifying the effect of advertising and promotions on the sales of
# a product. Here we will be working with anonymous data of a unknown product 
# and its weekly sales during two years. Provide a code in R for preparing the 
# data, as explained below. The effectiveness of the code, as well as its 
# cleanness, will be considered for the evaluation. Comment everything you 
# consider worth it with ###.
#
#-------------------------------------------------------------------------------
### Librarys: 
#-------------------------------------------------------------------------------
rm(list=ls())

library(readr)
library(janitor)
library(dplyr)
library (ggplot2)
library(tidyr)
library(purrr)



#-------------------------------------------------------------------------------
# EJERCICIO 1.
# Create a data frame called df_mmm from the csv file mktmix.csv. 
# Use janitor  for change the column names into something meeting the tidyverse 
# guidelines.
#-------------------------------------------------------------------------------

### Para poder leer de manera mas rapida y trabajar de manera mas eficiente, 
### hemos creado un proyecto al que llamamos "proyecto1". de esta manera no
### tendremos que invocar la función setwd para indicar al ordenador donde 
### estamos. Usaremos la libreria readr (a la que llamamos en el apartado
### inicial de librerias) para leer el csv mktmix, "read_csv". Para limpiar
### los nombres del df, usamos "clean_names" de la libreria janitor (la 
### llamamos en el apartado librarys al principio). 

df_mmm <- read_csv("mktmix.csv", na = c("", "NA", "NaN"))
df_mmm <- clean_names(df_mmm)

#-------------------------------------------------------------------------------
#EJERCICIO 2. 
# How many columns are there? And rows? What are the classes of base_price and 
# discount? Try and guess what they mean.
#-------------------------------------------------------------------------------

ncol(df_mmm)
nrow(df_mmm)
### Son 104 filas(rows) y 9 columnas. Tambien lo podriamos haber sacado con
### dim(df_mmm)

class(df_mmm$base_price)
class(df_mmm$discount)
### Ambos son clase numerica. Entendemos que el base_price es el precio base
### del producto, a algunos de estos productos se les disminuirá el precio base
### con la columna discount. Que es el descuento que se le aplica a diversos
### productos. 
#-------------------------------------------------------------------------------

# EJERCICIO 3. newspaper_inserts’s class is character. Change its values so that
# it can be numeric. All the NA values should be 0; the rest of them should be
# 1.
#-------------------------------------------------------------------------------

### Para convertir la columna a string, tenemos que mutar los datos de las
### columnas. Para ello usamos dplyr (Lo cargamos en la zona de librerias). 
### Usamos un if_else como condicional para, que nos devuelva un valor y otro
### en función de la condición establecida. 

df_mmm <- df_mmm %>% 
  mutate(newspaper_inserts = if_else(is.na(newspaper_inserts),0,1))

class(df_mmm$newspaper_inserts)

#-------------------------------------------------------------------------------

# How many different values are there in the website_campaign column (NA doesn’t
# count)? Create new columns for each of these categories, defined as 1 if 
# website_campaign equals this category, 0 in other cases. For instance, if 
# website_campaign equals "Google" on the 10th row, then you will create
# another column called Google that will equal 1 on the 10th row and will equal 
# 0 on the rest.   
#-------------------------------------------------------------------------------
 
unique(df_mmm$website_campaign)

### Podemos ver que hay 4 valores, R tiene en cuenta los valores NA como un tipo
### de valor. 


df_mmm <- df_mmm %>% 
  mutate(Facebook = if_else(website_campaign == "Facebook",1,0),
         Twitter = if_else(website_campaign == "Twitter",1,0),
         website_campaign = if_else(website_campaign == "Website Campaign",1,0))

df_mmm <- df_mmm %>% 
  mutate(
    Facebook = if_else(is.na(Facebook), 0, Facebook), 
    Twitter = if_else(is.na(Twitter), 0, Twitter), 
    website_campaign = if_else(is.na(website_campaign), 0, website_campaign)
  )

### Para alterar las columnas y crear nuevas utilizamos dplyr(mutate). Teniendo en 
### cuenta que sigue habiendo valores negativos, los cuales eliminamos de nuevo
### empleando el condicional if_else.

#-------------------------------------------------------------------------------
#
# Create a line plot with ggplot2 showing the evolution of new_vol_sales, which 
# would be the target variable in a model. Since we haven’t been provided with 
# dates, you will have to invent an x axis (it can be just numbers from 1 and 
# so on).
#-------------------------------------------------------------------------------

### Nos hemos inventado el eje X, y le hemos dado los valores de las ocurrencias
### de datos. Es decir, x = 1:104. 
### Para hacer el grafico utilizamos la libreria ggplot2 que cargamos en el 
### apartado de librerias. 
### En el grafico observamos una gran variabilidad en el volumen de ventas. Y no
### tenemos claro el patron que sigue. Si es cierto que el grueso de los valores 
### se posicionan entre los 19K y 23K. Pero no se identifica una media clara. 


ggplot(df_mmm) + 
  geom_line(aes(x = c(1:104), y = new_vol_sales), colour = "darkorange1", size = 1) +
  theme_grey() +
  labs(x = " ", y = "Volumen de Ventas") + 
  ggtitle("GRAFICO NEW VOLUME SALES") +
  theme(plot.title = element_text( hjust = 0.5 , vjust = 1, face = "bold", colour = "darkorange1", size = 15),
        axis.title.y = element_text(hjust = 0, size = 8, colour = "darkorange1", face = "bold"))

#-------------------------------------------------------------------------------
#
# Create a histogram and a boxplot of the same variable. Based on the plots, 
# which is the median of the variable? Calculate it with an R function. Were you
# close?
#-------------------------------------------------------------------------------

ggplot(df_mmm) +
  geom_histogram(aes(x = new_vol_sales), col = "white", fill = "darkorange1", alpha = 0.7) +
  theme_classic() +
  labs(x = "New Volume Sales", y = " ")  +
  ggtitle("HISTOGRAMA NEW VOLUME SALES") + 
  theme(plot.title = element_text(hjust = 0, vjust = 1, face = "bold", colour = "darkorange1", size = 15),
        axis.title.x = element_text(vjust = 1, size = 10, colour = "darkorange1", face = "bold"))

### Con este histograma, si que podemos determinar cual es el valor que mas se repite.
### tal y como hemos confirmado con la grafica anterior, el grueso de datos se situa
### entre 19K y 21K. Tmabien nos encontramos con valores representativos en el extremo
### derecho. 

ggplot(df_mmm) +
  geom_boxplot(aes(y = new_vol_sales), fill = "darkorange1", size = 1, alpha = 0.8)+
  theme_minimal()+
  labs(y = "New Volume Sales") +
  ggtitle("BOXPLOT NEW VOLUME SALES") +
  theme(plot.title = element_text(hjust = 0, vjust = 1, face = "bold", colour = "darkorange1", size = 15),
        axis.title.y = element_text(hjust = 0, size = 8, colour = "darkorange1", face = "bold"))

### Tal y como hemos comentado con los graficos anteriores, la media se va a situar
### entre los valores 19k y 21k. Estando situada la mediana por debajo de 20k
### Podemos ver, los  valores anomalos en el extremo superior, que se corresponden
### con los valores representativos del extremo derecho del histograma. 
### Sin embargo, por abajo no parece que haya ningun valor anómalo.

mean(df_mmm$new_vol_sales) 

### La media del volumen de ventas es 20.171,07 por lo que SI hemos estado cerca con
### nuestra apreciación anterior del gráfico. 

  
#-------------------------------------------------------------------------------
#
# Select only the media investment columns: tv, radio and stout, and create a 
# new data frame just with them. Use this data frame and the provided code for
# creating a plot with the evolution of these three columns. This should be a 
# plot in an only figure, with a share x axis but different y axis (see the 
# result). For using the provided code, suppose the data frame you created with 
# just the media data is called df_media. Is there anything worth mentioning 
# from the plot?
#-------------------------------------------------------------------------------

### Usamos la libreria dplyr (anteriormente cargada) para filtrar y seleccionar
### las 3 columnas indicadas. Posteriormente para el grafico, usaremos ggplot2.

df_media <- df_mmm %>% 
  select(tv,radio,stout)

### Hay un problema al crear el grafico. La longuitud de radio es mas corta 
### porque tiene valores NA. Usamos mutate de la libreria dplyr. Nuestra intención
### no es eliminar dichos valores, sino cambiarlos por 0 para que asi si cumplan las 
### 3 columnas con la misma dimensión. 

df_media <- df_media %>%
  mutate(radio = if_else(is.na(radio),0, radio))

df_media <- df_media %>%
  pivot_longer(everything())

  ggplot(df_media) +
  geom_line(aes(x = 1:nrow(df_media), y = value, col = name)) +
  facet_grid(name ~ . , scales = "free_y") + 
  labs(y = "Euros", x = "") +
  theme_gray()+
  ggtitle("INVERSION RADIO, TV, SCOUT") +
  theme(plot.title = element_text(hjust = 0, vjust = 1, face = "bold", colour = "darkorange1", size = 15),
        axis.title.y = element_text(hjust = 0, size = 8, colour = "darkorange1", face = "bold"))

### Podemos observar las caidas repentinas sobre todo en el grafico de líneas de la 
### radio. Eso es debido a los valores NA que hemos cambiado por 0. No obstante, sin
### tener en cuenta los datos nulos, podemos observar una linea de tendencia a lo largo del 
### periodo. 
#-------------------------------------------------------------------------------
#
# in_store is an index of the stock available on stores for selling the product.
# Create a scatter plot with ggplot2 for comparing the new_vol_sales column 
# against in_store. Choose carefully which column should be set on the x axis 
# and which on the y, considering that new_vol_sales will be the target variable
# on a model, i.e., the analyst will want to explain this variable based on the 
# rest of the information. Explain your decision and also comment anything 
# interesting from the plot. For doing this, think about the relation that could 
# exist between the stock of a product and its sales.
#
#-------------------------------------------------------------------------------

### Tal y como nosotros lo hemos planteado. La variable volumen de ventas va a 
### depender siempre del volumen de stock que haya en la tienda. El stock en 
### este caso, sera una varible independiente. Es por ello que nos hemos decidido
### a meter el valor de new_volume_sales en el eje y el stock en el eje x,
  

ggplot(df_mmm) +
    geom_point(aes(x = in_store, y = new_vol_sales), col = "darkorange1", size = 2)+
    theme_classic() + 
    ggtitle("SCATTER PLOT") +
    labs(x = "Stock en tienda", y = "Volumen de ventas") + 
    theme(plot.title = element_text(hjust = 0, vjust = 1, face = "bold", colour = "darkorange1", size = 15),
          axis.title.x = element_text(color = "darkorange1"),
          axis.title.y = element_text(color = "darkorange1"))

### Una vez creado el grafico podemos ver como entre las dos variables existe una
### relacción directamente proporcional. Esto quiere decir que, a mayor nivel de 
### stock va a haber un mayor volumen de ventas.
### Otra manera de ver el grafico sería desde un punto de vista estrategico. 
### Parece logico pensar que, a mayor volumen de ventas en una tienda determinada,
### mayor volumen de stock dicha tienda va a requerir en un futuro. 

#-------------------------------------------------------------------------------
#
# Create two different versions of the previous plot:
#     - Color each dot differently based on the newspaper_inserts column 
#      (using as.factor() here is recommended).
#     - Color each dot differently based on the tv column.
#-------------------------------------------------------------------------------

ggplot(df_mmm) +
  geom_point(aes(x = in_store, y = new_vol_sales, colour = as.factor(newspaper_inserts)))+
  theme_classic() + 
  ggtitle("SCATTER PLOT") +
  labs(x = "Stock en tienda ", y = "Volumen de Ventas") + 
  theme(plot.title = element_text(hjust = 0, vjust = 1, face = "bold", colour = "darkorange1", size = 15),
        axis.title.x = element_text(color = "darkorange1"),
        axis.title.y = element_text(color = "darkorange1"))

ggplot(df_mmm) +
  geom_point(aes(x = in_store, y = new_vol_sales,  colour = tv))+
  theme_classic() + 
  ggtitle("SCATTER PLOT") +
  labs(x = "Stock en tienda ", y = "Volumen de Ventas") + 
  theme(plot.title = element_text(hjust = 0, vjust = 1, face = "bold", colour = "darkorange1", size = 15),
        axis.title.x = element_text(color = "darkorange1"),
        axis.title.y = element_text(color = "darkorange1"))

### Con estos dos graficos podemos ver como la inversion en periodicos y tv parece
### que no tienen una relacción directa con el volumen de ventas de las tiendas.
### Si hubiera sido de diferente manera. Entiendo que en el caso de la tv, los puntos
### mas oscuros estarían en valores mas bajos mientras que los puntos mas claros en valores 
### mas altos. Pero eso no es así. Con respecto a la inversión del periodico. 
### Los puntos azules, estarían en los valores superiores, sin embargo
### se reparten por todo el grafico. 

#-------------------------------------------------------------------------------
#
# Create another column on the data frame indicating whether a discount has been
# applied or not. You can name discount_yesno, for example. The column can be 
# numerical or logical.. After that, create another data frame aggregating the
# original one, for calculating the average base price when there’s a discount 
# and where it isn’t. Use this data frame for creating a column plot with ggplot.
# On the x axis you should use the discount_yesno values and on the y axis, the 
# average price you calculated.
# Are the any significant differences? Remark. Try not to create new data frames
# nor overwriting the original one, but chain all the operations with %>%, 
# including the plot.
#-------------------------------------------------------------------------------

df_mmm %>% 
  mutate(discount_yesno = if_else(discount == 0, FALSE, TRUE)) %>% 
  group_by(discount_yesno) %>% 
  summarise(average_base_price = mean(base_price)) %>% 
  ggplot() +
  geom_bar(aes(x = discount_yesno, y = average_base_price, fill= as.factor(discount_yesno)) , stat = "identity")+
  theme_classic() + 
  ggtitle("Base Price vs Discount") +
  labs(x = "Precio Base ", y = " ") + 
  theme(plot.title = element_text(hjust = 0, vjust = 1, face = "bold", colour = "darkorange1", size = 15),
        axis.title.x = element_text(color = "darkorange1"),
        axis.title.y = element_text(color = "darkorange1"))

### Como se aprecia en el gráfico. No hay diferencias significativas en el precio. 

#-------------------------------------------------------------------------------
#
# Create a function that fits a model on this dataset using the provided code. 
# The idea of the function is selecting a subset of columns on the data frame, 
# creating a data frame with this selection, using the data frame on a model and
# returning a number that will indicate how good the model is. So the input
# will be a character, that will be the names of the columns, and the output 
# will be this fitness number, provided in the next piece of code. You have been
# asked to create an auxiliary data frame with the selection of columns: for 
# this piece of code we are assuming you’ve called the data frame df_aux but you
# can name it as you want and change the code.
#-------------------------------------------------------------------------------

### Para realizar la función, asignamos a una variable, el nombre de la columnas
### o columnas que nos han indicado, además, incluimos la columna new_vol_sales,
### sobre la que vamos a hacer la comparación. 

model_fit <- function(colnombre){
  df_aux <- df_mmm %>% 
    select(new_vol_sales, all_of(colnombre))
  my_model <- lm(new_vol_sales ~ ., data = df_aux)
  return(summary(my_model)$adj.r.squared)
}

model_fit("in_store")
#-------------------------------------------------------------------------------
#
# You are given three sets of variables. Create a list whose elements will be 
# these three vector. Now, using map_dbl() or sapply(), call the function you 
# created in the previous exercise for the three cases. Which of the three 
# subsets provide the best model, bearing in mind that the larger the returned
# number, the better?
# - c("base_price", "radio", "tv", "stout")
# - c("base_price", "in_store", "discount", "radio", "tv", "stout")
# - c("in_store", "discount")
#-------------------------------------------------------------------------------

lista_variables2 <- list(c1 = c("base_price", "radio", "tv", "stout"), 
                         c2 = c("base_price", "in_store", "discount", "radio", "tv", "stout"),
                         c3 = c("in_store", "discount"))


lapply(lista_variables2,model_fit)

### El vector con mayor valor es el C2. Basicamente por las variables base_price y
### in_store que son las que mas relacción guardan con new_vol_sales. Las dos por separado
### suponen un 0.52 y un 0.18 respectivamente, que sumadas hacen practicamente el 90%. 

#-------------------------------------------------------------------------------

