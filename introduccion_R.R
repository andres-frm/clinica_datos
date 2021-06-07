# ========== introducción a R ====================
# 
#  Menú de opciones de R y RStudio
#
#
#===== Operadores matemáticos y lógicos ========

log(10)

log10(10)

exp(10)

sqrt(10)

pi

5^2

10 + 10

10 - 10

10 * 10

10 / 10

(log(2646) - 
    sqrt(log10(32))) /
    4^52

 x < y # menor q
 x <= y # menor o igual q
 x > y  # mayor q
 x >= y # mayor o igual q
 x == y # igual
 x != y # diferente 
!x #  diferente (negación)
 x | y #  x o y
 x & y #  x y y 

# ============= creando objetos ============== 
curso <- 'clinica datos'
curso

dia <- 1
dia

promedio <- 
  function(x, na.rm = FALSE) {
    if (na.rm == FALSE) {
      print(sum(x) / length(x))
    } else{
      x <- na.omit(x)
      print(sum(x) / length(x))
    }
  }



# ============ estructuras de datos en R =======
# 
# ================ Vectores ============

x <- 1
x
length(x)

x <- 55; x; length(x)

# los objetos pueden tener desde 1 hasta n elementos

x <- c(5, 1, 8, 22, 15021, 0.003) # concatenar c()

y = c(5, 1, 8, 22, 15021, 0.003) 

identical(x, y) # son iguales, pero...

promedio(x, na.rm = FALSE) # = se usa tb en argumentos de funciones.
# entonces mejor usar "<-"

length(x); length(y)

# diferentes clases de vectores 

x
class(x)

y <- c("jkgjh", "5gh54fb", "4h-k")
y; class(y)

z <- as.factor(c("grande", "pequeño",  "mediano", "pequeño", "grande"))
z; class(z)

l <- c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE)
l; class(l)
length(l)
object.size(l)


# indexando vectores

x

x[1]

x[3]

x[1:3]

x[c(1, 3:5)]

x[c(1, 3 , 4, 5)]

x[1, 3:5]

# Agregando o eliminando elementos

x[-5]

x[-c(2:5)]

x[x >= 1 & x < 100]

x >= 1 & x < 100

x <- c(x, 50, 0.5)

# =========== matrices ============

matrix(data = seq(from = 1, to = 60, by = 2), ncol = 5, nrow = 6)

m <- matrix(data = seq(from = 1, to = 60, by = 2), ncol = 5, nrow = 6)

dim(m)
nrow(m)
ncol(m)
length(m)


# indexando matrices 

m[7]

m[1, 2]

m[1:3, c(1:3,5)]

# =================== data frames =====================

library(palmerpenguins)

df <- as.data.frame(penguins_raw)

View(df)

head(df)

#View(df)

str(df)
dim(df)

colnames(df)
(nombres_df <- colnames(df))

# summary(df)

# === cómo extraer vectores e indexar

df[1] # método 1

df[[1]] # método 2

df[, 1] # método 3

df$studyName # método 4

df$Species

# distinto uso dependiendo del código que se esté programando 

# indexación, igual a la de las matrices

df[55, 3]

df[55, c(1:3, 5)]

# extraer subconjuntos 

df1.1 <- df[df$studyName == "PAL0809" &
     df$Species != "Adelie Penguin (Pygoscelis adeliae)", ]

subset(df, df$studyName == "PAL0708" |
         df$studyName == "PAL0809" & df$`Body Mass (g)` >= 4000)


# ===== aplicando funciones a los DF ===============

mean(df$`Body Mass (g)`)

mean(df$`Body Mass (g)`, na.rm = TRUE)

sd(df$`Body Mass (g)`, na.rm = T)

summary(df$`Body Mass (g)`)

sum(df$`Body Mass (g)`, na.rm = T)


# ========= listas ==========

primera_lista <- list(df[1:10, ], 
                        rnorm(5),
                        matrix(rnorm(25), 5, 5))
class(primera_lista)
length(primera_lista)
object.size(primera_lista)

primera_lista # entender aquí la notación para indexar

primera_lista[[1]]


primera_lista[[1]][1:3, c(4, 6:8)]


primera_lista[[2]][2]


# ============ primeros gráficos ==========

plot(iris$Species, iris$Sepal.Length, ylab = "Longitud sépalo", xlab = "Especie")

plot(iris$Petal.Width, iris$Sepal.Length, 
     ylab = "Longitud sépalo", xlab = "Especie", col = "red", pch = 11)

plot(iris$Petal.Width, iris$Sepal.Length, 
     col = ifelse(iris$Species == "setosa", "green",
                  ifelse(iris$Species == "virginica", 
                         "black", "blue")), pch = 19)

plot(density(iris$Sepal.Length), main = "Density plot")


# ================== instalar y cargar paquetes ===========

# install.packages()

install.packages("ggplot2")

library(ggplot2)

require(dplyr)

citation("ggplot2")

citation()


# ================ buscando ayuda ============

?ggplot2
?ggplot
# ??ggplot2
# help(ggplot2)



args(citation)

example(citation)

# ========== para motivar (?) ================
# 

#### manejo de grandes volúmenes de archivos

files <- dir(paste(getwd(),"/coberturas/", sep = ""), 
             pattern = "^([aA1-zZ9]+_[aA1-zZ9]+)*([aA-zZ]+)*\\.xls$")

data_finca <- vector("list", length = length(files))
names(data_finca) <- files

for (i in seq_along(files)) {
  data_finca[[i]] <- data.frame(read_xls(
    paste(paste(getwd(),"/coberturas/", sep = ""), 
          files[[i]], sep = ""), col_names = T))
}

b <- vector("double", length = length(files))
c <- vector("character", length = length(files))

for (i in seq_along(data_finca)) {
  if (nrow(data_finca[[i]]) >= 1) {
    b[[i]] <- sum(data_finca[[i]][2])
    c[[i]] <- names(data_finca[i])
  } else {
    b[[i]] <- 0
    c[[i]] <- names(data_finca[i])
    next()
  }
}

data_cobertura <- data.frame(tarea = b, 
                             cober = c)



###### simulaciones 
lanzamiento_moneda <- function(n) {
  
  moneda <- c("cara", "sello")
  
  trial <- vector("character", length = n)
  
  for (i in 1:n) {
    trial[[i]] <- sample(moneda, 1)
  }
  
  trial <- as.factor(trial)
  table(trial)[1]/n
  
}



trials <- seq(10, 10000, 10)
prop <- vector("double", length = length(trials))

for (i in seq_along(trials)) {
  prop[[i]] <- lanzamiento_moneda(trials[[i]])
}


ensayo <- data.frame(trials, prop)

ggplot2::ggplot(ensayo) + geom_line(aes(x = trials, y = prop)) + 
  ggplot2::geom_hline(yintercept = 0.5, color = "red")


# funciones

mediana <-  function(x, na.rm = FALSE) {
  if (!is.logical(na.rm)) {
    stop("na.rm is not logigal")
  } else if (length(na.rm) != 1) {
    stop("na.rm lenght is not 1")
  }
  if (na.rm == TRUE) {
    x1 <- !is.na(x)
    x1 <- sort(x1)
    if (length(x1) %% 2 != 0) {
      m <- (length(x1) + 1) / 2
      return(x1[m])
    } else {
      n <- (((length(x1) / 2) + ((length(x1) / 2) + 1)) / 2) 
      n1 <- n + 1
      s <- (x1[n] + x1[n1]) / 2
      return(s)
    }
  } else {
    x <- sort(x)
    if (length(x) %% 2 != 0) {
      m <- (length(x) + 1) / 2
      return(x[m])
    } else {
      n <- (((length(x) / 2) + ((length(x) / 2) + 1)) / 2) 
      n1 <- n + 1
      s <- (x[n] + x[n1]) / 2
      return(s)
    }
  } 
  
}

mediana(1:11)
median(1:11)

# figuras 

library(gapminder)
library(ggplot2)
library(gganimate)
library(gifski)

p <- ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent) +
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear')

animate(p, renderer = gifski_renderer())
