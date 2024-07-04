#Limpiar el entorno
rm(list = ls())

#Llamar a la base de datos

library(readxl)
file.choose()

notas_2023<- read_excel(path = "/Users/mirian/Library/CloudStorage/OneDrive-Personal/MAGISTER EN DATA SCIENCE/4. Programacion en R/Catedra 1/nomina_calificaciones2023_Mirian Astete.xlsx", 
                              sheet = 1)
names(notas_2023)

promedios_2023<- read_excel(path = "/Users/mirian/Library/CloudStorage/OneDrive-Personal/MAGISTER EN DATA SCIENCE/4. Programacion en R/Catedra 1/nomina_calificaciones2023_Mirian Astete.xlsx", 
                              sheet = 2)
names(promedios_2023)



#Se le otorga un nombre a la base de datos
#hoja numero 1 del archivo excel
n1 <- notas_2023
str(n1)

#Hoja numero 2 del archivo excel
p1 <- promedios_2023
str(p1)

filter(p1 , p1$`Promedio Final`> 0)

#Llamar al paquete con el que se trabajará

library(tidyverse)
library(dplyr)

#Seleccionar las variables a analizar y visualizacion mediante la funcion names
n1 <-n1[ , c(3,10,12,14,17)]
names(n1)

#Se filtran los datos en base a los diferentes niveles educativos

#Filtro para primero medio
primero <- n1 %>% 
  filter ( n1$`Cod Grado` == 1)
view(primero)

#Ordenar datos de nivel educativo y asiganturas por columnas
C1<- primero %>%
  pivot_wider(names_from = `Desc Subsector`, 
              values_from = Calificación)
view(C1)

#Filtrado de datos por letra curso
#Primero medio
#Primero "A"
C1A <- C1 %>% 
  filter ( C1$`Letra Curso` == "A")
view(C1A)

#Estadistica descriptiva asiganturas de interes de 1roA
library(skimr)

skim(C1A$`LENGUA Y LITERATURA`)
skim(C1A$MATEMÁTICA)
skim(C1A$`HISTORIA, GEOGRAFÍA Y CIENCIAS SOCIALES`)
skim(C1A$`CIENCIAS NATURALES`)
skim(C1A$`IDIOMA EXTRANJERO: INGLÉS`)

#Diagrama de caja para las 5 asignaturas troncales
par(mfrow=c(1,1))
boxplot(C1A$`IDIOMA EXTRANJERO: INGLÉS`,
        C1A$`CIENCIAS NATURALES`, 
        C1A$`HISTORIA, GEOGRAFÍA Y CIENCIAS SOCIALES`,
        C1A$MATEMÁTICA,
        C1A$`LENGUA Y LITERATURA`,
        main = "Asignaturas 1ero A")

#Histograma para las 5 asignaturas troncales
par(mfrow=c(2,3))
hist(C1A$`CIENCIAS NATURALES`, main = "Ciencias")
hist(C1A$`IDIOMA EXTRANJERO: INGLÉS`, main = "Ingles")
hist(C1A$`HISTORIA, GEOGRAFÍA Y CIENCIAS SOCIALES`, main = "Historia")
hist(C1A$MATEMÁTICA, main = "Matemática")
hist(C1A$`LENGUA Y LITERATURA`, main = "Lenguaje")

#Curvas de densidad 
par(mfrow=c(2,3))
plot(density(C1A$`IDIOMA EXTRANJERO: INGLÉS`) , main = "Ingles")
plot(density(C1A$`CIENCIAS NATURALES`), main = "Ciencias")
plot(density(C1A$`HISTORIA, GEOGRAFÍA Y CIENCIAS SOCIALES`), main = "Educación ciudadana")
plot(density(C1A$MATEMÁTICA), main = "Matemática")
plot(density(C1A$`LENGUA Y LITERATURA`), main = "Lenguaje")

#Estadistica descriptiva asiganturas de interes 
#1roB
C1B <- C1 %>% 
  filter ( C1$`Letra Curso` == "B")
C1B

skim(C1B$`IDIOMA EXTRANJERO: INGLÉS`)
skim(C1B$`LENGUA Y LITERATURA`)
skim(C1B$MATEMÁTICA)
skim(C1B$`HISTORIA, GEOGRAFÍA Y CIENCIAS SOCIALES`)
skim(C1B$`CIENCIAS NATURALES`)

#Boxplot
par(mfrow=c(1,1))
boxplot(C1B$`IDIOMA EXTRANJERO: INGLÉS`, 
        C1B$`CIENCIAS NATURALES`, 
        C1B$`HISTORIA, GEOGRAFÍA Y CIENCIAS SOCIALES`,
        C1B$MATEMÁTICA,
        C1B$`LENGUA Y LITERATURA`)

#Histograma
par(mfrow=c(2,3))
hist(C1B$`CIENCIAS NATURALES`)
hist(C1B$`IDIOMA EXTRANJERO: INGLÉS`)
hist(C1B$`HISTORIA, GEOGRAFÍA Y CIENCIAS SOCIALES`)
hist(C1B$MATEMÁTICA)
hist(C1B$`LENGUA Y LITERATURA`)

#Estadistica descriptiva asiganturas de interes 
#1roC
C1C <- C1 %>% 
  filter ( C1$`Letra Curso` == "C")
C1C

skim(C1C$`IDIOMA EXTRANJERO: INGLÉS`)
skim(C1C$`LENGUA Y LITERATURA`)
skim(C1C$MATEMÁTICA)
skim(C1C$`HISTORIA, GEOGRAFÍA Y CIENCIAS SOCIALES`)
skim(C1C$`CIENCIAS NATURALES`)

#Boxplot
par(mfrow=c(1,1))
boxplot(C1C$`IDIOMA EXTRANJERO: INGLÉS`, 
        C1C$`CIENCIAS NATURALES`, 
        C1C$`HISTORIA, GEOGRAFÍA Y CIENCIAS SOCIALES`,
        C1C$MATEMÁTICA,
        C1C$`LENGUA Y LITERATURA`)

#Histograma
par(mfrow=c(2,3))
hist(C1C$`CIENCIAS NATURALES`)
hist(C1C$`IDIOMA EXTRANJERO: INGLÉS`)
hist(C1C$`HISTORIA, GEOGRAFÍA Y CIENCIAS SOCIALES`)
hist(C1C$MATEMÁTICA)
hist(C1C$`LENGUA Y LITERATURA`)

#Estadistica descriptiva asiganturas de interes 
#2do medio

#Filtrado en base a las 5 asiganturas de interes
segundo <- n1 %>% 
  filter ( n1$`Cod Grado` == 2) 
view(segundo)

#Se ordenan las asiganturas en columnas
C2<- segundo %>%
  pivot_wider(names_from = `Desc Subsector`, 
              values_from = Calificación)
view(C2)

#2do A
#Se filtra por letra de curso
C2A <- C2 %>% 
  filter ( C2$`Letra Curso` == "A")
C2A

#Estadistica descriptiva asiganturas de interes
skim(C2A$`IDIOMA EXTRANJERO: INGLÉS`)
skim(C2A$`LENGUA Y LITERATURA`)
skim(C2A$MATEMÁTICA)
skim(C2A$`HISTORIA, GEOGRAFÍA Y CIENCIAS SOCIALES`)
skim(C2A$`CIENCIAS NATURALES`)

#Boxplot
par(mfrow=c(1,1))
boxplot(C2A$`IDIOMA EXTRANJERO: INGLÉS`, 
        C2A$`CIENCIAS NATURALES`, 
        C2A$`HISTORIA, GEOGRAFÍA Y CIENCIAS SOCIALES`,
        C2A$MATEMÁTICA,
        C2A$`LENGUA Y LITERATURA`)

#Histograma
par(mfrow=c(2,3))
hist(C2A$`CIENCIAS NATURALES`)
hist(C2A$`IDIOMA EXTRANJERO: INGLÉS`)
hist(C2A$`HISTORIA, GEOGRAFÍA Y CIENCIAS SOCIALES`)
hist(C2A$MATEMÁTICA)
hist(C2A$`LENGUA Y LITERATURA`)

#2do B

#Se filtra por letra de cUrso
C2B <- C2 %>% 
  filter ( C2$`Letra Curso` == "B")
C2B

#Estadistica descriptiva asiganturas de interes
skim(C2B$`IDIOMA EXTRANJERO: INGLÉS`)
skim(C2B$`LENGUA Y LITERATURA`)
skim(C2B$MATEMÁTICA)
skim(C2B$`HISTORIA, GEOGRAFÍA Y CIENCIAS SOCIALES`)
skim(C2B$`CIENCIAS NATURALES`)

#Boxplot
par(mfrow=c(1,1))
boxplot(C2B$`IDIOMA EXTRANJERO: INGLÉS`, 
        C2B$`CIENCIAS NATURALES`, 
        C2B$`HISTORIA, GEOGRAFÍA Y CIENCIAS SOCIALES`,
        C2B$MATEMÁTICA,
        C2B$`LENGUA Y LITERATURA`)

#Histograma
par(mfrow=c(2,3))
hist(C2B$`CIENCIAS NATURALES`)
hist(C2B$`IDIOMA EXTRANJERO: INGLÉS`)
hist(C2B$`HISTORIA, GEOGRAFÍA Y CIENCIAS SOCIALES`)
hist(C2B$MATEMÁTICA)
hist(C2B$`LENGUA Y LITERATURA`)

#2do C
#Se filtra por letra de cUrso
C2C <- C2 %>% 
  filter ( C2$`Letra Curso` == "C")
C2C

#Estadistica descriptiva asiganturas de interes
skim(C2C$`IDIOMA EXTRANJERO: INGLÉS`)
skim(C2C$`LENGUA Y LITERATURA`)
skim(C2C$MATEMÁTICA)
skim(C2C$`HISTORIA, GEOGRAFÍA Y CIENCIAS SOCIALES`)
skim(C2C$`CIENCIAS NATURALES`)

#Boxplot
par(mfrow=c(1,1))
boxplot(C2C$`IDIOMA EXTRANJERO: INGLÉS`, 
        C2C$`CIENCIAS NATURALES`, 
        C2C$`HISTORIA, GEOGRAFÍA Y CIENCIAS SOCIALES`,
        C2C$MATEMÁTICA,
        C2C$`LENGUA Y LITERATURA`)

#Histograma
par(mfrow=c(2,3))
hist(C2C$`CIENCIAS NATURALES`)
hist(C2C$`IDIOMA EXTRANJERO: INGLÉS`)
hist(C2C$`HISTORIA, GEOGRAFÍA Y CIENCIAS SOCIALES`)
hist(C2C$MATEMÁTICA)
hist(C2C$`LENGUA Y LITERATURA`)

#Estadistica descriptiva asiganturas de interes 

#3ero medio
#Filtrado en base a las asiganturas 
tercero <- n1 %>% 
  filter ( n1$`Cod Grado` == 3)
view(tercero)

#Se ordenan las asiganturas en columnas
C3<- tercero %>%
  pivot_wider(names_from = `Desc Subsector`, 
              values_from = Calificación)
view(C3)

#3ero A
#Se filtra por letra de curso
C3A <- C3 %>% 
  filter ( C3$`Letra Curso` == "A")
C3A

#Estadistica descriptiva asiganturas de interes
skim(C3A$`IDIOMA EXTRANJERO: INGLÉS`)
skim(C3A$`LENGUA Y LITERATURA`)
skim(C3A$MATEMÁTICA)
skim(C3A$`EDUCACIÓN CIUDADANA`)
skim(C3A$`CIENCIAS PARA LA CIUDADANÍA`)

#Boxplot
par(mfrow=c(1,1))
boxplot(C3A$`IDIOMA EXTRANJERO: INGLÉS`, 
        C3A$`CIENCIAS PARA LA CIUDADANÍA`, 
        C3A$`EDUCACIÓN CIUDADANA`,
        C3A$MATEMÁTICA,
        C3A$`LENGUA Y LITERATURA`)

#Histograma
par(mfrow=c(2,3))
hist(C3A$`IDIOMA EXTRANJERO: INGLÉS`)
hist(C3A$`CIENCIAS PARA LA CIUDADANÍA`)
hist(C3A$`EDUCACIÓN CIUDADANA`)
hist(C3A$MATEMÁTICA)
hist(C3A$`LENGUA Y LITERATURA`)

#3ero B
#Se filtra por letra de curso
C3B <- C3 %>% 
  filter ( C3$`Letra Curso` == "B")
C3B

#Estadistica descriptiva asiganturas de interes
skim(C3B$`IDIOMA EXTRANJERO: INGLÉS`)
skim(C3B$`LENGUA Y LITERATURA`)
skim(C3B$MATEMÁTICA)
skim(C3B$`EDUCACIÓN CIUDADANA`)
skim(C3B$`CIENCIAS PARA LA CIUDADANÍA`)

#Boxplot
par(mfrow=c(1,1))
boxplot(C3B$`IDIOMA EXTRANJERO: INGLÉS`, 
        C3B$`CIENCIAS PARA LA CIUDADANÍA`, 
        C3B$`EDUCACIÓN CIUDADANA`,
        C3B$MATEMÁTICA,
        C3B$`LENGUA Y LITERATURA`)

#Histograma
par(mfrow=c(2,3))
hist(C3B$`IDIOMA EXTRANJERO: INGLÉS`)
hist(C3B$`CIENCIAS PARA LA CIUDADANÍA`)
hist(C3B$`EDUCACIÓN CIUDADANA`)
hist(C3B$MATEMÁTICA)
hist(C3B$`LENGUA Y LITERATURA`)

#3ero C
#Se filtra por letra de curso
C3C <- C3 %>% 
  filter ( C3$`Letra Curso` == "C")
C3C

#Estadistica descriptiva asiganturas de interes
skim(C3C$`IDIOMA EXTRANJERO: INGLÉS`)
skim(C3C$`LENGUA Y LITERATURA`)
skim(C3C$MATEMÁTICA)
skim(C3C$`EDUCACIÓN CIUDADANA`)
skim(C3C$`CIENCIAS PARA LA CIUDADANÍA`)

#Boxplot
par(mfrow=c(1,1))
boxplot(C3C$`IDIOMA EXTRANJERO: INGLÉS`, 
        C3C$`CIENCIAS PARA LA CIUDADANÍA`, 
        C3C$`EDUCACIÓN CIUDADANA`,
        C3C$MATEMÁTICA,
        C3C$`LENGUA Y LITERATURA`)

#Histograma
par(mfrow=c(2,3))
hist(C3C$`IDIOMA EXTRANJERO: INGLÉS`)
hist(C3C$`CIENCIAS PARA LA CIUDADANÍA`)
hist(C3C$`EDUCACIÓN CIUDADANA`)
hist(C3C$MATEMÁTICA)
hist(C3C$`LENGUA Y LITERATURA`)


#Estadistica descriptiva asiganturas de interes 
#4to Medio

#Filtrado en base a las asiganturas 
cuarto <- n1 %>% 
  filter ( n1$`Cod Grado` == 4)
view(cuarto)

#Se ordenan las asiganturas en columnas
C4<- cuarto %>%
  pivot_wider(names_from = `Desc Subsector`, 
              values_from = Calificación)
view(C4)

#4to A
#Se filtra por letra de curso
C4A <- C4 %>% 
  filter ( C4$`Letra Curso` == "A")
C4A

#Estadistica descriptiva asiganturas de interes
skim(C4A$`IDIOMA EXTRANJERO: INGLÉS`)
skim(C4A$`LENGUA Y LITERATURA`)
skim(C4A$MATEMÁTICA)
skim(C4A$`EDUCACIÓN CIUDADANA`)
skim(C4A$`CIENCIAS PARA LA CIUDADANÍA`)

#Boxplot
par(mfrow=c(1,1))
boxplot(C4A$`IDIOMA EXTRANJERO: INGLÉS`, 
        C4A$`CIENCIAS PARA LA CIUDADANÍA`, 
        C4A$`EDUCACIÓN CIUDADANA`,
        C4A$MATEMÁTICA,
        C4A$`LENGUA Y LITERATURA`)

#Histograma
par(mfrow=c(2,3))
hist(C4A$`IDIOMA EXTRANJERO: INGLÉS`)
hist(C4A$`CIENCIAS PARA LA CIUDADANÍA`)
hist(C4A$`EDUCACIÓN CIUDADANA`)
hist(C4A$MATEMÁTICA)
hist(C4A$`LENGUA Y LITERATURA`)

#4to B
#Se filtra por letra de curso
C4B <- C4 %>% 
  filter ( C4$`Letra Curso` == "B")
C4B

#Estadistica descriptiva asiganturas de interes
skim(C4B$`IDIOMA EXTRANJERO: INGLÉS`)
skim(C4B$`LENGUA Y LITERATURA`)
skim(C4B$MATEMÁTICA)
skim(C4B$`EDUCACIÓN CIUDADANA`)
skim(C4B$`CIENCIAS PARA LA CIUDADANÍA`)

#Boxplot
par(mfrow=c(1,1))
boxplot(C4B$`IDIOMA EXTRANJERO: INGLÉS`, 
        C4B$`CIENCIAS PARA LA CIUDADANÍA`, 
        C4B$`EDUCACIÓN CIUDADANA`,
        C4B$MATEMÁTICA,
        C4B$`LENGUA Y LITERATURA`)

#Histograma
par(mfrow=c(2,3))
hist(C4B$`IDIOMA EXTRANJERO: INGLÉS`)
hist(C4B$`CIENCIAS PARA LA CIUDADANÍA`)
hist(C4B$`EDUCACIÓN CIUDADANA`)
hist(C4B$MATEMÁTICA)
hist(C4B$`LENGUA Y LITERATURA`)

#4to C
#Se filtra por letra de curso
C4C <- C4 %>% 
  filter ( C4$`Letra Curso` == "C")
C4C

#Estadistica descriptiva asiganturas de interes
skim(C4C$`IDIOMA EXTRANJERO: INGLÉS`)
skim(C4C$`LENGUA Y LITERATURA`)
skim(C4C$MATEMÁTICA)
skim(C4C$`EDUCACIÓN CIUDADANA`)
skim(C4C$`CIENCIAS PARA LA CIUDADANÍA`)

#Boxplot
par(mfrow=c(1,1))
boxplot(C4C$`IDIOMA EXTRANJERO: INGLÉS`, 
        C4C$`CIENCIAS PARA LA CIUDADANÍA`, 
        C4C$`EDUCACIÓN CIUDADANA`,
        C4C$MATEMÁTICA,
        C4C$`LENGUA Y LITERATURA`)

#Histograma
par(mfrow=c(2,3))
hist(C4C$`IDIOMA EXTRANJERO: INGLÉS`)
hist(C4C$`CIENCIAS PARA LA CIUDADANÍA`)
hist(C4C$`EDUCACIÓN CIUDADANA`)
hist(C4C$MATEMÁTICA)
hist(C4C$`LENGUA Y LITERATURA`)

#Exploración de curvas de densidad para 4to medio A
par(mfrow=c(2,3))
plot(density(C4C$`IDIOMA EXTRANJERO: INGLÉS`) , main = "Ingles")
plot(density(C4C$`CIENCIAS PARA LA CIUDADANÍA`), main = "Ciencias")
plot(density(C4C$`EDUCACIÓN CIUDADANA`), main = "Educación ciudadana")
plot(density(C4C$MATEMÁTICA), main = "Matemática")
plot(density(C4C$`LENGUA Y LITERATURA`), main = "Lenguaje")


#_________#_________#____________#

#Análisis de % de asistencia y promedios, hoja 2= "promedio_2023"

library(tidyr)

p1 <- promedios_2023
str(p1)

p2 <- filter(p1, p1$`Promedio Final` != 0)


#1. Cantidad de mujeres y hombres totales
help(table)

#Tabla de frecuencia para la variable género
tabla1 <- data.frame(table(p2$Genero))
tabla1 

#Porcentaje de personas de sexo masculino y femenino
prop.table(tabla1$Freq)*100

#Grafico de barras de la variable genero
ggplot(tabla1, 
       aes(x = Var1, 
           y= Freq, )) + 
  geom_col(fill = "green" , 
           col = "black")

#2. summary,  desviación estándar y % asistencia y promedio
p2 <-p2[ , c(4,6,7,9,17,18)]
names(p2)

library(ggplot2)

#%Asistencia

#PRIMEROS MEDIOS  
p1A <- p2 %>% filter(p2$`Cod Grado`==1,
             p2$`Letra Curso`=="A") 

p1B <- p2 %>% filter(p2$`Cod Grado`==1,
                     p2$`Letra Curso`=="B") 

p1C <- p2 %>% filter(p2$`Cod Grado`==1,
                     p2$`Letra Curso`=="C") 

#SEGUNDOS MEDIOS
p2A <- p2 %>% filter(p2$`Cod Grado`==2,
                     p2$`Letra Curso`=="A") 

p2B <- p2 %>% filter(p2$`Cod Grado`==2,
                     p2$`Letra Curso`=="B") 

#TERCEROS MEDIOS
p3A <- p2 %>% filter(p2$`Cod Grado`==3,
                     p2$`Letra Curso`=="A") 

p3B <- p2 %>% filter(p2$`Cod Grado`==3,
                     p2$`Letra Curso`=="B")

p3C <- p2 %>% filter(p2$`Cod Grado`==3,
                     p2$`Letra Curso`=="C")

#CUARTOS MEDIOS
p4A <- p2 %>% filter(p2$`Cod Grado`==4,
                     p2$`Letra Curso`=="A") 

p4B <- p2 %>% filter(p2$`Cod Grado`==4,
                     p2$`Letra Curso`=="B")

p4C <- p2 %>% filter(p2$`Cod Grado`==4,
                     p2$`Letra Curso`=="C")

#Asistencia
A_pA <- mean(p1A$`%Asistenca`)
A_pB <- mean(p1B$`%Asistenca`)
A_pC <- mean(p1C$`%Asistenca`)
A_sA <- mean(p2A$`%Asistenca`)
A_sB <- mean(p2B$`%Asistenca`)
A_tA <- mean(p3A$`%Asistenca`)
A_tB <- mean(p3B$`%Asistenca`)
A_tC <- mean(p3C$`%Asistenca`)
A_cA <- mean(p4A$`%Asistenca`)
A_cB <- mean(p4B$`%Asistenca`)
A_cC <- mean(p4C$`%Asistenca`)

#Summary asistencias
summary (asistencia <- c(A_pA, A_pB, A_pC, A_sA, A_sB, A_tA, A_tB, A_tC, A_cA, A_cB, A_cC ))

#Boxplot asistencias
boxplot(asistencia)

# gráfico de dispersión para asistencia todos los cursos
grafico_asistencia <- qplot(x = resumen_asis$Cursos, 
                            y = resumen_asis$Prom_asistencia, 
                            data = resumen_asis, 
                            geom = "point",
                            main = "grafico asistencia")

help("lines")

#Promedio
P_pA <- mean(p1A$`Promedio Final`)
P_pB <- mean(p1B$`Promedio Final`)
P_pC <- mean(p1C$`Promedio Final`)
P_sA <- mean(p2A$`Promedio Final`)
P_sB <- mean(p2B$`Promedio Final`)
P_tA <- mean(p3A$`Promedio Final`)
P_tB <- mean(p3B$`Promedio Final`)
P_tC <- mean(p3C$`Promedio Final`)
P_cA <- mean(p4A$`Promedio Final`)
P_cB <- mean(p4B$`Promedio Final`)
P_cC <- mean(p4C$`Promedio Final`)

#Summary de promedios
summary (promedio <- c(P_pA, P_pB, P_pC, P_sA, P_sB, P_tA, P_tB, P_tC, P_cA, P_cB, P_cC))

#Boxplot de promedios
boxplot(promedio)

# gráfico de dispersión para promedios todos los cursos
grafico_promedios <- qplot(x = resumen_prom$Cursos, 
                            y = resumen_prom$Prom_promedio, 
                            data = resumen_asis, 
                            geom = "point",
                            main = "grafico promedios")
grafico_promedios

#Tabla resumen de datos
tabla_resumen <- 
  data.frame(Cursos = c("1A", "1B", "1C", 
                        "2A", "2B", 
                        "3A", "3B", "3C",
                        "4A", "4B", "4C"),
             Prom_asistencia = c(A_pA, A_pB, A_pC, 
                          A_sA, A_sB, A_tA, 
                          A_tB, A_tC, A_cA, 
                          A_cB, A_cC),
             Prom_promedio = c(P_pA, P_pB, P_pC, 
                               P_sA, P_sB, P_tA, 
                               P_tB, P_tC, P_cA, 
                               P_cB, P_cC))

# tabla resumen de asistencias y promedios
print(tabla_resumen)




#_________#_________#____________#

#3. Modelamiento de datos
#Para el modelamiento de datos se utilizará la asigantura de lenguaje
#en los distintos niveles, es decir, 1ero, 2do, 3ero y 4to medio, con la distribución normal

#Lenguaje 1eros medios (A, B,C)

#Se busca la data que es la primera hoja del libro de excel
n1 <-n1[ , c(3,10,12,14,17)]
names(n1)

#Se filtra por nivel y asigantura de Lenguaje
primero_lenguaje <- n1 %>% 
  filter ( n1$`Cod Grado` == 1,
           n1$`Desc Subsector` == "LENGUA Y LITERATURA")

print(primero_lenguaje)
data.frame(primero_lenguaje)

#Se obtienen la media y desviación estandar
mean(primero_lenguaje$Calificación)
# Media de valor 𝜇=5,47340

sd(primero_lenguaje$Calificación, na.rm = FALSE)
#Desviación estandar de 𝜎= 0.6662647

#vector
x <- (primero_lenguaje$Calificación)

#Prueba de normalidad shapiro-wilk
set.seed(50)
x <- rnorm(x)
x.test <- shapiro.test(x)
print(x.test)


##REVISAR FUNCION
help("lapply")

#Histograma de la variable calificacion lenguaje primeros medios
library(ggplot2)
library(tibble)
library(ggthemes)

ggplot(data = primero_lenguaje, mapping = 
         aes(x= primero_lenguaje$Calificación)) +
  geom_histogram(color = "black",
                 fill = "white",
                 binwidth = NULL,
                 bins = 30) +
  labs(title = "Notas de lenguaje primero medio",
       x = "Notas 1M",
       y = "Frecuencia")

#Se divide el primero en cursos A, B, C
nota_primero <- split(primero_lenguaje$Calificación,
                      primero_lenguaje$`Letra Curso`)
nota_primero

#Se comparan categorias, los diferentes cursos A, B, C
ggplot(data = primero_lenguaje, 
       mapping = aes(x= primero_lenguaje$Calificación))+
  geom_histogram( aes(fill = primero_lenguaje$`Letra Curso`),
                  bins = 30,
                  ) +
  labs(title = "Notas primeros medios",
       x = "Notas",
       y = "Frecuencia") +
  theme_economist()

#Grafica funcion de densidad de primeros medios en la asigamtura de lenguaje
x <- (primero_lenguaje$Calificación)
y1 <- dnorm(primero_lenguaje$Calificación, mean=5.5, sd=0.67)

plot(x,y1,
     ylim = c(0,1),
     xlab = "notas 1M",
     ylab = "Densidad",
     type = "p",
     main = "Notas lenguaje primero medio")

help("type")

#Curva de densidad
ggplot(data = primero_lenguaje,
       mapping = aes(x = primero_lenguaje$Calificación, mean=5.5, sd=0.67)) +
  geom_density() +
  labs(title = "Notas primeros medios",
       x = "Notas",
       y = "Porcentaje") +
  theme_economist()


#Revisando normalidad 
ggplot(primero_lenguaje, 
       aes(sample = primero_lenguaje$Calificación )) +
  stat_qq(aes(color = primero_lenguaje$`Letra Curso`))


#Probabilidad de que un estudiante tenga nota >6,3 

1- pnorm(calificacion esperada, mean=𝜇, sd=𝜎) 

#En este caso tenemos que:
1 - pnorm(6.3, mean= 5.47340, sd= 0.6662647)
[1] 0.02551803 #Se tiene una probabilidad de un 2,6%  

#El percentil 75 de la distribución
qnorm(p, mean=𝜇, sd=𝜎 )
qnorm(0.75, mean= 5.47340, sd= 0.6662647)
#El percentil 75 = 5.922789

#Entre que notas esta el 50% central de la distribución
qnorm(0.25, mean= 5.47340, sd= 0.6662647)
qnorm(0.75, mean= 5.47340, sd= 0.6662647)
#El 50% central de la distribución esta entre las notas 5.0 y 5.9

