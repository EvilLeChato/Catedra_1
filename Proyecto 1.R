#Limpiar el entorno
rm(list = ls())

#Lllamar a la base de datos
library(readxl)

View(nomina_calificaciones2023_Mirian_Astete)

nomina_cali2023 <- read_excel(path = "Library/CloudStorage/OneDrive-Personal/MAGISTER EN DATA SCIENCE/4. Programacion en R/Catedra 1/nomina_calificaciones2023_Mirian Astete.xlsx", 
                              sheet = 1)
nomina_cali2023

nomina_prom2023 <- read_excel(path = "Library/CloudStorage/OneDrive-Personal/MAGISTER EN DATA SCIENCE/4. Programacion en R/Catedra 1/nomina_calificaciones2023_Mirian Astete.xlsx", 
                              sheet = 2)
nomina_prom2023

excel_sheets("Library/CloudStorage/OneDrive-Personal/MAGISTER EN DATA SCIENCE/4. Programacion en R/Catedra 1/nomina_calificaciones2023_Mirian Astete.xlsx")

#Se le otorga un nombre a la base de datos
datos1 <- nomina_cali2023
datos1

datos2 <- nomina_prom2023
datos2


#Visualizar los datos, sus filas y columnas
names(datos1)
names(datos2)
str(datos1)
str(datos2)

#Llamar al paquete con el que se trabajará

library(tidyverse)
library(dplyr)



#Seleccionar las variables a analizar
#Hoja "nomina_calificaciones 2023"
#Se analizará media aritmética, moda, mediana, 
#nota mínima, nota máxima, desviación estándar
#1. Por curso y asigantura
#2. Por asigantura

###Utilizar summarice y group.by
names(datos1)

datos1.1 <-data.frame(datos1$`Cod Grado` , datos1$`Letra Curso`, 
                      datos1$`Desc Subsector` , datos1$Calificación)
datos1.1 

#filtrar asignatura por curso y letra curso

#PRIMERO MEDIO

#PRIMERO A
#1ero A: Lenguaje

primeroA_asignaturas<- filter(datos1.1, datos1$`Cod Grado`== "1", 
                           datos1$`Letra Curso`=="A" ,
                           datos1.1, datos1$`Desc Subsector`=="LENGUA Y LITERATURA")
                       
primeroA_asignaturas

summary(primeroA_lenguaje$datos1.Calificación)

sd(primeroA_lenguaje$datos1.Calificación)

tabla1 <- data.frame(table(primeroA_lenguaje$datos1.Calificación))
tabla1

ggplot(tabla1, aes(x = Var1, y= Freq)) + 
  geom_col(fill = "grey" , col = "black" )

boxplot(primeroA_lenguaje$datos1.Calificación)
abline(h=mean(primeroA_lenguaje$datos1.Calificación), col="green")

#1ero A: Matematica

primeroA_matematica <- filter(datos1.1, datos1$`Cod Grado`== "1", datos1$`Letra Curso`=="A" , 
                              datos1$`Desc Subsector`== "MATEMÁTICA")
primeroA_matematica

summary(primeroA_matematica$datos1.Calificación)

sd(primeroA_matematica$datos1.Calificación)

tabla2 <- data.frame(table(primeroA_matematica$datos1.Calificación))
tabla2

ggplot(tabla2, aes(x = Var1, y= Freq)) + 
  geom_col(fill = "grey" , col = "black" )

boxplot(primeroA_matematica$datos1.Calificación)
abline(h=mean(primeroA_matematica$datos1.Calificación), col="green")

#1ero A: Ciencias

primeroA_ciencias<- filter(datos1.1, datos1$`Cod Grado`== "1", datos1$`Letra Curso`=="A" , 
                           datos1$`Desc Subsector`== "CIENCIAS NATURALES")
primeroA_ciencias

summary(primeroA_ciencias$datos1.Calificación)

sd(primeroA_ciencias$datos1.Calificación)

tabla3 <- data.frame(table(primeroA_ciencias$datos1.Calificación))
tabla3

ggplot(tabla3, aes(x = Var1, y= Freq)) + 
  geom_col(fill = "grey" , col = "black" )

boxplot(primeroA_ciencias$datos1.Calificación)
abline(h=mean(primeroA_ciencias$datos1.Calificación), col="green")


#1ero A: Historia
primeroA_historia <- filter(datos1.1, datos1$`Cod Grado`== "1", datos1$`Letra Curso`=="A" , 
                    datos1$`Desc Subsector`== "HISTORIA, GEOGRAFÍA Y CIENCIAS SOCIALES")
primeroA_historia

summary(primeroA_historia$datos1.Calificación)

sd(primeroA_historia$datos1.Calificación)

tabla4 <- data.frame(table(primeroA_historia$datos1.Calificación))
tabla4

ggplot(tabla4, aes(x = Var1, y= Freq)) + 
  geom_col(fill = "grey" , col = "black" )

boxplot(primeroA_historia$datos1.Calificación)
abline(h=mean(primeroA_historia$datos1.Calificación), col="green")

#1ero A: Ingles
primeroA_ingles <- filter(datos1.1, datos1$`Cod Grado`== "1", datos1$`Letra Curso`=="A" , 
                          datos1$`Desc Subsector`== "IDIOMA EXTRANJERO: INGLÉS")
primeroA_ingles

summary(primeroA_ingles$datos1.Calificación)

sd(primeroA_ingles$datos1.Calificación)

tabla5 <- data.frame(table(primeroA_ingles$datos1.Calificación))
tabla5

ggplot(tabla5, aes(x = Var1, y= Freq)) + 
  geom_col(fill = "grey" , col = "black" )

boxplot(primeroA_ingles$datos1.Calificación)
abline(h=mean(primeroA_ingles$datos1.Calificación), col="green")

#PRIMERO B
#1ero B: Lenguaje

primeroB_lenguaje<- filter(datos1.1, datos1$`Cod Grado`== "1", datos1$`Letra Curso`=="B" , 
                           datos1$`Desc Subsector`== "LENGUA Y LITERATURA")
primeroB_lenguaje 

summary(primeroB_lenguaje$datos1.Calificación)

sd(primeroB_lenguaje$datos1.Calificación)

tabla6 <- data.frame(table(primeroB_lenguaje$datos1.Calificación))
tabla6

ggplot(tabla6, aes(x = Var1, y= Freq)) + 
  geom_col(fill = "grey" , col = "black" )

boxplot(primeroB_lenguaje$datos1.Calificación)
abline(h=mean(primeroB_lenguaje$datos1.Calificación), col="green")

#1ero B: Matematica

primeroB_matematica <- filter(datos1.1, datos1$`Cod Grado`== "1", datos1$`Letra Curso`=="B" , 
                              datos1$`Desc Subsector`== "MATEMÁTICA")
primeroB_matematica

summary(primeroB_matematica$datos1.Calificación)

sd(primeroB_matematica$datos1.Calificación)

tabla7 <- data.frame(table(primeroB_matematica$datos1.Calificación))
tabla7

ggplot(tabla7, aes(x = Var1, y= Freq)) + 
  geom_col(fill = "grey" , col = "black" )

boxplot(primeroB_matematica$datos1.Calificación)
abline(h=mean(primeroB_matematica$datos1.Calificación), col="green")

#1ero B: Ciencias

primeroB_ciencias<- filter(datos1.1, datos1$`Cod Grado`== "1", datos1$`Letra Curso`=="B" , 
                           datos1$`Desc Subsector`== "CIENCIAS NATURALES")
primeroB_ciencias

summary(primeroB_ciencias$datos1.Calificación)

sd(primeroB_ciencias$datos1.Calificación)

tabla8 <- data.frame(table(primeroB_ciencias$datos1.Calificación))
tabla8

ggplot(tabla8, aes(x = Var1, y= Freq)) + 
  geom_col(fill = "grey" , col = "black" )

boxplot(primeroB_ciencias$datos1.Calificación)
abline(h=mean(primeroB_ciencias$datos1.Calificación), col="green")


#1ero B: Historia
primeroB_historia <- filter(datos1.1, datos1$`Cod Grado`== "1", datos1$`Letra Curso`=="B" , 
                    datos1$`Desc Subsector`== "HISTORIA, GEOGRAFÍA Y CIENCIAS SOCIALES")
primeroB_historia

summary(primeroB_historia$datos1.Calificación)

sd(primeroB_historia$datos1.Calificación)

tabla9 <- data.frame(table(primeroB_historia$datos1.Calificación))
tabla9

ggplot(tabla9, aes(x = Var1, y= Freq)) + 
  geom_col(fill = "grey" , col = "black" )

boxplot(primeroB_historia$datos1.Calificación)
abline(h=mean(primeroB_historia$datos1.Calificación), col="green")

#1ero B: Ingles
primeroA_ingles <- filter(datos1.1, datos1$`Cod Grado`== "1", datos1$`Letra Curso`=="B" , 
                          datos1$`Desc Subsector`== "IDIOMA EXTRANJERO: INGLÉS")
primeroB_ingles

summary(primeroB_ingles$datos1.Calificación)

sd(primeroB_ingles$datos1.Calificación)

tabla10 <- data.frame(table(primeroB_ingles$datos1.Calificación))
tabla10

ggplot(tabla10, aes(x = Var1, y= Freq)) + 
  geom_col(fill = "grey" , col = "black" )

boxplot(primeroB_ingles$datos1.Calificación)
abline(h=mean(primeroB_ingles$datos1.Calificación), col="green")


#PRIMERO C
#1ero C: Lenguaje

primeroC_lenguaje<- filter(datos1.1, datos1$`Cod Grado`== "1", datos1$`Letra Curso`=="C" , 
                           datos1$`Desc Subsector`== "LENGUA Y LITERATURA")
primeroC_lenguaje 

summary(primeroC_lenguaje$datos1.Calificación)

sd(primeroC_lenguaje$datos1.Calificación)

tabla11 <- data.frame(table(primeroC_lenguaje$datos1.Calificación))
tabla11

ggplot(tabla11, aes(x = Var1, y= Freq)) + 
  geom_col(fill = "grey" , col = "black" )

boxplot(primeroC_lenguaje$datos1.Calificación)
abline(h=mean(primeroB_lenguaje$datos1.Calificación), col="green")

#1ero C: Matematica

primeroC_matematica <- filter(datos1.1, datos1$`Cod Grado`== "1", datos1$`Letra Curso`=="C" , 
                              datos1$`Desc Subsector`== "MATEMÁTICA")
primeroC_matematica

summary(primeroC_matematica$datos1.Calificación)

sd(primeroC_matematica$datos1.Calificación)

tabla12 <- data.frame(table(primeroC_matematica$datos1.Calificación))
tabla12

ggplot(tabla12, aes(x = Var1, y= Freq)) + 
  geom_col(fill = "grey" , col = "black" )

boxplot(primeroC_matematica$datos1.Calificación)
abline(h=mean(primeroC_matematica$datos1.Calificación), col="green")

#1ero C: Ciencias

primeroC_ciencias<- filter(datos1.1, datos1$`Cod Grado`== "1", datos1$`Letra Curso`=="C" , 
                           datos1$`Desc Subsector`== "CIENCIAS NATURALES")
primeroC_ciencias

summary(primeroC_ciencias$datos1.Calificación)

sd(primeroC_ciencias$datos1.Calificación)

tabla13 <- data.frame(table(primeroC_ciencias$datos1.Calificación))
tabla13

ggplot(tabla13, aes(x = Var1, y= Freq)) + 
  geom_col(fill = "grey" , col = "black" )

boxplot(primeroC_ciencias$datos1.Calificación)
abline(h=mean(primeroC_ciencias$datos1.Calificación), col="green")


#1ero C: Historia
primeroC_historia <- filter(datos1.1, datos1$`Cod Grado`== "1", datos1$`Letra Curso`=="C" , 
                            datos1$`Desc Subsector`== "HISTORIA, GEOGRAFÍA Y CIENCIAS SOCIALES")
primeroC_historia

summary(primeroC_historia$datos1.Calificación)

sd(primeroC_historia$datos1.Calificación)

tabla14 <- data.frame(table(primeroC_historia$datos1.Calificación))
tabla14

ggplot(tabla14, aes(x = Var1, y= Freq)) + 
  geom_col(fill = "grey" , col = "black" )

boxplot(primeroC_historia$datos1.Calificación)
abline(h=mean(primeroC_historia$datos1.Calificación), col="green")

#1ero C: Ingles
primeroC_ingles <- filter(datos1.1, datos1$`Cod Grado`== "1", datos1$`Letra Curso`=="C" , 
                          datos1$`Desc Subsector`== "IDIOMA EXTRANJERO: INGLÉS")
primeroC_ingles

summary(primeroC_ingles$datos1.Calificación)

sd(primeroC_ingles$datos1.Calificación)

tabla15 <- data.frame(table(primeroC_ingles$datos1.Calificación))
tabla15

ggplot(tabla15, aes(x = Var1, y= Freq)) + 
  geom_col(fill = "grey" , col = "black" )

boxplot(primeroC_ingles$datos1.Calificación)
abline(h=mean(primeroC_ingles$datos1.Calificación), col="green")


#SEGUNDO A
#2do A: Lenguaje

segundoA_lenguaje<- filter(datos1.1, datos1$`Cod Grado`== "2", datos1$`Letra Curso`=="A" , 
                           datos1$`Desc Subsector`== "LENGUA Y LITERATURA")
segundoA_lenguaje 

summary(segundoA_lenguaje_lenguaje$datos1.Calificación)

sd(segundoA_lenguaje_lenguaje$datos1.Calificación)

tabla16 <- data.frame(table(segundoA_lenguaje$datos1.Calificación))
tabla16

ggplot(tabla16, aes(x = Var1, y= Freq)) + 
  geom_col(fill = "grey" , col = "black" )

boxplot(segundoA_lenguaje$datos1.Calificación)
abline(h=mean(segundoA_lenguaje$datos1.Calificación), col="green")

#2do A: Matematica

segundoA_matematica <- filter(datos1.1, datos1$`Cod Grado`== "2", datos1$`Letra Curso`=="A" , 
                              datos1$`Desc Subsector`== "MATEMÁTICA")
segundoA_matematica

summary(segundoA_matematica$datos1.Calificación)

sd(segundoA_matematica$datos1.Calificación)

tabla17 <- data.frame(table(segundoA_matematica$datos1.Calificación))
tabla17

ggplot(tabla17, aes(x = Var1, y= Freq)) + 
  geom_col(fill = "grey" , col = "black" )

boxplot(segundoA_matematica$datos1.Calificación)
abline(h=mean(segundoA_matematica$datos1.Calificación), col="green")

#2do A: Ciencias

segundoA_ciencias<- filter(datos1.1, datos1$`Cod Grado`== "2", datos1$`Letra Curso`=="A" , 
                           datos1$`Desc Subsector`== "CIENCIAS NATURALES")
segundoA_ciencias

summary(segundoA_ciencias$datos1.Calificación)

sd(segundoA_ciencias$datos1.Calificación)

tabla18 <- data.frame(table(segundoA_ciencias$datos1.Calificación))
tabla18

ggplot(tabla18, aes(x = Var1, y= Freq)) + 
  geom_col(fill = "grey" , col = "black" )

boxplot(segundoA_ciencias$datos1.Calificación)
abline(h=mean(segundoA_ciencias$datos1.Calificación), col="green")


#2do A: Historia
segundoA_historia <- filter(datos1.1, datos1$`Cod Grado`== "2", datos1$`Letra Curso`=="A" , 
                          datos1$`Desc Subsector`== "HISTORIA, GEOGRAFÍA Y CIENCIAS SOCIALES")
segundoA_historia

summary(segundoA_historia$datos1.Calificación)

sd(segundoA_historia$datos1.Calificación)

tabla19 <- data.frame(table(segundoA_historia$datos1.Calificación))
tabla19

ggplot(tabla19, aes(x = Var1, y= Freq)) + 
  geom_col(fill = "grey" , col = "black" )

boxplot(segundoA_historia$datos1.Calificación)
abline(h=mean(segundoA_historia$datos1.Calificación), col="green")

#2do A: Ingles
segundoA_ingles <- filter(datos1.1, datos1$`Cod Grado`== "2", datos1$`Letra Curso`=="A" , 
                          datos1$`Desc Subsector`== "IDIOMA EXTRANJERO: INGLÉS")
segundoA_ingles

summary(segundoA_ingles$datos1.Calificación)

sd(segundoA_ingles$datos1.Calificación)

tabla20 <- data.frame(table(segundoA_ingles$datos1.Calificaciónn))
tabla20

ggplot(tabla20, aes(x = Var1, y= Freq)) + 
  geom_col(fill = "grey" , col = "black" )

boxplot(segundoA_ingles$datos1.Calificación)
abline(h=mean(segundoA_ingles$datos1.Calificación), col="green")

#SEGUNDO B
#2do B: Lenguaje

segundoB_lenguaje<- filter(datos1.1, datos1$`Cod Grado`== "2", datos1$`Letra Curso`=="B" , 
                           datos1$`Desc Subsector`== "LENGUA Y LITERATURA")
segundoB_lenguaje 

summary(segundoB_lenguaje_lenguaje$datos1.Calificación)

sd(segundoB_lenguaje$datos1.Calificación)

tabla21 <- data.frame(table(segundoB_lenguaje$datos1.Calificación))
tabla21

ggplot(tabla21, aes(x = Var1, y= Freq)) + 
  geom_col(fill = "grey" , col = "black" )

boxplot(segundoB_lenguaje$datos1.Calificación)
abline(h=mean(segundoB_lenguaje$datos1.Calificación), col="green")

#2do B: Matematica

segundoB_matematica <- filter(datos1.1, datos1$`Cod Grado`== "2", datos1$`Letra Curso`=="B" , 
                              datos1$`Desc Subsector`== "MATEMÁTICA")
segundoB_matematica

summary(segundoB_matematica$datos1.Calificación)

sd(segundoB_matematica$datos1.Calificación)

tabla22 <- data.frame(table(segundoB_matematica$datos1.Calificación))
tabla22 

ggplot(tabla22, aes(x = Var1, y= Freq)) + 
  geom_col(fill = "grey" , col = "black" )

boxplot(segundoB_matematica$datos1.Calificación)
abline(h=mean(segundoB_matematica$datos1.Calificación), col="green")

#2do B: Ciencias

segundoB_ciencias<- filter(datos1.1, datos1$`Cod Grado`== "2", datos1$`Letra Curso`=="B" , 
                           datos1$`Desc Subsector`== "CIENCIAS NATURALES")
segundoB_ciencias

summary(segundoB_ciencias$datos1.Calificación)

sd(segundoB_ciencias$datos1.Calificación)

tabla23 <- data.frame(table(segundoB_ciencias$datos1.Calificación))
tabla23

ggplot(tabla23, aes(x = Var1, y= Freq)) + 
  geom_col(fill = "grey" , col = "black" )

boxplot(segundoB_ciencias$datos1.Calificación)
abline(h=mean(segundoB_ciencias$datos1.Calificación), col="green")


#2do B: Historia
segundoB_historia<- filter(datos1.1, datos1$`Cod Grado`== "2", datos1$`Letra Curso`=="B" , 
                           datos1$`Desc Subsector`== "HISTORIA, GEOGRAFÍA Y CIENCIAS SOCIALES")
segundoB_historia

summary(segundoB_historia$datos1.Calificación)

sd(segundoB_historia$datos1.Calificación)

tabla24 <- data.frame(table(segundoA_historia$datos1.Calificación))
tabla24

ggplot(tabla24, aes(x = Var1, y= Freq)) + 
  geom_col(fill = "grey" , col = "black" )

boxplot(segundoB_historia$datos1.Calificación)
abline(h=mean(segundoB_historia$datos1.Calificación), col="green")

#2do B: Ingles
segundoB_ingles <- filter(datos1.1, datos1$`Cod Grado`== "2", datos1$`Letra Curso`=="B" , 
                          datos1$`Desc Subsector`== "IDIOMA EXTRANJERO: INGLÉS")
segundoB_ingles

summary(segundoB_ingles$datos1.Calificación)

sd(segundoB_ingles$datos1.Calificación)

tabla25 <- data.frame(table(segundoB_ingles$datos1.Calificación))
tabla25

ggplot(tabla19, aes(x = Var1, y= Freq)) + 
  geom_col(fill = "grey" , col = "black" )

boxplot(segundoB_ingles$datos1.Calificación)
abline(h=mean(segundoB_ingles$datos1.Calificación), col="green")

##TERCERO MEDIO

#3ero A: Lenguaje

terceroA_lenguaje<- filter(datos1.1, datos1$`Cod Grado`== "3", datos1$`Letra Curso`=="A" , 
                           datos1$`Desc Subsector`== "LENGUA Y LITERATURA")
terceroA_lenguaje 

summary(terceroA_lenguaje$datos1.Calificación)

sd(terceroA_lenguaje$datos1.Calificación)

tabla26 <- data.frame(table(terceroA_lenguaje$datos1.Calificación))
tabla26

ggplot(tabla26, aes(x = Var1, y= Freq)) + 
  geom_col(fill = "grey" , col = "black" )

boxplot(terceroA_lenguaje$datos1.Calificación)
abline(h=mean(terceroA_lenguaje$datos1.Calificación), col="green")

#3ro A: Matematica

terceroA_matematica <- filter(datos1.1, datos1$`Cod Grado`== "3", datos1$`Letra Curso`=="A" , 
                              datos1$`Desc Subsector`== "MATEMÁTICA")
terceroA_matematica

summary(terceroA_matematica$datos1.Calificación)

sd(terceroA_matematica$datos1.Calificación)

tabla27 <- data.frame(table(terceroA_matematica$datos1.Calificación))
tabla27 

ggplot(tabla27, aes(x = Var1, y= Freq)) + 
  geom_col(fill = "grey" , col = "black" )

boxplot(terceroA_matematica$datos1.Calificación)
abline(h=mean(terceroA_matematica$datos1.Calificación), col="green")

#3ero A: Ciencias

terceroA_ciencias<- filter(datos1.1, datos1$`Cod Grado`== "3", datos1$`Letra Curso`=="A" , 
                           datos1$`Desc Subsector`== "CIENCIAS PARA LA CIUDADANÍA")
terceroA_ciencias

summary(terceroA_ciencias$datos1.Calificación)

sd(terceroA_ciencias$datos1.Calificación)

tabla28 <- data.frame(table(terceroA_ciencias$datos1.Calificación))
tabla28

ggplot(tabla28, aes(x = Var1, y= Freq)) + 
  geom_col(fill = "grey" , col = "black" )

boxplot(terceroA_ciencias$datos1.Calificación)
abline(h=mean(terceroA_ciencias$datos1.Calificación), col="green")


#3ro A: Historia
terceroA_historia<- filter(datos1.1, datos1$`Cod Grado`== "3", datos1$`Letra Curso`=="A" , 
                           datos1$`Desc Subsector`== "EDUCACIÓN CIUDADANA")
terceroA_historia

summary(terceroA_historia$datos1.Calificación)

sd(terceroA_historia$datos1.Calificación)

tabla29 <- data.frame(table(terceroA_historia$datos1.Calificación))
tabla29

ggplot(tabla29, aes(x = Var1, y= Freq)) + 
  geom_col(fill = "grey" , col = "black" )

boxplot(terceroA_historia$datos1.Calificación)
abline(h=mean(terceroA_historia$datos1.Calificación), col="green")

#3ro A: Ingles
terceroA_ingles <- filter(datos1.1, datos1$`Cod Grado`== "3", datos1$`Letra Curso`=="A" , 
                          datos1$`Desc Subsector`== "IDIOMA EXTRANJERO: INGLÉS")
terceroA_ingles

summary(terceroA_ingles$datos1.Calificación)

sd(terceroA_ingles$datos1.Calificación)

tabla30 <- data.frame(table(segundoB_ingles$datos1.Calificación))
tabla30


ggplot(tabla30, aes(x = Var1, y= Freq)) + 
  geom_col(fill = "grey" , col = "black" )

boxplot(terceroA_ingles$datos1.Calificación)
abline(h=mean(terceroA_ingles$datos1.Calificación), col="green")

########AQUI QUEDE FALTA 3RO B, C. 4TO A,B,C

#Variables curso, letra curso, genero, porcentaje de asistencia y promedio general.
#Se hará un análisis comparativo en base a 
#1. Cantidad de mujeres y hombres en cada curso
#2. summary,  desviación estándar y CV entre cursos, % asistencia y promedio
#3. Análisis entre hombres y mujeres en relacion a % asistencia y promedios


datos2 <- nomina_prom2023
datos2

names(datos2)
str(datos2)

#Seleccion de variables a analizar
datos2.1 <-data.frame(datos2$`Cod Grado`, datos2$`Letra Curso`, datos2$Genero, 
                      datos2$`Promedio Final`, datos2$`%Asistenca`)
datos2.1 

#Comparacion de genero todo el establecimiento

comparacion_genero <- table(datos2$Genero)
comparacion_genero

prop.table(comparacion_genero) *100

#Estadistica variable promedio todo el establecimiento

summary()

#comparacion de genero por curso

comparacion_1A <- filter(datos2.1,  datos2.1$datos2..Cod.Grado.=="1", 
                         datos2.1$datos2..Letra.Curso.=="A")
comparacion_1A        

tablagenero_1A <- table(comparacion_1A$datos2.Genero)
tablagenero_1A

prop.table(tablagenero_1A) *100

#Estadistica variable Promedio 1A

summary(comparacion_1A$datos2..Promedio.Final.)
sd(comparacion_1A$datos2..Promedio.Final.)

#Estadistica variable % asistencia














