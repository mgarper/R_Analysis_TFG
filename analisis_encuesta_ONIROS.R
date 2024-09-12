install.packages("ggplot2")
install.packages("dplyr")
install.packages("GGally")
library(GGally)
library(dplyr)
library(ggplot2)
library(descr)
library(psych)
library(readxl)
library(sjPlot)
library(splitstackshape)
library(tidyr)
library(dlookr)
require(caret)

datos <- read_excel('C:/Users/USUARIO/Downloads/Encuesta de aceptación sobre ONIROS (respuestas).xlsx')

View(datos)
colnames(datos) <- c('Marca_temporal', 
                     'Juega_Videojuegos',
                     'No_Juega_Razon',
                     'Ha_Jugado_Videojuegos',
                     'Jugaria_Videojuegos',
                     'Frecuencia_Juego',
                     'Plataforma_Juego',
                     'Tipo_Juegos',
                     'Interes_SinglePlayer',
                     'Razon_NoSinglePLayer',
                     'Conoce_MV',
                     'Ha_JugadoMV',
                     'MV_Jugados',
                     'MV_Conocidos',
                     'Jugaria_MV',
                     'Likert_Narrativa1',
                     'Likert_Narrativa2',
                     'Likert_Narrativa3',
                     'Likert_Narrativa4',
                     'Likert_Narrativa5',
                     'Likert_Narrativa6',
                     'Likert_Jugabilidad1',
                     'Likert_Jugabilidad2',
                     'Likert_Jugabilidad3',
                     'Likert_Jugabilidad4',
                     'Num_Mecanicas',
                     'Complejidad_Mecanicas',
                     'Likert_EstiloVis1',
                     'Likert_EstiloVis2',
                     'Likert_EstiloVis3',
                     'Estilo_preferido',
                     'Likert_SoftSkills1',
                     'Likert_SoftSkills2',
                     'Likert_SoftSkills3',
                     'Likert_SoftSkills4',
                     'Likert_SoftSkills5',
                     'Likert_IntEmocional1',
                     'Likert_IntEmocional2',
                     'Likert_IntEmocional3',
                     'Likert_IntEmocional4',
                     'Likert_IntEmocional5',
                     'Likert_IntEmocional6',
                     'Likert_IntEmocional7',
                     'Aceptacion_Oniros',
                     'Oniros_destacado',
                     'GeneroJug',
                     'EdadJug',
                     'SituacionLaboralJug',
                     'HobbiesJug',
                     'Email',
                     'GeneroNoJug',
                     'EdadNoJug',
                     'SituacionLaboralNoJug',
                     'HobbiesNoJug',
                     'GeneroNoMV',
                     'EdadNoMV',
                     'SituacionLaboralNoMV',
                     'HobbiesNoMV') 

# PREPROCESADO
#1- Transformación de texto a factores -- PREGUNTAS CERRADAS Y ABIERTAS
datos$f_Juega_Videojuegos <- factor(datos$Juega_Videojuegos,
                                    levels = c('Sí','No'))

datos$f_Ha_Jugado_Videojuegos <- factor(datos$Ha_Jugado_Videojuegos,
                                    levels = c('Sí','No'))

datos$f_Jugaria_Videojuegos <- factor(datos$Jugaria_Videojuegos,
                                    levels = c('Sí','No'))

datos$f_Frecuencia_Juego <- factor(datos$Frecuencia_Juego,
                                      levels = c('1 hora al día',
                                                 '2-3 horas al día',
                                                 '3-4 horas al día',
                                                 '5 horas o más'))

datos$f_Plataforma_Juego <- factor(datos$Plataforma_Juego,
                                      levels = c('Nintendo Switch',
                                                 'Xbox Series X / Xbox Series S',
                                                 'PlayStation 4 / PlayStation 5',
                                                 'PC',
                                                 'Dispositivo móvil'))

datos$f_Tipo_Juegos <- factor(datos$Tipo_Juegos,
                                      levels = c('Juegos para un único jugador',
                                                 'Juegos multijugador de carácter cooperativo',
                                                 'Juegos multijugador de carácter competitivo'),
                                      labels = c("Singleplayer", 
                                                 "Cooperativo",
                                                 "Competitivo"))

datos$f_Interes_SinglePlayer <- factor(datos$Interes_SinglePlayer,
                                        levels = c('Sí','No'))

datos$f_Razon_NoSinglePLayer <- as.factor(datos$Razon_NoSinglePLayer)

datos$f_Conoce_MV <- factor(datos$Conoce_MV,
                                      levels = c('Sí','No'))

datos$f_Ha_JugadoMV <- factor(datos$Ha_JugadoMV,
                                      levels = c('Sí','No'))

datos$f_Jugaria_MV <- factor(datos$Jugaria_MV,
                              levels = c('Sí','No'))

datos$f_Num_Mecanicas <- factor(datos$Num_Mecanicas,
                             levels = c('Prefiero los videojuegos que tengan un número elevado y variado de mecánicas, aunque no todas sean esenciales para progresar en el juego.',
                                        'Prefiero los videojuegos que tienen un número reducido de mecánicas, que puedan ir perfeccionándose a medida que avanza la partida.'),
                             labels = c("Muchas", 
                                        "Pocas"))

datos$f_Complejidad_Mecanicas <- factor(datos$Complejidad_Mecanicas,
                              levels = c('Prefiero jugar a videojuegos con mecánicas complejas,  que me hagan pensar y que requieran una cierta pericia para poder progresar en el juego.',
                                         'Prefiero jugar videojuegos con mecánicas más simples, con el objetivo de poder centrarme más en disfrutar otros aspectos como la historia, pero sin dejar de lado el desafío que pueda ofrecer.'),
                              labels = c("Complejas", 
                                         "Sencillas"))

datos$f_Estilo_preferido <- factor(datos$Estilo_preferido,
                             levels = c('Estilo 1','Estilo 2','Estilo 3','Estilo 4'),
                             labels = c("Dibujo detallado complejo", 
                                        "Pixel-Art 32x32",
                                        "Pixel-Art 64x64",
                                        "Dibujo detallado simplificado"))

datos$f_Aceptacion_Oniros <- factor(datos$Aceptacion_Oniros,
                                       levels = c('Sí','No'))

datos$f_GeneroJug <- factor(datos$GeneroJug,
                                   levels = c('Hombre',
                                              'Mujer',
                                              'No binario',
                                              'Prefiero no decirlo'))

datos$f_EdadJug <- as.factor(datos$EdadJug)
datos$f_EdadJug <- as.numeric(as.character(datos$f_EdadJug))

datos$f_SituacionLaboralJug <- factor(datos$SituacionLaboralJug,
                                    levels = c('Estudiando',
                                               'Trabajando',
                                               'Ambas',
                                               'Ninguna'))

datos$f_Email <- as.factor(datos$Email)

datos$f_GeneroNoJug <- factor(datos$GeneroNoJug,
                            levels = c('Hombre',
                                       'Mujer',
                                       'No binario',
                                       'Prefiero no decirlo'))

datos$f_EdadNoJug <- as.factor(datos$EdadNoJug)
datos$f_EdadNoJug <- as.numeric(as.character(datos$f_EdadNoJug))

datos$f_SituacionLaboralNoJug <- factor(datos$SituacionLaboralNoJug,
                                      levels = c('Estudiando',
                                                 'Trabajando',
                                                 'Ambas',
                                                 'Ninguna'))

datos$f_GeneroNoMV <- factor(datos$GeneroNoMV,
                            levels = c('Hombre',
                                       'Mujer',
                                       'No binario',
                                       'Prefiero no decirlo'))

datos$f_EdadNoMV <- as.factor(datos$EdadNoMV)

datos$f_SituacionLaboralNoMV <- factor(datos$SituacionLaboralNoMV,
                                      levels = c('Estudiando',
                                                 'Trabajando',
                                                 'Ambas',
                                                 'Ninguna'))

#2 - Transformación de texto a factores -- PREGUNTAS CERRADAS DE SELECCIÓN MÚLTIPLE
datos <- cSplit_e(datos,'No_Juega_Razon', sep = ', ', type = 'character', fill = 0)
datos <- cSplit_e(datos,'MV_Jugados', sep = ', ', type = 'character', fill = 0)
datos <- cSplit_e(datos,'MV_Conocidos', sep = ', ', type = 'character', fill = 0)
datos <- cSplit_e(datos,'Oniros_destacado', sep = ', ', type = 'character', fill = 0)
datos <- cSplit_e(datos,'HobbiesJug', sep = ', ', type = 'character', fill = 0)
datos <- cSplit_e(datos,'HobbiesNoJug', sep = ', ', type = 'character', fill = 0)
datos <- cSplit_e(datos,'HobbiesNoMV', sep = ', ', type = 'character', fill = 0)


#3 - Transformación de texto a factores -- ESCALAS LIKERT
datos$f_Likert_Narrativa1 <- 
  factor( datos$Likert_Narrativa1, 
          levels = c('Totalmente en desacuerdo', 'En desacuerdo', 'Indiferente',
                     'De acuerdo', 'Totalmente de acuerdo'),
          labels = c('-10', '-5', '0', '5', '10'))

datos$f_Likert_Narrativa2 <- 
  factor( datos$Likert_Narrativa2, 
          levels = c('Totalmente en desacuerdo', 'En desacuerdo', 'Indiferente',
                     'De acuerdo', 'Totalmente de acuerdo'),
          labels = c('-10', '-5', '0', '5', '10'))

datos$f_Likert_Narrativa3 <- 
  factor( datos$Likert_Narrativa3, 
          levels = c('Totalmente en desacuerdo', 'En desacuerdo', 'Indiferente',
                     'De acuerdo', 'Totalmente de acuerdo'),
          labels = c('-10', '-5', '0', '5', '10'))

datos$f_Likert_Narrativa4 <- 
  factor( datos$Likert_Narrativa4, 
          levels = c('Totalmente en desacuerdo', 'En desacuerdo', 'Indiferente',
                     'De acuerdo', 'Totalmente de acuerdo'),
          labels = c('10', '5', '0', '-5', '-10'))

datos$f_Likert_Narrativa5 <- 
  factor( datos$Likert_Narrativa5, 
          levels = c('Totalmente en desacuerdo', 'En desacuerdo', 'Indiferente',
                     'De acuerdo', 'Totalmente de acuerdo'),
          labels = c('-10', '-5', '0', '5', '10'))

datos$f_Likert_Narrativa6 <- 
  factor( datos$Likert_Narrativa6, 
          levels = c('Totalmente en desacuerdo', 'En desacuerdo', 'Indiferente',
                     'De acuerdo', 'Totalmente de acuerdo'),
          labels = c('10', '5', '0', '-5', '-10'))

datos$score_Likert_Narrativa1 <- as.numeric(as.character(datos$f_Likert_Narrativa1))
datos$score_Likert_Narrativa2 <- as.numeric(as.character(datos$f_Likert_Narrativa2))
datos$score_Likert_Narrativa3 <- as.numeric(as.character(datos$f_Likert_Narrativa3))
datos$score_Likert_Narrativa4 <- as.numeric(as.character(datos$f_Likert_Narrativa4))
datos$score_Likert_Narrativa5 <- as.numeric(as.character(datos$f_Likert_Narrativa5))
datos$score_Likert_Narrativa6 <- as.numeric(as.character(datos$f_Likert_Narrativa6))

datos$Narrativa <- (datos$score_Likert_Narrativa1 + datos$score_Likert_Narrativa2 + datos$score_Likert_Narrativa3
                    + datos$score_Likert_Narrativa4 + datos$score_Likert_Narrativa5 + datos$score_Likert_Narrativa6)


datos$f_Likert_Jugabilidad1 <- 
  factor( datos$Likert_Jugabilidad1, 
          levels = c('Totalmente en desacuerdo', 'En desacuerdo', 'Indiferente',
                     'De acuerdo', 'Totalmente de acuerdo'),
          labels = c('-10', '-5', '0', '5', '10'))

datos$f_Likert_Jugabilidad2 <- 
  factor( datos$Likert_Jugabilidad2, 
          levels = c('Totalmente en desacuerdo', 'En desacuerdo', 'Indiferente',
                     'De acuerdo', 'Totalmente de acuerdo'),
          labels = c('-10', '-5', '0', '5', '10'))

datos$f_Likert_Jugabilidad3 <- 
  factor( datos$Likert_Jugabilidad3, 
          levels = c('Totalmente en desacuerdo', 'En desacuerdo', 'Indiferente',
                     'De acuerdo', 'Totalmente de acuerdo'),
          labels = c('-10', '-5', '0', '5', '10'))

datos$f_Likert_Jugabilidad4 <- 
  factor( datos$Likert_Jugabilidad4, 
          levels = c('Totalmente en desacuerdo', 'En desacuerdo', 'Indiferente',
                     'De acuerdo', 'Totalmente de acuerdo'),
          labels = c('-10', '-5', '0', '5', '10'))

datos$score_Likert_Jugabilidad1 <- as.numeric(as.character(datos$f_Likert_Jugabilidad1))
datos$score_Likert_Jugabilidad2 <- as.numeric(as.character(datos$f_Likert_Jugabilidad2))
datos$score_Likert_Jugabilidad3 <- as.numeric(as.character(datos$f_Likert_Jugabilidad3))
datos$score_Likert_Jugabilidad4 <- as.numeric(as.character(datos$f_Likert_Jugabilidad4))

datos$Jugabilidad <- (datos$score_Likert_Jugabilidad1 + datos$score_Likert_Jugabilidad2 + 
                        datos$score_Likert_Jugabilidad3 + datos$score_Likert_Jugabilidad4)


datos$f_Likert_EstiloVis1 <- 
  factor( datos$Likert_EstiloVis1, 
          levels = c('Totalmente en desacuerdo', 'En desacuerdo', 'Indiferente',
                     'De acuerdo', 'Totalmente de acuerdo'),
          labels = c('10', '5', '0', '-5', '-10'))

datos$f_Likert_EstiloVis2 <- 
  factor( datos$Likert_EstiloVis2, 
          levels = c('Totalmente en desacuerdo', 'En desacuerdo', 'Indiferente',
                     'De acuerdo', 'Totalmente de acuerdo'),
          labels = c('-10', '-5', '0', '5', '10'))

datos$f_Likert_EstiloVis3 <- 
  factor( datos$Likert_EstiloVis3, 
          levels = c('Totalmente en desacuerdo', 'En desacuerdo', 'Indiferente',
                     'De acuerdo', 'Totalmente de acuerdo'),
          labels = c('10', '5', '0', '-5', '-10'))

datos$score_Likert_EstiloVis1 <- as.numeric(as.character(datos$f_Likert_EstiloVis1))
datos$score_Likert_EstiloVis2 <- as.numeric(as.character(datos$f_Likert_EstiloVis2))
datos$score_Likert_EstiloVis3 <- as.numeric(as.character(datos$f_Likert_EstiloVis3))

datos$EstiloVis <- (datos$score_Likert_EstiloVis1 + datos$score_Likert_EstiloVis2 + datos$score_Likert_EstiloVis3)


datos$f_Likert_SoftSkills1 <- 
  factor( datos$Likert_SoftSkills1, 
          levels = c('Totalmente en desacuerdo', 'En desacuerdo', 'Indiferente',
                     'De acuerdo', 'Totalmente de acuerdo'),
          labels = c('-10', '-5', '0', '5', '10'))

datos$f_Likert_SoftSkills2 <- 
  factor( datos$Likert_SoftSkills2, 
          levels = c('Totalmente en desacuerdo', 'En desacuerdo', 'Indiferente',
                     'De acuerdo', 'Totalmente de acuerdo'),
          labels = c('-10', '-5', '0', '5', '10'))

datos$f_Likert_SoftSkills3 <- 
  factor( datos$Likert_SoftSkills3, 
          levels = c('Totalmente en desacuerdo', 'En desacuerdo', 'Indiferente',
                     'De acuerdo', 'Totalmente de acuerdo'),
          labels = c('-10', '-5', '0', '5', '10'))

datos$f_Likert_SoftSkills4 <- 
  factor( datos$Likert_SoftSkills4, 
          levels = c('Totalmente en desacuerdo', 'En desacuerdo', 'Indiferente',
                     'De acuerdo', 'Totalmente de acuerdo'),
          labels = c('-10', '-5', '0', '5', '10'))

datos$f_Likert_SoftSkills5 <- 
  factor( datos$Likert_SoftSkills5, 
          levels = c('Totalmente en desacuerdo', 'En desacuerdo', 'Indiferente',
                     'De acuerdo', 'Totalmente de acuerdo'),
          labels = c('-10', '-5', '0', '5', '10'))

datos$score_Likert_SoftSkills1 <- as.numeric(as.character(datos$f_Likert_SoftSkills1))
datos$score_Likert_SoftSkills2 <- as.numeric(as.character(datos$f_Likert_SoftSkills2))
datos$score_Likert_SoftSkills3 <- as.numeric(as.character(datos$f_Likert_SoftSkills3))
datos$score_Likert_SoftSkills4 <- as.numeric(as.character(datos$f_Likert_SoftSkills4))
datos$score_Likert_SoftSkills5 <- as.numeric(as.character(datos$f_Likert_SoftSkills5))

datos$SoftSkills <- (datos$score_Likert_SoftSkills1 + datos$score_Likert_SoftSkills2 + datos$score_Likert_SoftSkills3
                    + datos$score_Likert_SoftSkills4 + datos$score_Likert_SoftSkills5)


datos$f_Likert_IntEmocional1 <- 
  factor( datos$Likert_IntEmocional1, 
          levels = c('Totalmente en desacuerdo', 'En desacuerdo', 'Indiferente',
                     'De acuerdo', 'Totalmente de acuerdo'),
          labels = c('-10', '-5', '0', '5', '10'))

datos$f_Likert_IntEmocional2 <- 
  factor( datos$Likert_IntEmocional2, 
          levels = c('Totalmente en desacuerdo', 'En desacuerdo', 'Indiferente',
                     'De acuerdo', 'Totalmente de acuerdo'),
          labels = c('-10', '-5', '0', '5', '10'))

datos$f_Likert_IntEmocional3 <- 
  factor( datos$Likert_IntEmocional3, 
          levels = c('Totalmente en desacuerdo', 'En desacuerdo', 'Indiferente',
                     'De acuerdo', 'Totalmente de acuerdo'),
          labels = c('-10', '-5', '0', '5', '10'))

datos$f_Likert_IntEmocional4 <- 
  factor( datos$Likert_IntEmocional4, 
          levels = c('Totalmente en desacuerdo', 'En desacuerdo', 'Indiferente',
                     'De acuerdo', 'Totalmente de acuerdo'),
          labels = c('-10', '-5', '0', '5', '10'))

datos$f_Likert_IntEmocional5 <- 
  factor( datos$Likert_IntEmocional5, 
          levels = c('Totalmente en desacuerdo', 'En desacuerdo', 'Indiferente',
                     'De acuerdo', 'Totalmente de acuerdo'),
          labels = c('-10', '-5', '0', '5', '10'))

datos$f_Likert_IntEmocional6 <- 
  factor( datos$Likert_IntEmocional6, 
          levels = c('Totalmente en desacuerdo', 'En desacuerdo', 'Indiferente',
                     'De acuerdo', 'Totalmente de acuerdo'),
          labels = c('-10', '-5', '0', '5', '10'))

datos$f_Likert_IntEmocional7 <- 
  factor( datos$Likert_IntEmocional7, 
          levels = c('Totalmente en desacuerdo', 'En desacuerdo', 'Indiferente',
                     'De acuerdo', 'Totalmente de acuerdo'),
          labels = c('-10', '-5', '0', '5', '10'))

datos$score_Likert_IntEmocional1 <- as.numeric(as.character(datos$f_Likert_IntEmocional1))
datos$score_Likert_IntEmocional2 <- as.numeric(as.character(datos$f_Likert_IntEmocional2))
datos$score_Likert_IntEmocional3 <- as.numeric(as.character(datos$f_Likert_IntEmocional3))
datos$score_Likert_IntEmocional4 <- as.numeric(as.character(datos$f_Likert_IntEmocional4))
datos$score_Likert_IntEmocional5 <- as.numeric(as.character(datos$f_Likert_IntEmocional5))
datos$score_Likert_IntEmocional6 <- as.numeric(as.character(datos$f_Likert_IntEmocional6))
datos$score_Likert_IntEmocional7 <- as.numeric(as.character(datos$f_Likert_IntEmocional7))

datos$IntEmocional <- (datos$score_Likert_IntEmocional1 + datos$score_Likert_IntEmocional2 + datos$score_Likert_IntEmocional3
                    + datos$score_Likert_IntEmocional4 + datos$score_Likert_IntEmocional5 + datos$score_Likert_IntEmocional6
                    + datos$score_Likert_IntEmocional7)

View(datos)

#4 - Control de consistencia de la encuesta
datos <- datos[, 59:ncol(datos)]

condition <- (datos[, 33] == 0)
datos <- datos[condition, ]

condition <- (datos[, 42] == 0)
datos <- datos[condition, ]

#5 - Eliminación de columnas innecesarias
cols_to_remove <- grep("f_Likert", names(datos))
datos <- datos[, -cols_to_remove]

cols_to_remove <- grep("score_", names(datos))
datos <- datos[, -cols_to_remove]

cols_to_remove <- grep("EA FC 24", names(datos))
datos <- datos[, -cols_to_remove]

cols_to_remove <- grep("Final Fantasy VII", names(datos))
datos <- datos[, -cols_to_remove]

#6 - Corrección y resumen de respuestas similares a opciones predeterminadas
filas_b_iguales_1 <- datos$`Oniros_destacado_La aportación educativa,` == 1
datos$`Oniros_destacado_La aportación educativa`[filas_b_iguales_1] <- datos$`Oniros_destacado_La aportación educativa`[filas_b_iguales_1] + datos$`Oniros_destacado_La aportación educativa,`[filas_b_iguales_1]
datos <- datos[, !names(datos) %in% "Oniros_destacado_La aportación educativa,"]


filas_b_iguales_1 <- datos[["Oniros_destacado_Me encanta la propuesta y mas que tenga una parte de salud mental. Es algo que falta y que aporta mucho 🩵🧠"]] == 1
datos$`Oniros_destacado_La aportación educativa`[filas_b_iguales_1] <- datos$`Oniros_destacado_La aportación educativa`[filas_b_iguales_1] + datos[["Oniros_destacado_Me encanta la propuesta y mas que tenga una parte de salud mental. Es algo que falta y que aporta mucho 🩵🧠"]][filas_b_iguales_1]
datos <- datos[, !names(datos) %in% "Oniros_destacado_Me encanta la propuesta y mas que tenga una parte de salud mental. Es algo que falta y que aporta mucho 🩵🧠"]


filas_b_iguales_1 <- datos$`Oniros_destacado_La jugabilidad,` == 1
datos$`Oniros_destacado_La jugabilidad`[filas_b_iguales_1] <- datos$`Oniros_destacado_La jugabilidad`[filas_b_iguales_1] + datos$`Oniros_destacado_La jugabilidad,`[filas_b_iguales_1]
datos <- datos[, !names(datos) %in% "Oniros_destacado_La jugabilidad,"]


datos$`HobbiesJug_Quedar con amigos` <- datos$`HobbiesJug_Quedar con amigos` + datos$HobbiesJug_Colegas + datos$`HobbiesJug_Pasar el rato con los amigos`
datos <- datos[, !names(datos) %in% "HobbiesJug_Colegas"]
datos <- datos[, !names(datos) %in% "HobbiesJug_Pasar el rato con los amigos"]



datos$`HobbiesJug_Proyectos personales` <- datos$`HobbiesJug_Proyectos personales` + datos$`HobbiesJug_Trabajo en mi portfolio`
datos <- datos[, !names(datos) %in% "HobbiesJug_Trabajo en mi portfolio"]


datos$`HobbiesJug_Artes plasticas, escritura y musica` <- datos$`HobbiesJug_Dibujar y la música` + datos$`HobbiesJug_Escribo y dibujo` + datos$`HobbiesJug_Manualidades`
datos <- datos[, !names(datos) %in% "HobbiesJug_Dibujar y la música"]
datos <- datos[, !names(datos) %in% "HobbiesJug_Escribo y dibujo"]
datos <- datos[, !names(datos) %in% "HobbiesJug_Manualidades"]


fila_anyadir_A <- datos[["HobbiesJug_Un poco de todo 😅"]] == 1
fila_anyadir_B <- datos[["HobbiesJug_Un poco de todas"]] == 1
datos$`HobbiesJug_Practico deporte o actividades más físicas`[fila_anyadir_A | fila_anyadir_B] <- 1
datos$`HobbiesJug_Dedico mucho tiempo a la lectura o al estudio autónomo`[fila_anyadir_A | fila_anyadir_B] <- 1
datos$`HobbiesJug_Juego a videojuegos`[fila_anyadir_A | fila_anyadir_B] <- 1
datos$`HobbiesJug_Veo películas o series`[fila_anyadir_A | fila_anyadir_B] <- 1
datos <- datos[, !names(datos) %in% "HobbiesJug_Un poco de todo 😅"]
datos <- datos[, !names(datos) %in% "HobbiesJug_Un poco de todas"]


datos <- datos %>%
  mutate(`Actividades relacionadas con la naturaleza` = coalesce(`HobbiesNoJug_Caminar y cuidar el jardin`, HobbiesNoJug_naturaleza, `HobbiesNoJug_Vida espiritual`))

fila_anyadir_A <- datos[["HobbiesNoJug_Veo obras"]] == 1
datos$`HobbiesNoJug_actividades culturales`[fila_anyadir_A] <- 1

fila_anyadir_A <- datos[["HobbiesNoJug_Programo"]] == 1
fila_anyadir_B <- datos[["HobbiesNoJug_Programo y aprendo nuevas tecnologías"]] == 1
fila_anyadir_C <- datos[["HobbiesNoJug_Trabajo"]] == 1
datos$`HobbiesNoJug_Dedico mucho tiempo a la lectura o al estudio autónomo`[fila_anyadir_A | fila_anyadir_B | fila_anyadir_C] <- 1

fila_anyadir_A <- datos[["HobbiesNoJug_Pasear"]] == 1
fila_anyadir_B <- datos[["HobbiesNoJug_Salir a pasear"]] == 1
datos$`HobbiesNoJug_Practico deporte o actividades más físicas`[fila_anyadir_A | fila_anyadir_B] <- 1

datos <- datos[, !names(datos) %in% "HobbiesNoJug_Caminar y cuidar el jardin"]
datos <- datos[, !names(datos) %in% "HobbiesNoJug_naturaleza"]
datos <- datos[, !names(datos) %in% "HobbiesNoJug_Vida espiritual"]
datos <- datos[, !names(datos) %in% "HobbiesNoJug_Veo obras"]
datos <- datos[, !names(datos) %in% "HobbiesNoJug_Programo"]
datos <- datos[, !names(datos) %in% "HobbiesNoJug_Programo y aprendo nuevas tecnologías"]
datos <- datos[, !names(datos) %in% "HobbiesNoJug_Trabajo"]
datos <- datos[, !names(datos) %in% "HobbiesNoJug_Pasear"]
datos <- datos[, !names(datos) %in% "HobbiesNoJug_Salir a pasear"]
View(datos)

#7 - Subconjunto de datasets
#Dataset 1: jugadores
subset_Jugadores <- subset(datos, f_Aceptacion_Oniros == "Sí")
View(subset_Jugadores)
#Preprocesado eliminando edades anómalas
Q1 <- quantile(subset_Jugadores$f_EdadJug, 0.25)
Q3 <- quantile(subset_Jugadores$f_EdadJug, 0.75)
IQR <- IQR(subset_Jugadores$f_EdadJug)
lower_whisker <- max(min(subset_Jugadores$f_EdadJug), Q1 - 1.5 * IQR)
upper_whisker <- min(max(subset_Jugadores$f_EdadJug), Q3 + 1.5 * IQR)
#Boxplot - Ver puntos anómalos de edad en la muestra
jpeg("C:/Users/USUARIO/Documents/GitHub/Trabajos-de-Final-de-Grado/TFG_ADE/boxplot_edad_jug.jpg", width = 1000, quality = 90)
ggplot(subset_Jugadores, aes(y = f_EdadJug)) +
  geom_boxplot() +
  geom_text(aes(x = 1, y = Q1, label = paste("Q1: ", Q1)), vjust = -0.5, color = "blue") +
  geom_text(aes(x = 1, y = Q3, label = paste("Q3: ", Q3)), vjust = -0.5, color = "blue") +
  geom_text(aes(x = 1, y = lower_whisker, label = paste("Lower Whisker: ", lower_whisker)), vjust = 1.5, color = "red") +
  geom_text(aes(x = 1, y = upper_whisker, label = paste("Upper Whisker: ", upper_whisker)), vjust = -0.5, color = "red") +
  labs(title = "Distribución de Edad de Jugadores", y = "Edad") +
  theme_minimal()
dev.off()
subset_Jugadores <- subset(subset_Jugadores, f_EdadJug < 54.5)

#Dataset 2: No jugadores
subset_NoJugadores <- subset(datos, !is.na(f_EdadNoJug))
View(subset_NoJugadores)
#Preprocesado eliminando edades anómalas
Q1 <- quantile(subset_NoJugadores$f_EdadNoJug, 0.25)
Q3 <- quantile(subset_NoJugadores$f_EdadNoJug, 0.75)
IQR <- IQR(subset_NoJugadores$f_EdadNoJug)
lower_whisker <- max(min(subset_NoJugadores$f_EdadNoJug), Q1 - 1.5 * IQR)
upper_whisker <- min(max(subset_NoJugadores$f_EdadNoJug), Q3 + 1.5 * IQR)
#Boxplot - Ver puntos anómalos de edad en la muestra
jpeg("C:/Users/USUARIO/Documents/GitHub/Trabajos-de-Final-de-Grado/TFG_ADE/boxplot_edad_nojug.jpg", width = 1500, quality = 90)
ggplot(subset_NoJugadores, aes(y = f_EdadNoJug)) +
  geom_boxplot() +
  geom_text(aes(x = 1, y = Q1, label = paste("Q1: ", Q1)), vjust = -0.5, color = "blue") +
  geom_text(aes(x = 1, y = Q3, label = paste("Q3: ", Q3)), vjust = -0.5, color = "blue") +
  geom_text(aes(x = 1, y = lower_whisker, label = paste("Lower Whisker: ", lower_whisker)), vjust = 1.5, color = "red") +
  geom_text(aes(x = 1, y = upper_whisker, label = paste("Upper Whisker: ", upper_whisker)), vjust = -0.5, color = "red") +
  labs(title = "Distribución de Edad de No Jugadores", y = "Edad") +
  theme_minimal()
dev.off()


# ANÁLISIS DE LOS OBJETIVOS

#1. ANÁLISIS DE LOS PERFILES DE LA MUESTRA
#1.1. ANÁLISIS DE LOS SI JUGADORES (CHECK)
#Gráfica de tarta - Género (CHECK)
genero_frecuencias <- as.data.frame(table(subset_Jugadores$f_GeneroJug))
colnames(genero_frecuencias) <- c("Genero", "Frecuencia")
jpeg("C:/Users/USUARIO/Documents/GitHub/Trabajos-de-Final-de-Grado/TFG_ADE/tarta_género_jug.jpg", quality = 90)
ggplot(genero_frecuencias, aes(x = "", y = Frecuencia, fill = Genero)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Distribución por Género", fill = "Género") +
  geom_text(aes(label = Frecuencia), position = position_stack(vjust = 0.5))
dev.off()


#Histograma de frecuencias - Frecuencia de juego (CHECK)
time_frecuencias <- table(subset_Jugadores$f_Frecuencia_Juego)
jpeg("C:/Users/USUARIO/Documents/GitHub/Trabajos-de-Final-de-Grado/TFG_ADE/histograma_freq-juego_jug.jpg", width = 1200, quality = 90)
barplot(time_frecuencias, main = "Histograma de Frecuencias por Frecuencia de Juego", 
        xlab = "Frecuencia de juego", ylab = "Conteo", col = "blue", ylim = c(0, max(time_frecuencias) + 1))
text(x = barplot(time_frecuencias, plot = FALSE), y = time_frecuencias, 
     label = time_frecuencias, pos = 3, cex = 0.8, col = "black")
dev.off()

#Gráfica de tarta - Plataforma de juego (CHECK)
plataforma_frecuencias <- as.data.frame(table(subset_Jugadores$f_Plataforma_Juego))
colnames(plataforma_frecuencias) <- c("Plataforma", "Frecuencia")
jpeg("C:/Users/USUARIO/Documents/GitHub/Trabajos-de-Final-de-Grado/TFG_ADE/tarta_plataforma_jug.jpg", quality = 90)
ggplot(plataforma_frecuencias, aes(x = "", y = Frecuencia, fill = Plataforma)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Distribución por Plataforma", fill = "Plataforma") +
  geom_text(aes(label = Frecuencia), position = position_stack(vjust = 0.5))
dev.off()

#Histograma de frecuencias - Hobbies de los jugadores (CHECK)
subset_HobbiesJug <- datos %>%
  select(contains("HobbiesJug_"))
frecuencias <- colSums(subset_HobbiesJug == 1)
df_frecuencias <- data.frame(
  Hobbie = names(frecuencias),
  Frecuencia = as.vector(frecuencias)
)
jpeg("C:/Users/USUARIO/Documents/GitHub/Trabajos-de-Final-de-Grado/TFG_ADE/hist_hobbies.jpg", width = 2400, quality = 90)
ggplot(df_frecuencias, aes(x = Hobbie, y = Frecuencia)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = Frecuencia), vjust = -0.3) +
  labs(title = "Frecuencia de Hobbies de los jugadores", x = "Hobbie", y = "Frecuencia") +
  theme_minimal()
dev.off()

#Histograma de frecuencias - Edad de los jugadores (CHECK)
n <- length(subset_Jugadores$f_EdadJug)
num_intervals_sturges <- ceiling(log2(n) + 1)
print(num_intervals_sturges)

breaks <- c(11, 16, 21, 26, 31, 36, 41, 46, Inf)
labels <- c("[11-16)", "[16-21)", "[21-26)", "[26-31)", "[31-36)", "[36-41)", "[41-46)", ">= 46")
subset_Jugadores$AgeGroup <- cut(subset_Jugadores$f_EdadJug, 
                                 breaks = breaks, 
                                 labels = labels, 
                                 right = FALSE, 
                                 include.lowest = TRUE)
tipo_frecuencias <- table(subset_Jugadores$AgeGroup)
jpeg("C:/Users/USUARIO/Documents/GitHub/Trabajos-de-Final-de-Grado/TFG_ADE/hist_edad_jug.jpg", width = 1200, quality = 90)
barplot(tipo_frecuencias, main = "Histograma de Frecuencias por Rango de edades", 
        xlab = "Rango de edad", ylab = "Frecuencia", col = "blue", ylim = c(0, 35))
text(x = barplot(tipo_frecuencias, plot = FALSE), y = tipo_frecuencias, 
     label = tipo_frecuencias, pos = 3, cex = 0.8, col = "black")
dev.off()

#Histograma de frecuencias - Situación Laboral (CHECK)
laburo_frecuencias <- table(subset_Jugadores$f_SituacionLaboralJug)
jpeg("C:/Users/USUARIO/Documents/GitHub/Trabajos-de-Final-de-Grado/TFG_ADE/hist_laboral_jug.jpg", quality = 90)
barplot(laburo_frecuencias, main = "Histograma de Frecuencias por Situación Laboral", 
        xlab = "Situación Laboral", ylab = "Frecuencia", col = "blue", ylim = c(0, 35))
text(x = barplot(laburo_frecuencias, plot = FALSE), y = laburo_frecuencias, 
     label = laburo_frecuencias, pos = 3, cex = 0.8, col = "black")
dev.off()

#Gráfica de tarta - Relación con los videojuegos (CHECK)
subset_Jugadores <- subset_Jugadores %>%
  mutate(tipo_jugador = case_when(
    f_Ha_Jugado_Videojuegos == "Sí" & f_Jugaria_Videojuegos == "Sí" ~ "antiguo jugador",
    f_Juega_Videojuegos == "Sí" ~ "jugador activo",
    f_Ha_Jugado_Videojuegos == "No" & f_Jugaria_Videojuegos == "Sí" ~ "nuevo jugador potencial",
    TRUE ~ NA_character_  # Para casos que no cumplen ninguna condición
  ))
plataforma_frecuencias <- as.data.frame(table(subset_Jugadores$tipo_jugador))
colnames(plataforma_frecuencias) <- c("Relación con los videojuegos", "Frecuencia")
jpeg("C:/Users/USUARIO/Documents/GitHub/Trabajos-de-Final-de-Grado/TFG_ADE/tarta_relacion_jug.jpg", quality = 90)
ggplot(plataforma_frecuencias, aes(x = "", y = Frecuencia, fill = `Relación con los videojuegos`)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Distribución por Tipo de jugador", fill = "Tipo de jugador") +
  geom_text(aes(label = Frecuencia), position = position_stack(vjust = 0.5))
dev.off()

#Histograma de frecuencias - Tipo de juegos (CHECK)
tipo_frecuencias <- table(subset_Jugadores$f_Tipo_Juegos)
jpeg("C:/Users/USUARIO/Documents/GitHub/Trabajos-de-Final-de-Grado/TFG_ADE/hist_tipo-juegos_jug.jpg", quality = 90)
barplot(tipo_frecuencias, main = "Histograma de Frecuencias por Tipo de videojuego", 
        xlab = "Tipo de videojuego", ylab = "Frecuencia", col = "blue", ylim = c(0, 35))
text(x = barplot(tipo_frecuencias, plot = FALSE), y = tipo_frecuencias, 
     label = tipo_frecuencias, pos = 3, cex = 0.8, col = "black")
dev.off()

#1.2. ANÁLISIS DE LOS NO JUGADORES
#Gráfica de tarta - Género (CHECK)
genero_frecuencias <- as.data.frame(table(subset_NoJugadores$f_GeneroNoJug))
colnames(genero_frecuencias) <- c("Genero", "Frecuencia")
jpeg("C:/Users/USUARIO/Documents/GitHub/Trabajos-de-Final-de-Grado/TFG_ADE/tarta_género_nojug.jpg", width = 900, quality = 90)
ggplot(genero_frecuencias, aes(x = "", y = Frecuencia, fill = Genero)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Distribución por Género", fill = "Género") +
  geom_text(aes(label = Frecuencia), position = position_stack(vjust = 0.5))
dev.off()

#Histograma de frecuencias - Edad de los jugadores (CHECK)
n <- length(subset_NoJugadores$f_EdadNoJug)
num_intervals_sturges <- ceiling(log2(n) + 1)
print(num_intervals_sturges)

breaks <- c(15, 23, 31, 39, 47, 55, 62, Inf)
labels <- c("[15-23)", "[23-31)", "[31-39)", "[39-47)", "[47-55)", "[55-62)", ">= 62")
subset_NoJugadores$AgeGroup <- cut(subset_NoJugadores$f_EdadNoJug, 
                                 breaks = breaks, 
                                 labels = labels, 
                                 right = FALSE, 
                                 include.lowest = TRUE)
tipo_frecuencias <- table(subset_NoJugadores$AgeGroup)
jpeg("C:/Users/USUARIO/Documents/GitHub/Trabajos-de-Final-de-Grado/TFG_ADE/hist_edad_nojug.jpg", width = 1200, quality = 90)
barplot(tipo_frecuencias, main = "Histograma de Frecuencias por Rango de edades", 
        xlab = "Rango de edad", ylab = "Frecuencia", col = "blue", ylim = c(0, 35))
text(x = barplot(tipo_frecuencias, plot = FALSE), y = tipo_frecuencias, 
     label = tipo_frecuencias, pos = 3, cex = 0.8, col = "black")
dev.off()

#Histograma de frecuencias - Situación Laboral (CHECK)
laburo_frecuencias <- table(subset_NoJugadores$f_SituacionLaboralNoJug)
jpeg("C:/Users/USUARIO/Documents/GitHub/Trabajos-de-Final-de-Grado/TFG_ADE/hist_laboral_nojug.jpg", quality = 90)
barplot(laburo_frecuencias, main = "Histograma de Frecuencias por Situación Laboral", 
        xlab = "Situación Laboral", ylab = "Frecuencia", col = "blue", ylim = c(0, 35))
text(x = barplot(laburo_frecuencias, plot = FALSE), y = laburo_frecuencias, 
     label = laburo_frecuencias, pos = 3, cex = 0.8, col = "black")
dev.off()

#Histograma de frecuencias - Hobbies de los jugadores (CHECK)
subset_HobbiesNoJug <- datos %>%
  select(contains("HobbiesNoJug_"))
View(subset_HobbiesNoJug)
frecuencias <- colSums(subset_HobbiesNoJug == 1)
df_frecuencias <- data.frame(
  Hobbie = names(frecuencias),
  Frecuencia = as.vector(frecuencias)
)
jpeg("C:/Users/USUARIO/Documents/GitHub/Trabajos-de-Final-de-Grado/TFG_ADE/hist_hobbies.jpg", width = 2400, quality = 90)
ggplot(df_frecuencias, aes(x = Hobbie, y = Frecuencia)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = Frecuencia), vjust = -0.3) +
  labs(title = "Frecuencia de Hobbies de los no jugadores", x = "Hobbie", y = "Frecuencia") +
  theme_minimal()
dev.off()

#Gráfica de tarta - Relación con los videojuegos (CHECK)
subset_NoJugadores <- subset_NoJugadores %>%
  mutate(tipo_jugador = case_when(
    f_Ha_Jugado_Videojuegos == "Sí" & f_Jugaria_Videojuegos == "Sí" ~ "antiguo jugador",
    f_Juega_Videojuegos == "Sí" ~ "jugador activo",
    f_Ha_Jugado_Videojuegos == "No" & f_Jugaria_Videojuegos == "Sí" ~ "nuevo jugador potencial",
    f_Ha_Jugado_Videojuegos == "No" & f_Jugaria_Videojuegos == "No" ~ "no tiene interés en videojuegos",
    TRUE ~ NA_character_  # Para casos que no cumplen ninguna condición
  ))
plataforma_frecuencias <- as.data.frame(table(subset_NoJugadores$tipo_jugador))
colnames(plataforma_frecuencias) <- c("Relación con los videojuegos", "Frecuencia")
jpeg("C:/Users/USUARIO/Documents/GitHub/Trabajos-de-Final-de-Grado/TFG_ADE/tarta_relacion_nojug.jpg", width = 900, quality = 90)
ggplot(plataforma_frecuencias, aes(x = "", y = Frecuencia, fill = `Relación con los videojuegos`)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Distribución por Tipo de jugador", fill = "Tipo de jugador") +
  geom_text(aes(label = Frecuencia), position = position_stack(vjust = 0.5))
dev.off()

#Histograma de frecuencias - Frecuencia de juego (CHECK) 
time_frecuencias <- table(subset_NoJugadores$f_Frecuencia_Juego)
jpeg("C:/Users/USUARIO/Documents/GitHub/Trabajos-de-Final-de-Grado/TFG_ADE/histograma_freq-juego_nojug.jpg", width = 1200, quality = 90)
barplot(time_frecuencias, main = "Histograma de Frecuencias por Frecuencia de Juego", 
        xlab = "Frecuencia de juego", ylab = "Conteo", col = "blue", ylim = c(0, max(time_frecuencias) + 1))
text(x = barplot(time_frecuencias, plot = FALSE), y = time_frecuencias, 
     label = time_frecuencias, pos = 3, cex = 0.8, col = "black")
dev.off()

#Gráfica de tarta - Plataforma de juego (CHECK)
plataforma_frecuencias <- as.data.frame(table(subset_NoJugadores$f_Plataforma_Juego))
colnames(plataforma_frecuencias) <- c("Plataforma", "Frecuencia")
jpeg("C:/Users/USUARIO/Documents/GitHub/Trabajos-de-Final-de-Grado/TFG_ADE/tarta_plataforma_nojug.jpg", quality = 90)
ggplot(plataforma_frecuencias, aes(x = "", y = Frecuencia, fill = Plataforma)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Distribución por Plataforma", fill = "Plataforma") +
  geom_text(aes(label = Frecuencia), position = position_stack(vjust = 0.5))
dev.off()

#Histograma de frecuencias - Tipo de juegos (CHECK)
tipo_frecuencias <- table(subset_NoJugadores$f_Tipo_Juegos)
jpeg("C:/Users/USUARIO/Documents/GitHub/Trabajos-de-Final-de-Grado/TFG_ADE/hist_tipo-juegos_nojug.jpg", quality = 90)
barplot(tipo_frecuencias, main = "Histograma de Frecuencias por Tipo de videojuego", 
        xlab = "Tipo de videojuego", ylab = "Frecuencia", col = "blue", ylim = c(0, 35))
text(x = barplot(tipo_frecuencias, plot = FALSE), y = tipo_frecuencias, 
     label = tipo_frecuencias, pos = 3, cex = 0.8, col = "black")
dev.off()

#Histograma de frecuencias - Razón por la que no juegan a videojuegos (CHECK)
subset_RazonNoJug <- subset_NoJugadores %>%
  select(contains("No_Juega_Razon_"))
View(subset_RazonNoJug)
frecuencias <- colSums(subset_RazonNoJug == 1)
df_frecuencias <- data.frame(
  Razon = names(frecuencias),
  Frecuencia = as.vector(frecuencias)
)
jpeg("C:/Users/USUARIO/Documents/GitHub/Trabajos-de-Final-de-Grado/TFG_ADE/hist_razon_nojug.jpg", width = 2400, quality = 90)
ggplot(df_frecuencias, aes(x = Razon, y = Frecuencia)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = Frecuencia), vjust = -0.3) +
  labs(title = "Frecuencia de Razon por la que no juegan", x = "Razon", y = "Frecuencia") +
  theme_minimal()
dev.off()


#2. GRADO DE CONOCIMIENTO Y CONSOLIDACIÓN DEL GÉNERO METROIDVANIA (CHECK)
#Gráfica de tarta - Grado de conocimiento del género (CHECK)
plataforma_frecuencias <- as.data.frame(table(datos$f_Conoce_MV))
colnames(plataforma_frecuencias) <- c("Conoce MV", "Frecuencia")
jpeg("C:/Users/USUARIO/Documents/GitHub/Trabajos-de-Final-de-Grado/TFG_ADE/tarta_conoceMV.jpg", quality = 90)
ggplot(plataforma_frecuencias, aes(x = "", y = Frecuencia, fill = `Conoce MV`)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "¿Conocen el género Metroidvania?", fill = "Conoce MV") +
  geom_text(aes(label = Frecuencia), position = position_stack(vjust = 0.5))
dev.off()

#Gráfica de tarta - Relación con los videojuegos metroidvania (CHECK)
datos <- datos %>%
  mutate(tipo_jugadorMV = case_when(
    f_Conoce_MV == "Sí" & f_Ha_JugadoMV == "Sí" ~ "Habituado a los MV",
    f_Ha_JugadoMV == "No" & f_Jugaria_MV == "Sí" ~ "Potencial jugador de MV",
    f_Ha_JugadoMV == "No" & f_Jugaria_MV == "No" ~ "No jugador de MV",
    TRUE ~ NA_character_  # Para casos que no cumplen ninguna condición
  ))
plataforma_frecuencias <- as.data.frame(table(datos$tipo_jugadorMV))
colnames(plataforma_frecuencias) <- c("Relación con los videojuegos MV", "Frecuencia")
jpeg("C:/Users/USUARIO/Documents/GitHub/Trabajos-de-Final-de-Grado/TFG_ADE/tarta_relacion_MV.jpg", quality = 90)
ggplot(plataforma_frecuencias, aes(x = "", y = Frecuencia, fill = `Relación con los videojuegos MV`)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Distribución por Tipo de jugador", fill = "Tipo de jugador") +
  geom_text(aes(label = Frecuencia), position = position_stack(vjust = 0.5))
dev.off()

#Gráfica de tarta - Relación con los videojuegos metroidvania de los potenciales clientes (CHECK)
subset_Jugadores <- subset_Jugadores %>%
  mutate(tipo_jugadorMV = case_when(
    f_Conoce_MV == "Sí" & f_Ha_JugadoMV == "Sí" ~ "Habituado a los MV",
    f_Ha_JugadoMV == "No" & f_Jugaria_MV == "Sí" ~ "Potencial jugador de MV",
    f_Ha_JugadoMV == "No" & f_Jugaria_MV == "No" ~ "No jugador de MV",
    TRUE ~ NA_character_  # Para casos que no cumplen ninguna condición
  ))
plataforma_frecuencias <- as.data.frame(table(subset_Jugadores$tipo_jugadorMV))
colnames(plataforma_frecuencias) <- c("Relación con los videojuegos MV", "Frecuencia")
jpeg("C:/Users/USUARIO/Documents/GitHub/Trabajos-de-Final-de-Grado/TFG_ADE/tarta_relacion_MV_jug.jpg", quality = 90)
ggplot(plataforma_frecuencias, aes(x = "", y = Frecuencia, fill = `Relación con los videojuegos MV`)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Distribución por Tipo de jugador", fill = "Tipo de jugador") +
  geom_text(aes(label = Frecuencia), position = position_stack(vjust = 0.5))
dev.off()

#3. ANÁLISIS DE LA COMPETENCIA DIRECTA (CHECK)
#3.1. ANÁLISIS DE LOS MÁS JUGADOS  (CHECK)
subset_MVJug <- datos %>%
  select(contains("MV_Jugados_"))
frecuencias <- colSums(subset_MVJug == 1)
df_frecuencias <- data.frame(
  Juego = names(frecuencias),
  Frecuencia = as.vector(frecuencias)
)
jpeg("C:/Users/USUARIO/Documents/GitHub/Trabajos-de-Final-de-Grado/TFG_ADE/hist_MVJugs_jug.jpg", width = 1500, quality = 90)
ggplot(df_frecuencias, aes(x = Juego, y = Frecuencia)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = Frecuencia), vjust = -0.3) +
  labs(title = "Frecuencia de Jugadores por Juego", x = "Juego", y = "Frecuencia") +
  theme_minimal()
dev.off()

#3.2. ANÁLISIS DE LOS MEJOR PROMOCIONADOS (CHECK)
subset_MVCon <- datos %>%
  select(contains("MV_Conocidos_"))
frecuencias <- colSums(subset_MVCon == 1)
df_frecuencias <- data.frame(
  Juego = names(frecuencias),
  Frecuencia = as.vector(frecuencias)
)
jpeg("C:/Users/USUARIO/Documents/GitHub/Trabajos-de-Final-de-Grado/TFG_ADE/hist_MVPromo_jug.jpg", width = 1500, quality = 90)
ggplot(df_frecuencias, aes(x = Juego, y = Frecuencia)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = Frecuencia), vjust = -0.3) +
  labs(title = "Frecuencia de Jugadores por Juego", x = "Juego", y = "Frecuencia") +
  theme_minimal()
dev.off()

#4. ANÁLISIS DE LAS PREFERENCIAS DE LOS CONSUMIDORES (CHECK)
#Narrativa (CHECK)
subset_Narrativa <- datos[!is.na(datos$Narrativa), ]
subset_Narrativa$Narrativa <- as.numeric(subset_Narrativa$Narrativa)
breaks <- c(-60, -30, 0, 30, 60)
labels <- c("Historia muy lineal", 
            "Historia lineal con ramas secundarias opcionales muy marcadas", 
            "Indiferente", 
            "Historia no lineal con puntos clave de no retorno", 
            "Historia no lineal")
colors <- c("Historia muy lineal" = "#E6E6FA", 
            "Historia lineal con ramas secundarias opcionales muy marcadas" = "#D8BFD8", 
            "Indiferente" = "#9370DB", 
            "Historia no lineal con puntos clave de no retorno" = "#8A2BE2", 
            "Historia no lineal" = "#4B0082")
subset_Narrativa$Rango <- cut(subset_Narrativa$Narrativa, breaks = c(-60, -30, 0, 1, 30, 60), labels = labels, include.lowest = TRUE, right = FALSE)
jpeg("C:/Users/USUARIO/Documents/GitHub/Trabajos-de-Final-de-Grado/TFG_ADE/pref_narrativa.jpg", width = 750, quality = 90)
ggplot(subset_Narrativa, aes(x = Rango, fill = Rango)) +
  geom_bar(width = 0.5) +  # Ajustar el ancho de las barras
  scale_fill_manual(values = colors) +
  labs(title = "Distribución de Narrativas", x = "Rango de Narrativa", y = "Frecuencia", fill = "Tipo de Narrativa") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_blank(),  # Quita las etiquetas del eje x
    axis.ticks.x = element_blank()  # Quita las marcas del eje x
  ) +
  guides(fill = guide_legend(ncol = 1)) +  # Ajustar la leyenda para que se apile verticalmente
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5)  # Añadir etiquetas con los valores
dev.off()

jpeg("C:/Users/USUARIO/Documents/GitHub/Trabajos-de-Final-de-Grado/TFG_ADE/ppn_narrativa.jpg")
qqnorm(datos$Narrativa, main = "Papel probabilístico normal de Narrativa")
qqline(datos$Narrativa, col = "red")
dev.off()
median(datos$Narrativa, na.rm = TRUE) #Mejor medida
mean(datos$Narrativa, na.rm = TRUE)

#Jugabilidad (CHECK)
subset_Jugabilidad <- datos[!is.na(datos$Jugabilidad), ]
subset_Jugabilidad$Jugabilidad <- as.numeric(subset_Jugabilidad$Jugabilidad)
breaks <- c(-40, -20, 0, 20, 40)
labels <- c("Diferentes niveles de dificultad seleccionables", 
            "Diferentes niveles de dificultad, pero con una mayor dificultad de partida sobre otros juegos", 
            "Indiferente", 
            "Diferentes niveles de dificultad, pero sin posibilidad de cambiarlo durante la partida", 
            "Único nivel de dificultad autoajustado")
colors <- c("Diferentes niveles de dificultad seleccionables" = "#E6E6FA", 
            "Diferentes niveles de dificultad, pero con una mayor dificultad de partida sobre otros juegos" = "#D8BFD8", 
            "Indiferente" = "#9370DB", 
            "Diferentes niveles de dificultad, pero sin posibilidad de cambiarlo durante la partida" = "#8A2BE2", 
            "Único nivel de dificultad autoajustado" = "#4B0082")
subset_Jugabilidad$Rango <- cut(subset_Jugabilidad$Jugabilidad, breaks = c(-40, -20, 0, 1, 20, 40), labels = labels, include.lowest = TRUE, right = FALSE)
jpeg("C:/Users/USUARIO/Documents/GitHub/Trabajos-de-Final-de-Grado/TFG_ADE/pref_jugabilidad.jpg", width = 750, quality = 90)
ggplot(subset_Jugabilidad, aes(x = Rango, fill = Rango)) +
  geom_bar(width = 0.5) +  # Ajustar el ancho de las barras
  scale_fill_manual(values = colors) +
  labs(title = "Distribución de Jugabilidad", x = "Rango de Jugabilidad", y = "Frecuencia", fill = "Tipo de Jugabilidad") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_blank(),  # Quita las etiquetas del eje x
    axis.ticks.x = element_blank()  # Quita las marcas del eje x
  ) +
  guides(fill = guide_legend(ncol = 1)) +  # Ajustar la leyenda para que se apile verticalmente
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5)  # Añadir etiquetas con los valores
dev.off()

jpeg("C:/Users/USUARIO/Documents/GitHub/Trabajos-de-Final-de-Grado/TFG_ADE/ppn_jugabilidad.jpg")
qqnorm(datos$Jugabilidad, main = "Papel probabilístico normal de Jugabilidad")
qqline(datos$Jugabilidad, col = "red")
dev.off()
median(datos$Jugabilidad, na.rm = TRUE) #Mejor medida
mean(datos$Jugabilidad, na.rm = TRUE)

jpeg("C:/Users/USUARIO/Documents/GitHub/Trabajos-de-Final-de-Grado/TFG_ADE/num_mecanicas.jpg", quality = 90)
numMecanicas_frecuencias <- as.data.frame(table(datos$f_Num_Mecanicas))
colnames(numMecanicas_frecuencias) <- c("NumMecanicas", "Frecuencia")
ggplot(numMecanicas_frecuencias, aes(x = "", y = Frecuencia, fill = NumMecanicas)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Distribución por Nº de mecánicas preferido", fill = "NumMecanicas") +
  geom_text(aes(label = Frecuencia), position = position_stack(vjust = 0.5))
dev.off()

jpeg("C:/Users/USUARIO/Documents/GitHub/Trabajos-de-Final-de-Grado/TFG_ADE/complej_mecanicas.jpg", quality = 90)
complejMecanicas_frecuencias <- as.data.frame(table(datos$f_Complejidad_Mecanicas))
colnames(complejMecanicas_frecuencias) <- c("ComplejidadMecanicas", "Frecuencia")
ggplot(complejMecanicas_frecuencias, aes(x = "", y = Frecuencia, fill = ComplejidadMecanicas)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Distribución por nivel de complejidad de las mecánicas", fill = "ComplejidadMecanicas") +
  geom_text(aes(label = Frecuencia), position = position_stack(vjust = 0.5))
dev.off()

#EstiloVisual (CHECK)
subset_EstiloVis <- datos[!is.na(datos$EstiloVis), ]
subset_EstiloVis$EstiloVis <- as.numeric(subset_EstiloVis$EstiloVis)
breaks <- c(-30, -15, 0, 15, 30)
labels <- c("Mayor potencia gráfica", 
            "Preferencia por gráficos modernos y avanzados",
            "Indiferente", 
            "Preferencia por estilo artístico con alguna coherencia",
            "Mayor importancia sobre estilo artístico consistente y bien definido")
colors <- c("Mayor potencia gráfica" = "#FFC0CB", 
            "Preferencia por gráficos modernos y avanzados" = "#FF69B4", 
            "Indiferente" = "#9370DB", 
            "Preferencia por estilo artístico con alguna coherencia" = "#8A2BE2", 
            "Mayor importancia sobre estilo artístico consistente y bien definido" = "#4B0082")
subset_EstiloVis$Rango <- cut(subset_EstiloVis$EstiloVis, breaks = c(-30, -15, 0, 1, 15, 30), labels = labels, include.lowest = TRUE, right = FALSE)
jpeg("C:/Users/USUARIO/Documents/GitHub/Trabajos-de-Final-de-Grado/TFG_ADE/pref_estiloVis.jpg", width = 750, quality = 90)
ggplot(subset_EstiloVis, aes(x = Rango, fill = Rango)) +
  geom_bar(width = 0.5) +  # Ajustar el ancho de las barras
  scale_fill_manual(values = colors) +
  labs(title = "Distribución de Estilo Visual", x = "Rango de Estilo Visual", y = "Frecuencia", fill = "Tipo de Estilo Visual") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_blank(),  # Quita las etiquetas del eje x
    axis.ticks.x = element_blank()  # Quita las marcas del eje x
  ) +
  guides(fill = guide_legend(ncol = 1)) +  # Ajustar la leyenda para que se apile verticalmente
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5)  # Añadir etiquetas con los valores
dev.off()


jpeg("C:/Users/USUARIO/Documents/GitHub/Trabajos-de-Final-de-Grado/TFG_ADE/ppn_estiloVis.jpg", quality = 90)
qqnorm(datos$EstiloVis, main = "Papel probabilístico normal de Estilo Visual")
qqline(datos$EstiloVis, col = "red")
median(datos$EstiloVis, na.rm = TRUE) #Mejor medida
mean(datos$EstiloVis, na.rm = TRUE)
dev.off()

jpeg("C:/Users/USUARIO/Documents/GitHub/Trabajos-de-Final-de-Grado/TFG_ADE/estilo_preferido.jpg", width = 1200, quality = 90)
estiloPref_frecuencias <- as.data.frame(table(datos$f_Estilo_preferido))
colnames(estiloPref_frecuencias) <- c("EstiloPreferido", "Frecuencia")
ggplot(estiloPref_frecuencias, aes(x = "", y = Frecuencia, fill = EstiloPreferido)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Distribución por estilo artístico preferido", fill = "EstiloPreferido") +
  geom_text(aes(label = Frecuencia), position = position_stack(vjust = 0.5))
dev.off()

#SoftSkills (CHECK)
subset_SoftSkills <- datos[!is.na(datos$SoftSkills), ]
subset_SoftSkills$SoftSkills <- as.numeric(subset_SoftSkills$SoftSkills)
breaks <- c(-50, -25, 0, 25, 50)
labels <- c("Solo aportan entretenimiento y no conocimientos de Soft Skills", 
            "Aportan entretenimiento con algunos conocimientos de Soft Skills",
            "Indiferente", 
            "Aportan conocimientos de Soft Skills con algo de entretenimiento",
            "Aportan conocimientos en Soft Skills")
colors <- c("Solo aportan entretenimiento y no conocimientos de Soft Skills" = "#FFC0CB", 
            "Aportan entretenimiento con algunos conocimientos de Soft Skills" = "#FF69B4", 
            "Indiferente" = "#9370DB", 
            "Aportan conocimientos de Soft Skills con algo de entretenimiento" = "#8A2BE2", 
            "Aportan conocimientos en Soft Skills" = "#4B0082")
subset_SoftSkills$Rango <- cut(subset_SoftSkills$SoftSkills, breaks = c(-50, -25, 0, 1, 25, 50), labels = labels, include.lowest = TRUE, right = FALSE)
jpeg("C:/Users/USUARIO/Documents/GitHub/Trabajos-de-Final-de-Grado/TFG_ADE/pref_softskills.jpg", width = 750, quality = 90)
ggplot(subset_SoftSkills, aes(x = Rango, fill = Rango)) +
  geom_bar(width = 0.5) +  # Ajustar el ancho de las barras
  scale_fill_manual(values = colors) +
  labs(title = "Distribución de Soft Skills", x = "Rango de Soft Skills", y = "Frecuencia", fill = "Aportación de Soft Skills") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_blank(),  # Quita las etiquetas del eje x
    axis.ticks.x = element_blank()  # Quita las marcas del eje x
  ) +
  guides(fill = guide_legend(ncol = 1)) +  # Ajustar la leyenda para que se apile verticalmente
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5)  # Añadir etiquetas con los valores
dev.off()


jpeg("C:/Users/USUARIO/Documents/GitHub/Trabajos-de-Final-de-Grado/TFG_ADE/ppn_softskills.jpg", quality = 90)
qqnorm(datos$SoftSkills, main = "Papel probabilístico normal de Soft Skills")
qqline(datos$SoftSkills, col = "red")
median(datos$SoftSkills, na.rm = TRUE) #Mejor medida
mean(datos$SoftSkills, na.rm = TRUE)
dev.off()

#Inteligencia Emocional (CHECK)
subset_IntEmocional <- datos[!is.na(datos$IntEmocional), ]
subset_IntEmocional$IntEmocional <- as.numeric(subset_IntEmocional$IntEmocional)
breaks <- c(-70, -35, 0, 35, 70)
labels <- c("No ayudan a desarrollar la inteligencia emocional", 
            "Poco útiles para el desarrollo de la inteligencia emocional",
            "Indiferente", 
            "Útiles para el desarrollo de la inteligencia emocional",
            "Son una buena herramienta de desarrollo de inteligencia emocional")
colors <- c("No ayudan a desarrollar la inteligencia emocional" = "#FFC0CB", 
            "Poco útiles para el desarrollo de la inteligencia emocional" = "#FF69B4", 
            "Indiferente" = "#9370DB", 
            "Útiles para el desarrollo de la inteligencia emocional" = "#8A2BE2", 
            "Son una buena herramienta de desarrollo de inteligencia emocional" = "#4B0082")
subset_IntEmocional$Rango <- cut(subset_IntEmocional$IntEmocional, breaks = c(-70, -35, 0, 1, 35, 70), labels = labels, include.lowest = TRUE, right = FALSE)
jpeg("C:/Users/USUARIO/Documents/GitHub/Trabajos-de-Final-de-Grado/TFG_ADE/pref_intEmocional.jpg", width = 750, quality = 90)
ggplot(subset_IntEmocional, aes(x = Rango, fill = Rango)) +
  geom_bar(width = 0.5) +  # Ajustar el ancho de las barras
  scale_fill_manual(values = colors) +
  labs(title = "Distribución de Inteligencia Emocional", x = "Rango de Inteligencia Emocional", y = "Frecuencia", fill = "Aportación de Inteligencia Emocional") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_blank(),  # Quita las etiquetas del eje x
    axis.ticks.x = element_blank()  # Quita las marcas del eje x
  ) +
  guides(fill = guide_legend(ncol = 1)) +  # Ajustar la leyenda para que se apile verticalmente
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5)  # Añadir etiquetas con los valores
dev.off()


jpeg("C:/Users/USUARIO/Documents/GitHub/Trabajos-de-Final-de-Grado/TFG_ADE/ppn_intEmocional.jpg", quality = 90)
qqnorm(datos$IntEmocional, main = "Papel probabilístico normal de Inteligencia Emocional")
qqline(datos$IntEmocional, col = "red")
median(datos$IntEmocional, na.rm = TRUE)
mean(datos$IntEmocional, na.rm = TRUE) #Ambas son igual de buenas
dev.off()

#5. CORRELACIONES (CHECK)
subset_aux <- datos %>%
  select(contains("f_Aceptacion_Oniros"))
#Análisis de correlacion de perfiles con el nivel de aceptación (CHECK)
edad <- ifelse(is.na(datos$f_EdadJug), datos$f_EdadNoJug, datos$f_EdadJug)
sitLab <- ifelse(is.na(datos$f_SituacionLaboralJug), datos$f_SituacionLaboralNoJug, datos$f_SituacionLaboralJug)
genero <- ifelse(is.na(datos$f_GeneroJug), datos$f_GeneroNoJug, datos$f_GeneroJug)
perfil <- cbind(subset_aux, edad, sitLab, genero)
perfil <- drop_na(perfil, f_Aceptacion_Oniros)

jpeg("C:/Users/USUARIO/Documents/GitHub/Trabajos-de-Final-de-Grado/TFG_ADE/corr_caract-perfil1.jpg", quality = 90)
perfil %>% correlate() %>% plot()
dev.off()

jpeg("C:/Users/USUARIO/Documents/GitHub/Trabajos-de-Final-de-Grado/TFG_ADE/corr_caract-perfil2.jpg", quality = 90)
ggpairs(perfil, 
        columns = c("edad","sitLab","genero"),
        upper = list(continuous = "points"),
        lower = list(continuous = "smooth"),
        diag = list(continuous = "densityDiag"))
dev.off()

subset_mod1 <- perfil %>% select(edad, sitLab, genero)
subset_acept <- perfil %>% select(f_Aceptacion_Oniros)
subset_acept <- subset_acept %>%
  mutate(f_Aceptacion_Oniros = case_when(
    f_Aceptacion_Oniros == "Sí" ~ 1,
    f_Aceptacion_Oniros == "No" ~ 0))
subset_acept <- as.vector(subset_acept[[1]]) 

sink("C:/Users/USUARIO/Documents/GitHub/Trabajos-de-Final-de-Grado/TFG_ADE/resumen_modeloA.txt")
lgm1 <- train(subset_mod1, subset_acept,
              method = 'glm', family = binomial,
              trControl = trainControl(method = 'cv'))
cat("Resumen del modelo lgm1\n")
print(summary(lgm1))
sink()

#Análisis de correlacion de las preferencias con el nivel de aceptación (CHECK)
subset_preferencias <- datos[, 69:73]
View(subset_preferencias)

jpeg("C:/Users/USUARIO/Documents/GitHub/Trabajos-de-Final-de-Grado/TFG_ADE/corr_preferencias1.jpg", quality = 90)
subset_preferencias %>% correlate() %>% plot()
dev.off()

jpeg("C:/Users/USUARIO/Documents/GitHub/Trabajos-de-Final-de-Grado/TFG_ADE/corr_preferencias2.jpg", quality = 90)
ggpairs(subset_preferencias, 
        columns = c("Narrativa", "Jugabilidad", "EstiloVis", "SoftSkills", "IntEmocional"),
        upper = list(continuous = "points"),
        lower = list(continuous = "smooth"),
        diag = list(continuous = "densityDiag"))
dev.off()

concatenado <- cbind(subset_aux, subset_preferencias)
concatenado <- drop_na(concatenado, f_Aceptacion_Oniros)

subset_acept <- concatenado %>% select(f_Aceptacion_Oniros)
subset_acept <- subset_acept %>%
  mutate(f_Aceptacion_Oniros = case_when(
    f_Aceptacion_Oniros == "Sí" ~ 1,
    f_Aceptacion_Oniros == "No" ~ 0))
subset_acept <- as.vector(subset_acept[[1]]) 

subset_mod1 <- concatenado %>% select(Jugabilidad, EstiloVis, IntEmocional)
subset_mod2 <- concatenado %>% select(Narrativa, EstiloVis, IntEmocional)
subset_mod3 <- concatenado %>% select(SoftSkills, EstiloVis)
subset_mod4 <- concatenado %>% select(IntEmocional, EstiloVis)

sink("C:/Users/USUARIO/Documents/GitHub/Trabajos-de-Final-de-Grado/TFG_ADE/resumen_modelosB.txt")
lgm1 <- train(subset_mod1, subset_acept,
             method = 'glm', family = binomial,
             trControl = trainControl(method = 'cv'))
cat("Resumen del modelo lgm1\n")
print(summary(lgm1)) #Hay que quitar todas menos IntEmocional

lgm2 <- train(subset_mod2, subset_acept,
              method = 'glm', family = binomial,
              trControl = trainControl(method = 'cv'))
cat("Resumen del modelo lgm1\n")
print(summary(lgm2))#Hay que quitar todas menos IntEmocional

lgm3 <- train(subset_mod3, subset_acept,
              method = 'glm', family = binomial,
              trControl = trainControl(method = 'cv'))
cat("Resumen del modelo lgm1\n")
print(summary(lgm3)) #Hay que quitar todas menos SoftSkills

lgm4 <- train(subset_mod4, subset_acept,
              method = 'glm', family = binomial,
              trControl = trainControl(method = 'cv'))
cat("Resumen del modelo lgm1\n")
print(summary(lgm4)) #Hay que quitar todas menos IntEmocional

sink()

#6. ANÁLISIS DEL NIVEL DE ACEPTACIÓN (CHECK)
#Gráfica de tarta - Nivel de aceptación de Oniros (CHECK)
aceptacion_frecuencias <- as.data.frame(table(datos$f_Aceptacion_Oniros))
colnames(aceptacion_frecuencias) <- c("Aceptacion", "Frecuencia")
jpeg("C:/Users/USUARIO/Documents/GitHub/Trabajos-de-Final-de-Grado/TFG_ADE/tarta_aceptacion.jpg", quality = 90)
ggplot(aceptacion_frecuencias, aes(x = "", y = Frecuencia, fill = Aceptacion)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Nivel de aceptación de la propuesta", fill = "Aceptación") +
  geom_text(aes(label = Frecuencia), position = position_stack(vjust = 0.5))
dev.off()

#Histograma de frecuencias - Aspecto más destacado de la propuesta (CHECK)
subset_OnirosDestaca <- subset_Jugadores %>%
  select(contains("Oniros_destacado_"))
frecuencias <- colSums(subset_OnirosDestaca == 1)
df_frecuencias <- data.frame(
  Aspecto_destacado = names(frecuencias),
  Frecuencia = as.vector(frecuencias)
)
jpeg("C:/Users/USUARIO/Documents/GitHub/Trabajos-de-Final-de-Grado/TFG_ADE/hist_md.jpg", width = 1200, quality = 90)
ggplot(df_frecuencias, aes(x = Aspecto_destacado, y = Frecuencia)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = Frecuencia), vjust = -0.3) +
  labs(title = "Aspecto más destacado de la propuesta", x = "Aspecto destacado", y = "Frecuencia") +
  theme_minimal()
dev.off()

#Histograma de frecuencias - Comparativa de respecto al interés real (CHECK)
count_aceptacion <- sum(!is.na(subset_Jugadores$f_Aceptacion_Oniros))
count_email <- sum(!is.na(subset_Jugadores$f_Email))
conteos <- data.frame(
  Variable = c("f_Aceptacion_Oniros", "f_Email"),
  Conteo = c(count_aceptacion, count_email)
)
jpeg("C:/Users/USUARIO/Documents/GitHub/Trabajos-de-Final-de-Grado/TFG_ADE/hist_interes_real.jpg", quality = 90)
ggplot(conteos, aes(x = Variable, y = Conteo, fill = Variable)) +
  geom_bar(stat = "identity") +
  labs(title = "Conteo de Datos No Nulos", x = "Variable", y = "Conteo") +
  theme_minimal() +
  scale_fill_manual(values = c("f_Aceptacion_Oniros" = "lightblue", "f_Email" = "lightgreen"))
dev.off()



