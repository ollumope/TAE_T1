# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

library(dplyr)
library(plyr)

#' funcion que calcula la moda de una lista dada
#' @param v A vector.
#' @return valor de la moda de vector dado
#' @examples
#' getmode(c(1,2,3,4,4,4,4))
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#' Funcion que cambia los header del dataset
#' @param list_names A vector.
#' @return Retorna un vector con los nombre normalizados
#' @examples
#' set_dataSet_names(c('name1','name2'...)).
set_dataSet_names <- function(list_names){

  names_ECV <- list_names
  i = 1
  l_names <- list()
  for (name_col in names_ECV) {
    str_col <- unlist(strsplit(name_col," "))
    str_col <- unlist(strsplit(str_col,"[.]"))
    if (length(str_col) > 2){
      l_names[i] <- paste(str_col[2],str_col[3],sep = '_')
    }
    else
    {
      l_names[i] <- paste(str_col[2])
    }

    i = i + 1
  }

  return(l_names)

}

#' funcion que calcula los indicadores porcentuales de una pregunta
#' individual dada en el dataFrame de calidad de vida de Medellín.
#' @param df_data A dataFrame.
#' @param question A string.
#' @return La funcion retorna un dataFrame con el calculo del indicador porcentual.
#' @examples
#' get_indicadores(data,'p_229')
get_indicadores_ei <- function(df_data, question){
  require(dplyr)
  require(lazyeval)

  #separo un dataSet con la informacion importante para calcular el indicador
  ECV_ALIMENTO_QTION <- df_data[,c("barrio","estrato", question)]
  ECV_BASICO <- ECV[,c("comuna","barrio","estrato")]
  ECV_BASICO <- unique(ECV_BASICO)

  #Genero un conteo del toral de respuestas por barrio y estrato
  ECV_ALIMENTO_QTION %>%
    group_by_("barrio","estrato",question) %>%
    summarise(totalRespuestaE=n()) -> ECV_ALIMENTO_QTION_COUNTXR
  ECV_ALIMENTO_QTION_COUNTXR <- setNames(ECV_ALIMENTO_QTION_COUNTXR,c("barrio",'estrato',interp(question),"totalRespuestaE"))

  #Genero un conteo del toral de respuestas por barrio
  ECV_ALIMENTO_QTION %>%
    group_by_("barrio") %>%
    summarise(TotalB=n()) -> ECV_ALIMENTO_QTION_CTB
  ECV_ALIMENTO_QTION_CTB <- setNames(ECV_ALIMENTO_QTION_CTB,c("barrio","TotalB"))

  #Separo otro DataSet con el barrio, estrato y la cantidad de respuestas para calcular el total #encuestado

  ECV_ALIMENTO_QTION_COUNT <- ECV_ALIMENTO_QTION_COUNTXR[,c("barrio","estrato","totalRespuestaE")]
  ECV_ALIMENTO_QTION_COUNT %>%
    group_by(barrio,estrato) %>%
    summarise(totalEncuestadosE = sum(totalRespuestaE)) -> ECV_ALIMENTO_QTION_COUNT

  # se cuenta con 2 dataSet para generar el indicador k_question
  ECV_ALIMENTO_QTION_KQTION <- merge(x=ECV_ALIMENTO_QTION_COUNT,y=ECV_ALIMENTO_QTION_COUNTXR,by=c("barrio","estrato"))
  ECV_ALIMENTO_QTION_KQTION <- within(ECV_ALIMENTO_QTION_KQTION, k_qtion_1 <- totalRespuestaE / totalEncuestadosE  )

  ECV_ALIMENTO_QTION_KQTION <- merge(x=ECV_ALIMENTO_QTION_KQTION,y=ECV_ALIMENTO_QTION_CTB,by=c("barrio"))
  ECV_ALIMENTO_QTION_KQTION <- within(ECV_ALIMENTO_QTION_KQTION, k_qtion_2 <- totalEncuestadosE / TotalB  )

  ECV_ALIMENTO_QTION_KQTION <- merge(x=ECV_ALIMENTO_QTION_KQTION,y=ECV_BASICO,by = c("barrio","estrato"))

  ECV_ALIMENTO_QTION_KQTION <- ECV_ALIMENTO_QTION_KQTION[,c("comuna","barrio","estrato",question,"totalRespuestaE","totalEncuestadosE","TotalB","k_qtion_1","k_qtion_2")]

  return(ECV_ALIMENTO_QTION_KQTION)
}

#' funcion que calcula los indicadores porcentuales de una pregunta
#' consolidada por familia dada en el dataFrame de calidad de vida de Medellín
#' la consolidacion es resuelta por medio de la moda.
#' @param df_data A dataFrame.
#' @param question A string.
#' @return La funcion retorna un dataFrame con el calculo del indicador porcentual.
#' @examples
#' get_indicadores(data,'p_229')
get_indicadores_ec <- function(df_data, question){
  require(dplyr)
  require(lazyeval)

  #separo un dataSet con la informacion importante para calcular el indicador
  ECV_ALIMENTO_QTION <- df_data[,c("encuesta","barrio","estrato", question)]
  ECV_BASICO <- ECV[,c("comuna","barrio","estrato")]
  ECV_BASICO <- unique(ECV_BASICO)

  ECV_ALIMENTO_QTION <- ECV_ALIMENTO_QTION %>% group_by(encuesta,barrio,estrato) %>%
  summarise( QTION = getmode(.data[[question]])) %>% ungroup()
  ECV_ALIMENTO_QTION <- setNames(ECV_ALIMENTO_QTION,c("encuesta","barrio",'estrato',interp(question)))

  #Genero un conteo del toral de respuestas por barrio y estrato
  ECV_ALIMENTO_QTION %>%
    group_by_("barrio","estrato",question) %>%
    summarise(totalRespuestaE=n()) -> ECV_ALIMENTO_QTION_COUNTXR
  ECV_ALIMENTO_QTION_COUNTXR <- setNames(ECV_ALIMENTO_QTION_COUNTXR,c("barrio",'estrato',interp(question),"totalRespuestaE"))

  #Genero un conteo del toral de respuestas por barrio
  ECV_ALIMENTO_QTION %>%
    group_by_("barrio") %>%
    summarise(TotalB=n()) -> ECV_ALIMENTO_QTION_CTB
  ECV_ALIMENTO_QTION_CTB <- setNames(ECV_ALIMENTO_QTION_CTB,c("barrio","TotalB"))

  #Separo otro DataSet con el barrio, estrato y la cantidad de respuestas para calcular el total #encuestado

  ECV_ALIMENTO_QTION_COUNT <- ECV_ALIMENTO_QTION_COUNTXR[,c("barrio","estrato","totalRespuestaE")]
  ECV_ALIMENTO_QTION_COUNT %>%
    group_by(barrio,estrato) %>%
    summarise(totalEncuestadosE = sum(totalRespuestaE)) -> ECV_ALIMENTO_QTION_COUNT

  # se cuenta con 2 dataSet para generar el indicador k_question
  ECV_ALIMENTO_QTION_KQTION <- merge(x=ECV_ALIMENTO_QTION_COUNT,y=ECV_ALIMENTO_QTION_COUNTXR,by=c("barrio","estrato"))
  ECV_ALIMENTO_QTION_KQTION <- within(ECV_ALIMENTO_QTION_KQTION, k_qtion_1 <- totalRespuestaE / totalEncuestadosE  )

  ECV_ALIMENTO_QTION_KQTION <- merge(x=ECV_ALIMENTO_QTION_KQTION,y=ECV_ALIMENTO_QTION_CTB,by=c("barrio"))
  ECV_ALIMENTO_QTION_KQTION <- within(ECV_ALIMENTO_QTION_KQTION, k_qtion_2 <- totalEncuestadosE / TotalB  )

  ECV_ALIMENTO_QTION_KQTION <- merge(x=ECV_ALIMENTO_QTION_KQTION,y=ECV_BASICO,by = c("barrio","estrato"))

  ECV_ALIMENTO_QTION_KQTION <- ECV_ALIMENTO_QTION_KQTION[,c("comuna","barrio","estrato",question,"totalRespuestaE","totalEncuestadosE","TotalB","k_qtion_1","k_qtion_2")]

  return(ECV_ALIMENTO_QTION_KQTION)
}
