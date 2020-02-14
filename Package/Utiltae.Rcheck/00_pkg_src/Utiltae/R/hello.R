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

#funcion que calcula la moda de una lista dada
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#Funcion que cambia los header del dataset
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

get_indicadores <- function(df_data, question){
  #separo un dataSet con la informacion importante para calcular el indicador
  ECV_ALIMENTO_QTION <- df_data[,c("barrio","estrato",question)]

  #Genero un conteo del toral de respuestas por barrio y estrato
  #ECV_ALIMENTO_QTION_COUNTXR <- aggregate(ECV_ALIMENTO_QTION$question,by=list(ECV_ALIMENTO_QTION$barrio,ECV_ALIMENTO_QTION$estrato,ECV_ALIMENTO_QTION$question),function(x){NROW(x)})
  #ECV_ALIMENTO_QTION_COUNTXR <- setNames(ECV_ALIMENTO_QTION_COUNTXR,c("barrio",'estrato',question,"totalRespuestaE"))


  return(ECV_ALIMENTO_QTION)
}

