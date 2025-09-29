#' Call Data Indicators CEPALSTAT
#'
#'@description
#'call.data is a function that allows you to consult the CEPALSTAT API to obtain in
#'a data frame with the values of the desired indicator, disaggregations, by year, countries
#'and others that the indicator has.
#'
#'@usage call.data(id.indicator, language.en = TRUE, notes = FALSE)
#'
#' @param id.indicator You must determine the ID of the indicator or variable that you want to obtain.
#' @param language.en If true or omitted is selected, the default language will be English. Select False to choose the Spanish language.
#' @param notes They are the methodological notes that an indicator may have. Select TRUE to be able to add the notes to the data frame.
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate_at
#' @importFrom dplyr filter
#' @import tidyr separate
#' @import jsonlite fromJSON
#' @import stats setNames
#' @import stringr str_count
#'
#' @examples data <- call.data(id.indicator = 1)
#'

utils::globalVariables(c("Years", "Country", "value", "id", "name"))

call.data <- function(id.indicator,
                      language.en = TRUE,
                      notes = FALSE) {

  # Llamar datos dependiendo del lenguaje

  if(language.en == TRUE) {

    # 1. Definir url del indicador seleccionado ----

    url.data <- paste0('https://api-cepalstat.cepal.org/cepalstat/api/v1/indicator/',
                       id.indicator,
                       '/data?lang=en&format=json&in=1')

    # 2. Importar datos en formato JSON ----

    data.lista <- jsonlite::fromJSON(url.data)

    # 3. Extraer datos de la lista ----
    data <- data.lista$body$data

    # 4. Llamar dimensiones -----

    url.dimesiones <- paste0('https://api-cepalstat.cepal.org/cepalstat/api/v1/indicator/',
                             id.indicator,
                             '/dimensions?lang=en&format=csv&in=1')

    dimension <- read.csv(url.dimesiones)


    nombre <- names(data)
    V <- grepl('dim_', nombre)
    nombre <- nombre[V]

    for(i in 1:length(nombre)) {

      n <- c(nombre[i], paste0('Labels_', nombre[i]))

      d <- dimension %>%
        dplyr::select(member_id, name) %>%
        stats::setNames(n)

      data <- suppressMessages(dplyr::left_join(data, d))

    }

    data <- data[,!names(data) %in% nombre]

    # 5. Cambiar variable a numérico ----

    data$value <- as.numeric(data$value)

    x <- unique(names(data) %in% c('Labels_dim_29117'))

    if(length(x) > 1) {

      data$Labels_dim_29117 <- as.numeric(data$Labels_dim_29117) }


    # 6. Llamar notas si son solicitadas ----

    if(notes == FALSE) {

      data <- data  }

    else {

      url.notes <- paste0('https://api-cepalstat.cepal.org/cepalstat/api/v1/indicator/',
                          id.indicator,
                          '/footnotes?lang=en&format=csv')

      notes <- read.csv(url.notes)

      n <- max(stringr::str_count(data$notes_ids, pattern = ',')) + 1

      notas.n  <- paste0('id_Notes_', 1:n)

      data <- data %>%
        tidyr::separate(notes_ids, into = notas.n)

      for(i in 1:length(notas.n)) {

        n <- c(notas.n[i], paste0('Notes_',i))

        d <- notes %>%
          stats::setNames(n)

        data[,notas.n[i]] <- as.numeric(data[,notas.n[i]])

        data <- suppressMessages(dplyr::left_join(data, d))

      }

      data <- data[,!names(data) %in% notas.n] }


    # 7. Asignar Nombres a las variables ----

    url.label <- paste0('https://api-cepalstat.cepal.org/cepalstat/api/v1/indicator/',
                        id.indicator,
                        '/dimensions?lang=en&format=json&in=1')

    labels.var.lista <- jsonlite::fromJSON(url.label)
    labels.var <- labels.var.lista$body$dimensions[,1:2]
    labels.var$label <- paste0('Labels_dim_', labels.var$id)
    labels.var$name <- gsub('__ESTANDAR', '', labels.var$name)

    nombre <- data.frame(label = names(data))

    nombre <- suppressMessages(dplyr::left_join(nombre, labels.var))

    nombre <- nombre %>%
      dplyr::mutate(nombre = ifelse(is.na(name), label, name))

    nombre <- nombre$nombre
    names(data) <- nombre


    }





  # Datos en Español
  else {

    # 1. Definir url del indicador seleccionado ----

    url.data <- paste0('https://api-cepalstat.cepal.org/cepalstat/api/v1/indicator/',
                       id.indicator,
                       '/data?lang=es&format=json&in=1')

    # 2. Importar datos en formato JSON ----

    data.lista <- jsonlite::fromJSON(url.data)

    # 3. Extraer datos de la lista ----
    data <- data.lista$body$data

    # 4. Llamar dimensiones -----

    url.dimesiones <- paste0('https://api-cepalstat.cepal.org/cepalstat/api/v1/indicator/',
                             id.indicator,
                             '/dimensions?lang=es&format=csv&in=1')

    dimension <- read.csv(url.dimesiones)


    nombre <- names(data)
    V <- grepl('dim_', nombre)
    nombre <- nombre[V]

    for(i in 1:length(nombre)) {

      n <- c(nombre[i], paste0('Labels_', nombre[i]))

      d <- dimension %>%
        dplyr::select(member_id, name) %>%
        stats::setNames(n)

      data <- suppressMessages(dplyr::left_join(data, d))

    }

    data <- data[,!names(data) %in% nombre]

    # 5. Cambiar variable a numérico ----

    data$value <- as.numeric(data$value)

    x <- unique(names(data) %in% c('Labels_dim_29117'))

    if(length(x) > 1) {

    data$Labels_dim_29117 <- as.numeric(data$Labels_dim_29117) }


    # 6. Llamar notas si son solicitadas ----

  if(notes == FALSE) {

    data <- data  }

  else {

    url.notes <- paste0('https://api-cepalstat.cepal.org/cepalstat/api/v1/indicator/',
                        id.indicator,
                        '/footnotes?lang=es&format=csv')


    notes <- read.csv(url.notes)

    n <- max(stringr::str_count(data$notes_ids, pattern = ',')) + 1

    notas.n  <- paste0('id_Nota_', 1:n)

    data <- data %>%
      tidyr::separate(notes_ids, into = notas.n)

    for(i in 1:length(notas.n)) {

      n <- c(notas.n[i], paste0('Nota_',i))

      d <- notes %>%
        stats::setNames(n)

      data[,notas.n[i]] <- as.numeric(data[,notas.n[i]])

      data <- suppressMessages(dplyr::left_join(data, d))

    }

    data <- data[,!names(data) %in% notas.n] }



    # Asignar Nombres a las variables

    url.label <- paste0('https://api-cepalstat.cepal.org/cepalstat/api/v1/indicator/',
                        id.indicator,
                        '/dimensions?lang=es&format=json&in=1')

    labels.var.lista <- jsonlite::fromJSON(url.label)
    labels.var <- labels.var.lista$body$dimensions[,1:2]
    labels.var$label <- paste0('Labels_dim_', labels.var$id)
    labels.var$name <- gsub('__ESTANDAR', '', labels.var$name)

    nombre <- data.frame(label = names(data))

    nombre <- suppressMessages(dplyr::left_join(nombre, labels.var))

    nombre <- nombre %>%
      dplyr::mutate(nombre = ifelse(is.na(name), label, name))

    nombre <- nombre$nombre

    #nombre <- ifelse(nombre %in% c('Labels_dim_144', 'Labels_dim_74472'), 'Sexo',
     #         ifelse(nombre == 'Labels_dim_208', 'País',
      #        ifelse(nombre == 'Labels_dim_29117', 'Año',
       #       ifelse(nombre == 'Labels_dim_374', 'Grupos Etarios',
        #      ifelse(nombre  %in%  c('Labels_dim_267', 'Labels_dim_48309'), 'Grupos de Edad',
         #     ifelse(nombre == 'Labels_dim_74392', 'Edad',
          #    ifelse(nombre == 'Labels_dim_146723', 'Edad hasta 29',
           #   ifelse(nombre == 'Labels_dim_47491', 'Década',
            #  ifelse(nombre == 'Labels_dim_47394', 'Maternidad',
             # ifelse(nombre == 'Labels_dim_48627', 'Maternidad con ignorados imputados a No Madres',
              #ifelse(nombre == 'Labels_dim_78149', 'Condición Indigena', nombre)))))))))))


    names(data) <- nombre




  }


  return(data)

}



