

#' Title
#'
#' @param id.indicator
#' @param language.en
#' @param save
#' @param height
#' @param width
#' @param size.title
#' @param title
#' @param caption
#' @param color
#' @param color.alc
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr arrange
#' @importFrom dplyr summarise
#' @importFrom stringr str_split
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_col
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 coord_flip
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 ggsave
#' @examples



ranking.ods <- function(id.indicator, language.en=TRUE, save = FALSE,
                        height=5, width=9, size.title=10, title=TRUE,
                        caption=TRUE, color='#032B47', color.alc='#36B3FF') {

# Si se selecciona idioma inglés
  if(language.en==TRUE) {

    # 1. Llamar y Filtrar indicadores ----
    indicadores <- call.indicators()

    indicadores <- indicadores %>%
      dplyr::filter(Dimension == 'Sustainable Development Goals (SDG)') %>%
      dplyr::mutate(name = ifelse((Indicador.3 =="" & Indicador.2 == ""), Indicador.1,
                           ifelse(Indicador.3 =="", Indicador.2, Indicador.3))) %>%
      dplyr::filter(`Indicator ID` == id.indicator)

    name.indicator <- indicadores$name

    n <- length(name.indicator)

    # 2. Advertencia de selección erronea de indicador ----

    if(n == 0) {cat(paste0('Warning: \nSelect the indicator id of the Sustainable Development Goals (SDG) dimension'))}

    # 3. Continuar con indicador correcto -----
    else{

    # 4. Ajustar nombre de indicador para el título ----
      name.indicator <- stringr::str_split(name.indicator, pattern = '\\s+') %>% unlist()

      l <- length(name.indicator)


      name.indicator.o <- ifelse(l <= 10, paste(name.indicator, collapse = " "),
                          ifelse(l > 10 & l <= 20, paste0(paste(name.indicator[1:10], collapse = " "),  '\n',
                                                          paste(name.indicator[11:l], collapse = " ")),
                          ifelse(l > 20, paste0(paste(name.indicator[1:10], collapse = " "),  '\n',
                                                paste(name.indicator[11:20], collapse = " "), '\n',
                                                paste(name.indicator[21:l], collapse = " ")))))

      # 5. Llamar y procesar los datos del indicador ----
      data <- call.data(id.indicator = id.indicator)

      data <- data %>%
        dplyr::group_by(Country) %>%
        dplyr::mutate(Maximo = max(Years),
                      filtro = ifelse(Years == Maximo, 1, 0)) %>%
        dplyr::ungroup() %>%
        dplyr::filter(filtro == 1) %>%
        dplyr::group_by(Years, Country) %>%
        dplyr::summarise(value = mean(value, na.rm = T)) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(Years) %>%
        dplyr::mutate(filtro = ifelse(Country == 'Latin America and the Caribbean', 1, 0))


      # 6. Crear bucle para crear el vector de la nota -----
      cap <- unique(data$Years)
      cap.list <- list()
      for(i in 1:length(cap)){

        paises <- data$Country[data$Years == cap[i]]
        paises <- paste0(cap[i], ': ', paste(paises, collapse = ', '))
        cap.list[[i]] <- paises

      }

      cap <- do.call(cbind, cap.list) %>% array()
      cap <- paste0('Note: Latest data available, ', paste(cap, collapse = '; '))

      cap <- stringr::str_split(cap, pattern = '\\s+') %>% unlist()

      l <- length(cap)

      cap <- ifelse(l <= 15, paste(cap, collapse = " "),

             ifelse(l > 15 & l <= 30, paste0('\n', paste(cap[1:15], collapse = " "),  '\n',
                                      paste(cap[16:l], collapse = " ")),

             ifelse(l > 30 & l <= 45, paste0('\n', paste(cap[1:15], collapse = " "),  '\n',
                                      paste(cap[16:30], collapse = " "), '\n',
                                      paste(cap[31:l], collapse = " ")),

             ifelse(l > 45 & l <= 60, paste0('\n', paste(cap[1:15], collapse = " "),  '\n',
                                      paste(cap[16:30], collapse = " "), '\n',
                                      paste(cap[31:45], collapse = " "), '\n',
                                      paste(cap[46:l], collapse = " ")),

            ifelse(l > 60 & l <= 75, paste0('\n', paste(cap[1:15], collapse = " "),  '\n',
                                     paste(cap[16:30], collapse = " "), '\n',
                                     paste(cap[31:45], collapse = " "), '\n',
                                     paste(cap[46:60], collapse = " "), '\n',
                                     paste(cap[61:l], collapse = " ")),

           ifelse(l > 75,            paste0('\n', paste(cap[1:15], collapse = " "),  '\n',
                                     paste(cap[16:30], collapse = " "), '\n',
                                     paste(cap[31:45], collapse = " "), '\n',
                                     paste(cap[46:60], collapse = " "), '\n',
                                     paste(cap[61:75], collapse = " "), '\n',
                                     paste(cap[76:l], collapse = " "))))))))


      # 7. Crear Gráficos dependiendo de si se encuentra ALC ----

      alc <- unique(data$filtro)

      if(title == TRUE & caption == TRUE) {

        if(length(alc) == 1) {

          g <- data %>%
            ggplot2::ggplot(ggplot2::aes(x = reorder(Country, value), y = value)) +
            ggplot2::geom_col(fill = color) +
            ggplot2::coord_flip() +
            ggplot2::labs(title =  name.indicator.o, y = 'Indicator achievement',
                 x = 'Country', caption = cap) +
            ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0, size = size.title, face = "bold"),
                  plot.caption = ggplot2::element_text(hjust = 0))

        } else {
          g <- data %>%
            ggplot2::ggplot(ggplot2::aes(x = reorder(Country, value), y = value, fill = as.character(filtro))) +
            ggplot2::geom_col() +
            ggplot2::coord_flip() +
            ggplot2::labs(title =  name.indicator.o, y = 'Indicator achievement',
                 x = 'Country', caption = cap) +
            ggplot2::scale_fill_manual(values = c(color, color.alc)) +
            ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0, size = size.title, face = "bold"),
                  plot.caption = ggplot2::element_text(hjust = 0),
                  legend.position = 'none')
            }




      }

      if(title == TRUE & caption == FALSE) {


        if(length(alc) == 1) {

          g <- data %>%
            ggplot2::ggplot(ggplot2::aes(x = reorder(Country, value), y = value)) +
            ggplot2::geom_col(fill = color) +
            ggplot2::coord_flip() +
            ggplot2::labs(title =  name.indicator.o, y = 'Indicator achievement',
                 x = 'Country') +
            ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0, size = size.title, face = "bold"),
                  plot.caption = ggplot2::element_text(hjust = 0))

        } else {
          g <- data %>%
            ggplot2::ggplot(ggplot2::aes(x = reorder(Country, value), y = value, fill = as.character(filtro))) +
            ggplot2::geom_col() +
            ggplot2::coord_flip() +
            ggplot2::labs(title =  name.indicator.o, y = 'Indicator achievement',
                 x = 'Country') +
            ggplot2::scale_fill_manual(values = c(color, color.alc)) +
            ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0, size = size.title, face = "bold"),
                  plot.caption = ggplot2::element_text(hjust = 0),
                  legend.position = 'none')
        }




      }

      if(title == FALSE & caption == TRUE) {

        if(length(alc) == 1) {

          g <- data %>%
            ggplot2::ggplot(ggplot2::aes(x = reorder(Country, value), y = value)) +
            ggplot2::geom_col(fill = color) +
            ggplot2::coord_flip() +
            ggplot2::labs(y = 'Indicator achievement',
                 x = 'Country', caption = cap) +
            ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0, size = size.title, face = "bold"),
                  plot.caption = ggplot2::element_text(hjust = 0))

        } else {
          g <- data %>%
            ggplot2::ggplot(ggplot2::aes(x = reorder(Country, value), y = value, fill = as.character(filtro))) +
            ggplot2::geom_col() +
            ggplot2::coord_flip() +
            ggplot2::labs(y = 'Indicator achievement',
                 x = 'Country', caption = cap) +
            ggplot2::scale_fill_manual(values = c(color, color.alc)) +
            ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0, size = size.title, face = "bold"),
                  plot.caption = ggplot2::element_text(hjust = 0),
                  legend.position = 'none')
        }




      }

      if(title == FALSE & caption == FALSE) {

        if(length(alc) == 1) {

          g <- data %>%
            ggplot2::ggplot(ggplot2::aes(x = reorder(Country, value), y = value)) +
            ggplot2::geom_col(fill = color) +
            ggplot2::coord_flip() +
            ggplot2::labs(y = 'Indicator achievement',
                 x = 'Country') +
            ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0, size = size.title, face = "bold"),
                  plot.caption = ggplot2::element_text(hjust = 0))

        } else {
          g <- data %>%
            ggplot2::ggplot(ggplot2::aes(x = reorder(Country, value), y = value, fill = as.character(filtro))) +
            ggplot2::geom_col() +
            ggplot2::coord_flip() +
            ggplot2::labs(y = 'Indicator achievement',
                 x = 'Country') +
            ggplot2::scale_fill_manual(values = c(color, color.alc)) +
            ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0, size = size.title, face = "bold"),
                  plot.caption = ggplot2::element_text(hjust = 0),
                  legend.position = 'none')
        }




      }

      if(save == FALSE) { g } else {
        print(g)
        setwd('~/')
        ggplot2::ggsave(paste0('Indicator achievement ', id.indicator, '.png'), g, width = width, height = height)
        cat(paste0("Chart saved in setwd('~/')"))
      }

    }


  }


# Si se selecciona idioma español
  else {

    if(language.en==FALSE) {

      # 1. Llamar y Filtrar indicadores ----
      indicadores <- call.indicators(language.en = FALSE)

      indicadores <- indicadores %>%
        dplyr::filter(Dimensión == 'Objetivos de Desarrollo Sostenible (ODS)') %>%
        dplyr::mutate(name = ifelse((Indicador.3 =="" & Indicador.2 == ""), Indicador.1,
                                    ifelse(Indicador.3 =="", Indicador.2, Indicador.3))) %>%
        dplyr::filter(`Id del Indicador` == id.indicator)

      name.indicator <- indicadores$name

      n <- length(name.indicator)

      # 2. Advertencia de selección erronea de indicador ----

      if(n == 0) {cat(paste0('Warning: \nSeleccione el id de indicadores de la dimensión de Objetivos de Desarrollo Sostenible (ODS)'))}

      # 3. Continuar con indicador correcto -----
      else{

        # 4. Ajustar nombre de indicador para el título ----
        name.indicator <- stringr::str_split(name.indicator, pattern = '\\s+') %>% unlist()

        l <- length(name.indicator)


        name.indicator.o <- ifelse(l <= 10, paste(name.indicator, collapse = " "),
                                   ifelse(l > 10 & l <= 20, paste0(paste(name.indicator[1:10], collapse = " "),  '\n',
                                                                   paste(name.indicator[11:l], collapse = " ")),
                                          ifelse(l > 20, paste0(paste(name.indicator[1:10], collapse = " "),  '\n',
                                                                paste(name.indicator[11:20], collapse = " "), '\n',
                                                                paste(name.indicator[21:l], collapse = " ")))))

        # 5. Llamar y procesar los datos del indicador ----
        data <- call.data(id.indicator = id.indicator, language.en = FALSE)

        data <- data %>%
          dplyr::group_by(País) %>%
          dplyr::mutate(Maximo = max(Años),
                        filtro = ifelse(Años == Maximo, 1, 0)) %>%
          dplyr::ungroup() %>%
          dplyr::filter(filtro == 1) %>%
          dplyr::group_by(Años, País) %>%
          dplyr::summarise(value = mean(value, na.rm = T)) %>%
          dplyr::ungroup() %>%
          dplyr::arrange(Años) %>%
          dplyr::mutate(filtro = ifelse(País == 'América Latina y el Caribe', 1, 0))


        # 6. Crear bucle para crear el vector de la nota -----
        cap <- unique(data$Años)
        cap.list <- list()
        for(i in 1:length(cap)){

          paises <- data$País[data$Años == cap[i]]
          paises <- paste0(cap[i], ': ', paste(paises, collapse = ', '))
          cap.list[[i]] <- paises

        }

        cap <- do.call(cbind, cap.list) %>% array()
        cap <- paste0('Nota: Último dato disponible, ', paste(cap, collapse = '; '))

        cap <- stringr::str_split(cap, pattern = '\\s+') %>% unlist()

        l <- length(cap)

        cap <- ifelse(l <= 15, paste(cap, collapse = " "),

                      ifelse(l > 15 & l <= 30, paste0('\n', paste(cap[1:15], collapse = " "),  '\n',
                                                      paste(cap[16:l], collapse = " ")),

                             ifelse(l > 30 & l <= 45, paste0('\n', paste(cap[1:15], collapse = " "),  '\n',
                                                             paste(cap[16:30], collapse = " "), '\n',
                                                             paste(cap[31:l], collapse = " ")),

                                    ifelse(l > 45, paste0('\n', paste(cap[1:15], collapse = " "),  '\n',
                                                          paste(cap[16:30], collapse = " "), '\n',
                                                          paste(cap[31:45], collapse = " "), '\n',
                                                          paste(cap[46:l], collapse = " "))))))

        # 7. Crear Gráficos dependiendo de si se encuentra ALC ----

        alc <- unique(data$filtro)

        if(title == TRUE & caption == TRUE) {

          if(length(alc) == 1) {

            g <- data %>%
              ggplot2::ggplot(ggplot2::aes(x = reorder(País, value), y = value)) +
              ggplot2::geom_col(fill = color) +
              ggplot2::coord_flip() +
              ggplot2::labs(title =  name.indicator.o, y = 'Logro del indicador',
                   x = 'País', caption = cap) +
              ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0, size = size.title, face = "bold"),
                    plot.caption = ggplot2::element_text(hjust = 0))

          } else {
            g <- data %>%
              ggplot2::ggplot(ggplot2::aes(x = reorder(País, value), y = value, fill = as.character(filtro))) +
              ggplot2::geom_col() +
              ggplot2::coord_flip() +
              ggplot2::labs(title =  name.indicator.o, y = 'Logro del indicador',
                   x = 'País', caption = cap) +
              ggplot2::scale_fill_manual(values = c(color, color.alc)) +
              ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0, size = size.title, face = "bold"),
                    plot.caption = ggplot2::element_text(hjust = 0),
                    legend.position = 'none')
          }




        }

        if(title == TRUE & caption == FALSE) {


          if(length(alc) == 1) {

            g <- data %>%
              ggplot2::ggplot(ggplot2::aes(x = reorder(País, value), y = value)) +
              ggplot2::geom_col(fill = color) +
              ggplot2::coord_flip() +
              ggplot2::labs(title =  name.indicator.o, y = 'Logro del indicador',
                   x = 'País') +
              ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0, size = size.title, face = "bold"),
                    plot.caption = ggplot2::element_text(hjust = 0))

          } else {
            g <- data %>%
              ggplot2::ggplot(ggplot2::aes(x = reorder(País, value), y = value, fill = as.character(filtro))) +
              ggplot2::geom_col() +
              ggplot2::coord_flip() +
              ggplot2::labs(title =  name.indicator.o, y = 'Logro del indicador',
                   x = 'País') +
              ggplot2::scale_fill_manual(values = c(color, color.alc)) +
              ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0, size = size.title, face = "bold"),
                    plot.caption = ggplot2::element_text(hjust = 0),
                    legend.position = 'none')
          }




        }

        if(title == FALSE & caption == TRUE) {

          if(length(alc) == 1) {

            g <- data %>%
              ggplot2::ggplot(ggplot2::aes(x = reorder(País, value), y = value)) +
              ggplot2::geom_col(fill = color) +
              ggplot2::coord_flip() +
              ggplot2::labs(y = 'Logro del indicador',
                   x = 'País', caption = cap) +
              ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0, size = size.title, face = "bold"),
                    plot.caption = ggplot2::element_text(hjust = 0))

          } else {
            g <- data %>%
              ggplot2::ggplot(ggplot2::aes(x = reorder(País, value), y = value, fill = as.character(filtro))) +
              ggplot2::geom_col() +
              ggplot2::coord_flip() +
              ggplot2::labs(y = 'Logro del indicador',
                   x = 'País', caption = cap) +
              ggplot2::scale_fill_manual(values = c(color, color.alc)) +
              ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0, size = size.title, face = "bold"),
                    plot.caption = ggplot2::element_text(hjust = 0),
                    legend.position = 'none')
          }




        }

        if(title == FALSE & caption == FALSE) {

          if(length(alc) == 1) {

            g <- data %>%
              ggplot2::ggplot(ggplot2::aes(x = reorder(País, value), y = value)) +
              ggplot2::geom_col(fill = color) +
              ggplot2::coord_flip() +
              ggplot2::labs(y = 'Logro del indicador',
                   x = 'País') +
              ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0, size = size.title, face = "bold"),
                    plot.caption = ggplot2::element_text(hjust = 0))

          } else {
            g <- data %>%
              ggplot2::ggplot(ggplot2::aes(x = reorder(País, value), y = value, fill = as.character(filtro))) +
              ggplot2::geom_col() +
              ggplot2::coord_flip() +
              ggplot2::labs(y = 'Logro del indicador',
                   x = 'País') +
              ggplot2::scale_fill_manual(values = c(color, color.alc)) +
              ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0, size = size.title, face = "bold"),
                    plot.caption = ggplot2::element_text(hjust = 0),
                    legend.position = 'none')
          }




        }

        if(save == FALSE) { g } else {

          setwd('~/')
          ggplot2::ggsave(paste0('Logro del indicador ', id.indicator, '.png'), g, width = width, height = height)
          print(g)
          cat(paste0("Gráfico guardado en setwd('~/')"))
        }

      }


    }

  }



}
