

ranking.ods <- function(id.indicator, language.en=TRUE, save = FALSE,
                        height=5, width=7, size.title=9, title=TRUE,
                        caption=FALSE) {


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

    if(n == 0) {print(paste0('Warning \nSelect the indicator id of the Sustainable Development Goals (SDG) dimension'))}

    # 3. Continuar con indicador correcto -----
    else{

    # 4. Ajustar nombre de indicador para el título ----
      name.indicator <- str_split(name.indicator, pattern = '\\s+') %>% unlist()

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

      cap <- str_split(cap, pattern = '\\s+') %>% unlist()

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

      # Aparte de crear el grafico colocando un color distinto a ALC
      # Hay que crear los gráficos dependiendo de si se va a colocar el título y la nota
      # se deben de hacer tres condiciones: , if(titulo= F & nota = T), if (titulo= T & nota = F)
      # else(titulo=T & nota=T)

      g <- data %>%
        ggplot(aes(x = fct_reorder(Country, value), y = value)) +
        geom_col(fill = '#0A1873') +
        coord_flip() +
        labs(title =  name.indicator.o, y = 'Indicator achievement',
             x = 'Country', caption = cap) +
        theme(plot.title = element_text(hjust = 0, size = 9, face = "bold"),
              plot.caption = element_text(hjust = 0))

    }


  }



}
