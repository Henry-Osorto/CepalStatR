

ranking.ods <- function(id.indicator, language.en=TRUE, save = FALSE, height=5, width=7) {

  id <- id.indicator

  if(language.en==TRUE) {

    indicadores <- call.indicators()

    indicadores <- indicadores %>%
      dplyr::filter(Dimension == 'Sustainable Development Goals (SDG)') %>%
      dplyr::mutate(name = ifelse((Indicador.3 =="" & Indicador.2 == ""), Indicador.1,
                           ifelse(Indicador.3 =="", Indicador.2, Indicador.3))) %>%
      dplyr::filter(`Indicator ID` == id)

    name.indicator <- indicadores$name

    n <- length(name.indicator)

    if(n == 0) {print(paste0('Warning \nSelect the indicator id of the Sustainable Development Goals (SDG) dimension'))}
    else{


      l <- str_length(name.indicator)

      data <- call.data(id.indicator = id)

      data <- data %>%
        dplyr::group_by(Country) %>%
        dplyr::mutate(Maximo = max(Years),
                      filtro = ifelse(Years == Maximo, 1, 0)) %>%
        dplyr::ungroup() %>%
        dplyr::filter(filtro == 1)


      # Para hacer la nota del último año se puede hacer un bucle para crear el vector de la nota

      g <- data %>%
        ggplot(aes(x = fct_reorder(Country, value), y = value)) +
        geom_col(fill = '#0A1873') +
        coord_flip() +
        labs(title =  name.indicator, y = 'Indicator achievement', x = 'Country') +
        theme(plot.title = element_text(hjust = 0, size = 9, face = "bold"),
              plot.caption = element_text(hjust = 0))

    }


  }



}
