

call.indicators <- function(language.en = TRUE) {

  if(language.en == TRUE) {

    # 1. Definir url de la API ----

    url.indicators <- 'https://api-cepalstat.cepal.org/cepalstat/api/v1/thematic-tree?lang=en&format=csv'

    # 2. Importar datos en csv ----
    data <- read.csv(url.indicators)

    # 3. Crear Subconjuntos de datos por áreas e indicadores ----

    n.1 <- data %>%
      dplyr::filter(indentation == 6)

    n.2 <- data %>%
      dplyr::filter(indentation == 5)

    n.3 <- data %>%
      dplyr::filter(indentation == 4)

    n.4 <- data %>%
      dplyr::filter(indentation == 3)

    n.5 <- data %>%
      dplyr::filter(indentation == 2)

    n.6 <- data %>%
      dplyr::filter(indentation == 1)

    # 4. Unir Subconjuntos de datos ----
    n.1 <- n.1 %>%
      dplyr::select(id, name, parent)

    n.2 <- n.2 %>%
      dplyr::left_join(n.1, by = c('area_id' = 'parent')) %>%
      dplyr::select(id.x, name.x, parent, id.y, name.y)

    n.3 <- n.3 %>%
      dplyr::left_join(n.2, by = c('area_id' = 'parent')) %>%
      dplyr::select(id, name, parent, id.x, name.x,  id.y, name.y)

    n.4 <- n.4 %>%
      dplyr::left_join(n.3, by = c('area_id' = 'parent')) %>%
      dplyr::select(id.x.x, name.x.x, parent, id.y, name.y,id.x, name.x,  id.y.y, name.y.y)

    n.5 <- n.5 %>%
      dplyr::left_join(n.4, by = c('area_id' = 'parent')) %>%
      dplyr::select(id, name, parent, id.x.x, name.x.x, id.y, name.y, id.x, name.x,  id.y.y, name.y.y)

    n.6 <- n.6 %>%
      dplyr::left_join(n.5, by = c('area_id' = 'parent'))

    data <- n.6 %>%
      dplyr::select(Area = name.x.x.x, Dimension = name.y, Subdimension = name.x.x,
             Indicador.1 = name.y.y, Indicador.2 = name.x, Indicador.3 = name.y.y.y,
             id.y.y, id.x, id.y.y.y) %>%
      dplyr::mutate(`Indicator ID` = ifelse(is.na(id.y.y.y) & is.na(id.x), id.y.y,
                                         ifelse(is.na(id.y.y.y), id.x, id.y.y.y))) %>%
      dplyr::select(-id.y.y.y, -id.y.y, -id.x) %>%
      dplyr::mutate_at(4:6, ~ifelse(is.na(.), '', .))

    }

  else {

    # 1. Definir url de la API ----
    url.indicators <- 'https://api-cepalstat.cepal.org/cepalstat/api/v1/thematic-tree?lang=es&format=csv'

    # 2. Importar datos en csv ----
    data <- read.csv(url.indicators)

    # 3. Crear Subconjuntos de datos por áreas e indicadores ----

    n.1 <- data %>%
      dplyr::filter(indentation == 6)

    n.2 <- data %>%
      dplyr::filter(indentation == 5)

    n.3 <- data %>%
      dplyr::filter(indentation == 4)

    n.4 <- data %>%
      dplyr::filter(indentation == 3)

    n.5 <- data %>%
      dplyr::filter(indentation == 2)

    n.6 <- data %>%
      dplyr::filter(indentation == 1)

    # 4. Unir Subconjuntos de datos ----
    n.1 <- n.1 %>%
      dplyr::select(id, name, parent)

    n.2 <- n.2 %>%
      dplyr::left_join(n.1, by = c('area_id' = 'parent')) %>%
      dplyr::select(id.x, name.x, parent, id.y, name.y)

    n.3 <- n.3 %>%
      dplyr::left_join(n.2, by = c('area_id' = 'parent')) %>%
      dplyr::select(id, name, parent, id.x, name.x,  id.y, name.y)

    n.4 <- n.4 %>%
      dplyr::left_join(n.3, by = c('area_id' = 'parent')) %>%
      dplyr::select(id.x.x, name.x.x, parent, id.y, name.y,id.x, name.x,  id.y.y, name.y.y)

    n.5 <- n.5 %>%
      dplyr::left_join(n.4, by = c('area_id' = 'parent')) %>%
      dplyr::select(id, name, parent, id.x.x, name.x.x, id.y, name.y, id.x, name.x,  id.y.y, name.y.y)

    n.6 <- n.6 %>%
      dplyr::left_join(n.5, by = c('area_id' = 'parent'))

    data <- n.6 %>%
      dplyr::select(Área = name.x.x.x, Dimensión = name.y, Subdimensión = name.x.x,
             Indicador.1 = name.y.y, Indicador.2 = name.x, Indicador.3 = name.y.y.y,
             id.y.y, id.x, id.y.y.y) %>%
      dplyr::mutate(`Id del Indicador` = ifelse(is.na(id.y.y.y) & is.na(id.x), id.y.y,
                                         ifelse(is.na(id.y.y.y), id.x, id.y.y.y))) %>%
      dplyr::select(-id.y.y.y, -id.y.y, -id.x) %>%
      dplyr::mutate_at(4:6, ~ifelse(is.na(.), '', .))
  }

  return(data)

  }


