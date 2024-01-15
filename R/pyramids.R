#' Population pyramids
#'
#' @description
#' A short description...
#'
#'@usage pyramids(country, years, language.en,
#'                language.en = TRUE, color=c('#B20B27', '#0A1873'),
#'                save=FALSE, height, width)
#'
#' @param country
#' @param years
#' @param language.en
#' @param color
#' @param save
#' @param height
#' @param width
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 coord_flip
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 ggsave
#' @importFrom gridExtra grid.arrange
#'
#'
#'
#' @examples


pyramids <- function(country, years=1:31, language.en = TRUE, color=c('#B20B27', '#0A1873'),
                     save=FALSE, height=5, width=7) {



  if(language.en == TRUE) {


    df <- call.data(id.indicator = 31, language.en = TRUE)

    country.filter <- country
    years.filter <- seq(1950, 2100, 5)
    years.filter <- years.filter[years]

    edades <- unique(df$`Age__group (each five_year) (0_100 and over)`)

    df <- df %>%
      dplyr::filter(Country %in% country.filter,
             Years %in% years.filter,
             `Age__group (each five_year) (0_100 and over)` != 'Total age',
             Sex != 'Both sexes') %>%
      dplyr::mutate(Edad = ifelse(`Age__group (each five_year) (0_100 and over)` == '0_4', 1,
                    ifelse(`Age__group (each five_year) (0_100 and over)` == '5_9', 2,
                    ifelse(`Age__group (each five_year) (0_100 and over)` == '10_14', 3,
                    ifelse(`Age__group (each five_year) (0_100 and over)` == '15_19', 4,
                    ifelse(`Age__group (each five_year) (0_100 and over)` == '20_24', 5,
                    ifelse(`Age__group (each five_year) (0_100 and over)` == '25_29', 6,
                    ifelse(`Age__group (each five_year) (0_100 and over)` == '30_34', 7,
                    ifelse(`Age__group (each five_year) (0_100 and over)` == '35_39', 8,
                    ifelse(`Age__group (each five_year) (0_100 and over)` == '40_44', 9,
                    ifelse(`Age__group (each five_year) (0_100 and over)` == '45_49', 10,
                    ifelse(`Age__group (each five_year) (0_100 and over)` == '50_54', 11,
                    ifelse(`Age__group (each five_year) (0_100 and over)` == '55_59', 12,
                    ifelse(`Age__group (each five_year) (0_100 and over)` == '60_64', 13,
                    ifelse(`Age__group (each five_year) (0_100 and over)` == '65_69', 14,
                    ifelse(`Age__group (each five_year) (0_100 and over)` == '70_74', 15,
                    ifelse(`Age__group (each five_year) (0_100 and over)` == '75_79', 16,
                    ifelse(`Age__group (each five_year) (0_100 and over)` == '80_84', 17,
                    ifelse(`Age__group (each five_year) (0_100 and over)` == '85_89', 18,
                    ifelse(`Age__group (each five_year) (0_100 and over)` == '90_94', 19,
                    ifelse(`Age__group (each five_year) (0_100 and over)` == '95_99', 20,
                    ifelse(`Age__group (each five_year) (0_100 and over)` == '100 and over', 21, NA))))))))))))))))))))))

    Edades <- c("0 - 4", "5 - 9", "10 - 14", "15 - 19", "20 - 24",
                "25 - 29", "30 - 34", "35 - 39", "40 - 44", "45 - 49",
                "50 - 54", "55 - 59", "60 - 64", "65 - 69", "70 - 74", "75 - 79",
                "80 - 84", "85 - 89", "90 - 94", "95 - 99", "100 and over")

    df$Edad <- factor(df$Edad, levels = c(1:21), labels = Edades)

    df <- df %>%
      dplyr::mutate(value = ifelse(Sex == 'Men', -1*value, value))

    v <- round(c(min(df$value), min(df$value)/2, 0, max(df$value)/2, max(df$value)),0)
    Sex <- c('Men','Women')

    p <- list()

    for(i in 1:length(years.filter)) {

      data <- df %>%
        dplyr::filter(Years == years.filter[i])

      v <- round(c(min(data$value), min(data$value)/2, 0, max(data$value)/2, max(data$value)),0)
      a <- years.filter[i]

      p[[i]] <- data %>%
        ggplot2::ggplot(aes(x = Edad, y = value, fill = Sex)) +
        ggplot2::geom_bar(stat = 'identity', subset = (Sex == 'Men')) +
        ggplot2::geom_bar(stat = 'identity', subset = (Sex == 'Women')) +
        ggplot2::scale_y_continuous(breaks = v, labels = abs(v)) +
        ggplot2::coord_flip() +
        ggplot2::scale_fill_manual(values = color) +
        ggplot2::labs(subtitle = paste0('Year: ',a), y = 'Miles de Personas', x = 'Age', fill = '') }

    g <- do.call(gridExtra::grid.arrange, p)

    if(save == FALSE) {

      g }

    else{

      setwd('~/')
      ggplot2::ggsave('Pirámide Poblacional.png', plot = g, height = height, width = width)

  }



  }

  else{

    f <- call.data(id.indicator = 31, language.en = F)

    country.filter <- country
    years.filter <- seq(1950, 2100, 5)
    years.filter <- years.filter[years]

    edades <- unique(df$Labels_dim_267)

    df <- df %>%
      filter(Labels_dim_208 %in% country.filter,
             Labels_dim_29117 %in% years.filter,
             Labels_dim_267 != 'Total edades',
             Labels_dim_144 != 'Ambos sexos') %>%
      mutate(Edad = ifelse(Labels_dim_267 == '0_4', 1,
                           ifelse(Labels_dim_267 == '5_9', 2,
                                  ifelse(Labels_dim_267 == '10_14', 3,
                                         ifelse(Labels_dim_267 == '15_19', 4,
                                                ifelse(Labels_dim_267 == '20_24', 5,
                                                       ifelse(Labels_dim_267 == '25_29', 6,
                                                              ifelse(Labels_dim_267 == '30_34', 7,
                                                                     ifelse(Labels_dim_267 == '35_39', 8,
                                                                            ifelse(Labels_dim_267 == '40_44', 9,
                                                                                   ifelse(Labels_dim_267 == '45_49', 10,
                                                                                          ifelse(Labels_dim_267 == '50_54', 11,
                                                                                                 ifelse(Labels_dim_267 == '55_59', 12,
                                                                                                        ifelse(Labels_dim_267 == '60_64', 13,
                                                                                                               ifelse(Labels_dim_267 == '65_69', 14,
                                                                                                                      ifelse(Labels_dim_267 == '70_74', 15,
                                                                                                                             ifelse(Labels_dim_267 == '75_79', 16,
                                                                                                                                    ifelse(Labels_dim_267 == '80_84', 17,
                                                                                                                                           ifelse(Labels_dim_267 == '85_89', 18,
                                                                                                                                                  ifelse(Labels_dim_267 == '90_94', 19,
                                                                                                                                                         ifelse(Labels_dim_267 == '95_99', 20,
                                                                                                                                                                ifelse(Labels_dim_267 == '100 y más', 21, NA))))))))))))))))))))))

    Edades <- c("0 - 4", "5 - 9", "10 - 14", "15 - 19", "20 - 24",
                "25 - 29", "30 - 34", "35 - 39", "40 - 44", "45 - 49",
                "50 - 54", "55 - 59", "60 - 64", "65 - 69", "70 - 74", "75 - 79",
                "80 - 84", "85 - 89", "90 - 94", "95 - 99", "100 +")

    df$Edad <- factor(df$Edad, levels = c(1:21), labels = Edades)

    df <- df %>%
      mutate(value = ifelse(Labels_dim_144 == 'Hombres', -1*value, value))

    v <- round(c(min(df$value), min(df$value)/2, 0, max(df$value)/2, max(df$value)),0)
    Sexo <- c('Hombre','Mujer')

    p <- list()

    for(i in 1:length(years.filter)) {

      data <- df %>%
        filter(Labels_dim_29117 == years.filter[i])

      v <- round(c(min(data$value), min(data$value)/2, 0, max(data$value)/2, max(data$value)),0)
      a <- years.filter[i]

      p[[i]] <- data %>%
        ggplot(aes(x = Edad, y = value, fill = Labels_dim_144)) +
        geom_bar(stat = 'identity', subset = (Sexo == 'Hombre')) +
        geom_bar(stat = 'identity', subset = (Sexo == 'Mujer')) +
        scale_y_continuous(breaks = v, labels = abs(v)) +
        coord_flip() +
        scale_fill_manual(values = color) +
        labs(subtitle = paste0('Año: ',a), y = 'Miles de Personas', fill = '') }

    g <- do.call(grid.arrange, p)

    if(save == FALSE) {

      g }

    else{

      setwd('~/')
      ggsave('Pirámide Poblacional.png', plot = g, height = height, width = width)

    }



  }
  }



