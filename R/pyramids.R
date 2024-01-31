#' Population pyramids
#'
#' @description
#' A short description...
#'
#' @usage pyramids(country, years, language.en, language.en = TRUE, color = c('#B20B27', '#0A1873'),
#'          save = FALSE, height = 5, width = 7)
#'
#' @param country
#' Write the name of the country
#' @param years
#' It is a vector that contains values between 1 and 31. These values reflect the position of a vector of years in 5-year intervals.
#' The starting year is 1950 (1) and the ending year is 2100 (31).
#' @param language.en
#' If TRUE or omitted is selected, the default language will be English.
#' Select FALSE to choose the Spanish language.
#' @param color
#' It is a vector that provides two colored characters in RGB scale: c('#B20B27', '#0A1873').
#' However, you can modify the colors using the conventional names (red, yellow, blue, green, etc.).
#' @param save
#' It is a logical parameter that allows saving the graph in png format in the setwd('~/') directory.
#' By default the save option is FALSE. Select TRUE if you want to save the graph.
#' @param height
#' Determines the height of the graph. You can adjust the size by increasing or decreasing.
#' By default the height is 5 units ("in", "cm", "mm", or "px", depending on the device).
#' @param width
#' Determines the width of the graph. You can adjust the size by increasing or decreasing.
#' By default the width is 7 units ("in", "cm", "mm", or "px", depending on the device).
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
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 ggsave
#' @importFrom gridExtra grid.arrange
#'
#' @examples
#' pyramids(country = "Latin America", years = 16)
#'
#'# With the values of the years parameter the years 1950, 2000, 2025,
#'# 2050 and 2100 will be obtained.
#' pyramids(country = "Latin America", years = c(1, 11, 16, 21, 25, 31))
#'
#'# To save the graph in setwd('~/') set save to TRUE
#' pyramids(country = "Latin America", years = c(1, 11, 16, 21, 25, 31),
#'          save = TRUE, height = 8, width = 10)


pyramids <- function(country, years=1:31, language.en = TRUE, color = c('#B20B27', '#0A1873'),
                     save = FALSE, height=5, width=7) {


suppressWarnings({

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
        ggplot2::ggplot(ggplot2::aes(x = Edad, y = value, fill = Sex)) +
        ggplot2::geom_bar(stat = 'identity', subset = (Sex == 'Men')) +
        ggplot2::geom_bar(stat = 'identity', subset = (Sex == 'Women')) +
        ggplot2::scale_y_continuous(breaks = v, labels = abs(v)) +
        ggplot2::coord_flip() +
        ggplot2::scale_fill_manual(values = color) +
        ggplot2::labs(subtitle = paste0('Year: ',a), y = 'Thousands of people', x = 'Age', fill = '') }

    g <- do.call(gridExtra::grid.arrange, p)

    if(save == TRUE) {

      setwd('~/')
      ggplot2::ggsave('Population pyramids.png', plot = g, height = height, width = width)

  }



  }

  else{

    df <- call.data(id.indicator = 31, language.en = F)

    country.filter <- country
    years.filter <- seq(1950, 2100, 5)
    years.filter <- years.filter[years]

    edades <- unique(df$`Edad__Grupos de edad (cada 5 años) (0_100 mas)`)

    df <- df %>%
      dplyr::filter(País %in% country.filter,
             Años %in% years.filter,
             `Edad__Grupos de edad (cada 5 años) (0_100 mas)` != 'Total edades',
             Sexo != 'Ambos sexos') %>%
      dplyr::mutate(Edad = ifelse(`Edad__Grupos de edad (cada 5 años) (0_100 mas)` == '0_4', 1,
                           ifelse(`Edad__Grupos de edad (cada 5 años) (0_100 mas)` == '5_9', 2,
                           ifelse(`Edad__Grupos de edad (cada 5 años) (0_100 mas)` == '10_14', 3,
                           ifelse(`Edad__Grupos de edad (cada 5 años) (0_100 mas)` == '15_19', 4,
                           ifelse(`Edad__Grupos de edad (cada 5 años) (0_100 mas)` == '20_24', 5,
                           ifelse(`Edad__Grupos de edad (cada 5 años) (0_100 mas)` == '25_29', 6,
                           ifelse(`Edad__Grupos de edad (cada 5 años) (0_100 mas)` == '30_34', 7,
                           ifelse(`Edad__Grupos de edad (cada 5 años) (0_100 mas)` == '35_39', 8,
                           ifelse(`Edad__Grupos de edad (cada 5 años) (0_100 mas)` == '40_44', 9,
                           ifelse(`Edad__Grupos de edad (cada 5 años) (0_100 mas)` == '45_49', 10,
                           ifelse(`Edad__Grupos de edad (cada 5 años) (0_100 mas)` == '50_54', 11,
                           ifelse(`Edad__Grupos de edad (cada 5 años) (0_100 mas)` == '55_59', 12,
                           ifelse(`Edad__Grupos de edad (cada 5 años) (0_100 mas)` == '60_64', 13,
                           ifelse(`Edad__Grupos de edad (cada 5 años) (0_100 mas)` == '65_69', 14,
                           ifelse(`Edad__Grupos de edad (cada 5 años) (0_100 mas)` == '70_74', 15,
                           ifelse(`Edad__Grupos de edad (cada 5 años) (0_100 mas)` == '75_79', 16,
                           ifelse(`Edad__Grupos de edad (cada 5 años) (0_100 mas)` == '80_84', 17,
                           ifelse(`Edad__Grupos de edad (cada 5 años) (0_100 mas)` == '85_89', 18,
                           ifelse(`Edad__Grupos de edad (cada 5 años) (0_100 mas)` == '90_94', 19,
                           ifelse(`Edad__Grupos de edad (cada 5 años) (0_100 mas)` == '95_99', 20,
                           ifelse(`Edad__Grupos de edad (cada 5 años) (0_100 mas)` == '100 y más', 21, NA))))))))))))))))))))))

    Edades <- c("0 - 4", "5 - 9", "10 - 14", "15 - 19", "20 - 24",
                "25 - 29", "30 - 34", "35 - 39", "40 - 44", "45 - 49",
                "50 - 54", "55 - 59", "60 - 64", "65 - 69", "70 - 74", "75 - 79",
                "80 - 84", "85 - 89", "90 - 94", "95 - 99", "100 +")

    df$Edad <- factor(df$Edad, levels = c(1:21), labels = Edades)

    df <- df %>%
      dplyr::mutate(value = ifelse(Sexo == 'Hombres', -1*value, value))

    v <- round(c(min(df$value), min(df$value)/2, 0, max(df$value)/2, max(df$value)),0)
    Sexo <- c('Hombre','Mujer')

    p <- list()

    for(i in 1:length(years.filter)) {

      data <- df %>%
        dplyr::filter(Años == years.filter[i])

      v <- round(c(min(data$value), min(data$value)/2, 0, max(data$value)/2, max(data$value)),0)
      a <- years.filter[i]

      p[[i]] <- data %>%
        ggplot2::ggplot(ggplot2::aes(x = Edad, y = value, fill = Sexo)) +
        ggplot2::geom_bar(stat = 'identity', subset = (Sexo == 'Hombre')) +
        ggplot2::geom_bar(stat = 'identity', subset = (Sexo == 'Mujer')) +
        ggplot2::scale_y_continuous(breaks = v, labels = abs(v)) +
        ggplot2::coord_flip() +
        ggplot2::scale_fill_manual(values = color) +
        ggplot2::labs(subtitle = paste0('Año: ',a), y = 'Miles de Personas', fill = '') }

    g <- do.call(gridExtra::grid.arrange, p)

    if(save == TRUE)  {

      setwd('~/')
      ggplot2::ggsave('Pirámide Poblacional.png', plot = g, height = height, width = width)

    }



  }
  })

}



