fixture_thematic_tree <- function() {
  indicator <- list(
    name = "Total population, by sex",
    indicator_id = "4788",
    area_id = "1",
    order = 1
  )

  subgroup_2 <- list(
    name = "Population estimates",
    children = list(indicator)
  )
  subgroup_1 <- list(
    name = "Population",
    children = list(subgroup_2)
  )
  group <- list(
    name = "Demography",
    children = list(subgroup_1)
  )
  subdimension <- list(
    name = "Population dynamics",
    children = list(group)
  )
  dimension <- list(
    name = "Demographic statistics",
    children = list(subdimension)
  )
  area <- list(
    name = "Social statistics",
    children = list(dimension)
  )

  list(
    header = list(success = TRUE),
    body = list(
      name = "Root",
      children = list(area)
    )
  )
}

fixture_indicator_response <- function() {
  list(
    header = list(success = TRUE),
    body = list(
      metadata = list(
        indicator_name = "Total population, by sex",
        unit = "Thousands of persons",
        definition = "Population by country and year",
        data_features = "Mocked fixture",
        calculation_methodology = "Not applicable",
        comments = "",
        theme = "Population",
        area = "Social statistics",
        last_update = "2026-01-01"
      ),
      data = list(
        list(
          value = "10.5",
          dim_1 = "100",
          dim_2 = "2020",
          notes_ids = "1"
        ),
        list(
          value = "11.0",
          dim_1 = "200",
          dim_2 = "2021",
          notes_ids = NA_character_
        )
      ),
      dimensions = list(
        list(
          id = "1",
          name = "Country__ESTANDAR",
          members = list(
            list(id = "100", name = "Honduras"),
            list(id = "200", name = "Guatemala")
          )
        ),
        list(
          id = "2",
          name = "Years__ESTANDAR",
          members = list(
            list(id = "2020", name = "2020"),
            list(id = "2021", name = "2021")
          )
        )
      ),
      footnotes = list(
        list(id = "1", description = "Provisional value")
      )
    )
  )
}

fixture_dimensions_response <- function() {
  list(
    header = list(success = TRUE),
    body = list(
      dimensions = list(
        list(
          id = 1,
          name = "Country__ESTANDAR",
          members = data.frame(
            id = c(1, 2, 3),
            name = c("Honduras", "Guatemala", "Honduras"),
            order = c(2, 1, 3),
            stringsAsFactors = FALSE
          )
        ),
        list(
          id = 2,
          name = "Sex__ESTANDAR",
          members = data.frame(
            id = c(1, 2),
            name = c("Men", "Women"),
            order = c(1, 2),
            stringsAsFactors = FALSE
          )
        )
      )
    )
  )
}

fixture_indicator_catalogue <- function() {
  data.frame(
    Area = c("Social statistics", "Social statistics"),
    Dimension = c(
      "Sustainable Development Goals (SDG)",
      "Demographic statistics"
    ),
    Subdimension = c("Goal 1", "Population dynamics"),
    Group = c("Poverty", "Population"),
    `Sub Group Level 1` = c("Target 1.1", "Estimates"),
    `Sub Group Level 2` = c("Indicator 1.1.1", "Total population"),
    `Indicator Name` = c(
      "Population below the international poverty line",
      "Total population, by sex"
    ),
    `Indicator ID` = c("3682", "31"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

fixture_population_data <- function() {
  data.frame(
    Country = rep("Honduras", 4),
    Years = rep(2025, 4),
    Sex = c("Men", "Women", "Men", "Women"),
    `Age__group (each five_year) (0_100 and over)` =
      c("0_4", "0_4", "5_9", "5_9"),
    Value = c(100, 96, 92, 89),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

fixture_sdg_data <- function() {
  data.frame(
    Country = c("Honduras", "Honduras", "Guatemala", "Guatemala"),
    Years = c(2020, 2021, 2020, 2022),
    Value = c(12, 11, 10, 9),
    stringsAsFactors = FALSE
  )
}

