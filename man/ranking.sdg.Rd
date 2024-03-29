% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ranking.ods.R
\name{ranking.sdg}
\alias{ranking.sdg}
\title{Sustainable Development Goals Ranking}
\usage{
ranking.sdg(
  id.indicator,
  language.en = TRUE,
  save = FALSE,
  height = 5,
  width = 9,
  size.title = 10,
  title = TRUE,
  caption = TRUE,
  color = "#032B47",
  color.gc = "#36B3FF"
)
}
\arguments{
\item{id.indicator}{You must determine the ID of the indicator or variable that you want to obtain.}

\item{language.en}{If true or omitted is selected, the default language will be English. Select False to choose the Spanish language.}

\item{save}{It is a logical parameter that allows saving the graph in png format in the setwd('~/') directory.
By default the save option is FALSE. Select TRUE if you want to save the graph.}

\item{height}{Determines the height of the graph. You can adjust the size by increasing or decreasing.
By default the height is 5 units ("in", "cm", "mm", or "px", depending on the device).}

\item{width}{Determines the width of the graph. You can adjust the size by increasing or decreasing.
By default the width is 9 units ("in", "cm", "mm", or "px", depending on the device).}

\item{title}{It is a logical parameter that allows you to activate or deactivate the display of the graph title.
By default the title display option is TRUE. Select FALSE if you do not want the title to be displayed.}

\item{caption}{It is a logical parameter that allows you to activate or deactivate the display of the graph note.
The grade determines the last year in which the indicator was obtained.
By default the option to view the note is TRUE. Select FALSE if you do not want the note to be displayed.}

\item{color}{It is a character that contains the color that all countries within the graph will have.
By default, the RGB scale code is used: '#032B47'. However, you can modify the color using the conventional names.}

\item{color.gc}{It is a character that is activated when groups of countries are available in the graph: Latin America and the Caribbean, Central America, South America, etc.
By default, the RGB scale code is used: '#36B3FF'. However, you can modify the color using the conventional names.}
}
\description{
Sustainable Development Goals Ranking
}
