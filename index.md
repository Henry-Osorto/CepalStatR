# CepalStatR

![CepalStatR logo](inst/CEPALSTAT_images/CepalStatR_icon.png)

**Access, explore, and visualize CEPALSTAT statistics from R**

[GitHub](https://github.com/Henry-Osorto/CepalStatR) ·
[CEPALSTAT](https://statistics.cepal.org/portal/cepalstat/) ·
[Paper](https://doi.org/10.18687/LACCEI2024.1.1.1473)

## Overview

**CepalStatR** is an R package designed to provide reproducible access
to **CEPALSTAT**, the statistical portal of the Economic Commission for
Latin America and the Caribbean (**ECLAC/CEPAL**). The package
streamlines the retrieval of indicator metadata and data from the
CEPALSTAT API and offers built-in tools for interactive exploration and
visualization.

The package is especially useful for researchers, analysts, lecturers,
and students working with Latin American and Caribbean statistics who
need a workflow that is fully integrated into R.

## Main features

- Access CEPALSTAT indicators directly from R
- Explore the hierarchical thematic structure of indicators
- Retrieve indicator data in analysis-ready tables
- List countries available in CEPALSTAT dimensions
- Generate demographic visualizations such as population pyramids
- Produce rankings for indicators associated with the Sustainable
  Development Goals
- Visualize the thematic hierarchy of indicators through an interactive
  map

## Installation

Install the development version from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("Henry-Osorto/CepalStatR")
```

## Quick start

``` r
library(CepalStatR)

# Interactive indicator browser
viewer.indicators()

# Download the indicator hierarchy as a data frame
indicators <- call.indicators()

# Download indicator data
df <- call.data(id.indicator = 1)

# Available countries
countries()

# Population pyramids
pyramids(country = "Honduras", years = c(1, 5, 10, 15))

# SDG ranking
ranking.sdg(id.indicator = 3682)

# Interactive thematic map
cepal_topic_map()
```

## Core functions

### Metadata and exploration

- [`call.indicators()`](https://henry-osorto.github.io/CepalStatR/reference/call.indicators.md)
  — downloads the thematic structure of CEPALSTAT indicators
- [`viewer.indicators()`](https://henry-osorto.github.io/CepalStatR/reference/viewer.indicators.md)
  — displays the indicator hierarchy in an interactive HTML table
- `cepal_topic_map()` — creates an interactive thematic tree of
  indicators
- [`countries()`](https://henry-osorto.github.io/CepalStatR/reference/countries.md)
  — returns the list of available countries from CEPALSTAT dimensions

### Data retrieval

- [`call.data()`](https://henry-osorto.github.io/CepalStatR/reference/call.data.md)
  — downloads indicator data and returns an analysis-ready data frame

### Visualization

- [`pyramids()`](https://henry-osorto.github.io/CepalStatR/reference/pyramids.md)
  — generates population pyramids using CEPALSTAT demographic indicators
- [`ranking.sdg()`](https://henry-osorto.github.io/CepalStatR/reference/ranking.sdg.md)
  — creates ranking plots for indicators associated with the Sustainable
  Development Goals

## Screenshots

### Interactive indicator viewer

![viewer.indicators
screenshot](inst/CEPALSTAT_images/viewer.indicators.png)

### Interactive thematic map

![cepal_topic_map screenshot](inst/CEPALSTAT_images/topic_map.png)

## Data source

All data and metadata are obtained from **CEPALSTAT**:

<https://statistics.cepal.org/portal/cepalstat/>

## Why use CepalStatR?

CEPALSTAT provides a rich statistical infrastructure for Latin America
and the Caribbean, but direct use of its API may be cumbersome for many
users. **CepalStatR** simplifies that process by offering a consistent R
interface for metadata discovery, reproducible data acquisition, tidy
outputs, and built-in visual tools for exploratory and applied analysis.

This makes the package especially suitable for:

- empirical research
- policy analysis
- reproducible workflows
- academic teaching
- exploratory analysis of regional statistics

## Citation

If you use **CepalStatR** in academic work, please cite the conference
paper that documents the package:

Osorto, H. (2024). *CepalStatR: a package in R for access to ECLAC
statistics*. 22nd LACCEI International Multi-Conference for Engineering,
Education, and Technology.
<https://doi.org/10.18687/LACCEI2024.1.1.1473>

### BibTeX

``` bibtex
@inproceedings{Osorto2024CepalStatR,
  author    = {Henry Osorto},
  title     = {CepalStatR: a package in R for access to ECLAC statistics},
  booktitle = {22nd LACCEI International Multi-Conference for Engineering, Education, and Technology},
  year      = {2024},
  doi       = {10.18687/LACCEI2024.1.1.1473}
}
```

## Project status

The package currently includes a CRAN-ready structure, interactive
exploration tools, indicator retrieval functions, and built-in
visualization features. Future development may extend the package with
additional metadata utilities, thematic graphics, and analytical
workflows.

## Reporting issues

Bug reports, suggestions, and feature requests are welcome through the
GitHub issue tracker:

<https://github.com/Henry-Osorto/CepalStatR/issues>
