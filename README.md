# CepalStatR

<!-- badges: start -->
[![R-CMD-check](https://github.com/Henry-Osorto/CepalStatR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Henry-Osorto/CepalStatR/actions)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
<!-- badges: end -->

<p align="center">
  <img src="inst/CEPALSTAT_images/CepalStatR_icon.png" alt="CepalStatR logo" width="700">
</p>

<p align="center">
  <strong>R interface to access, explore and visualize CEPALSTAT indicators</strong>
</p>

<p align="center">
  <a href="https://statistics.cepal.org/portal/cepalstat/">CEPALSTAT Portal</a> ·
  <a href="https://doi.org/10.18687/LACCEI2024.1.1.1473">Paper (LACCEI 2024)</a> ·
  <a href="https://github.com/Henry-Osorto/CepalStatR/issues">Report Issue</a>
</p>

---

## Overview

**CepalStatR** is an R package that provides a reproducible and user-friendly interface to access data and metadata from **CEPALSTAT**, the statistical portal of the Economic Commission for Latin America and the Caribbean (**ECLAC/CEPAL**).

The package is designed to simplify the interaction with the CEPALSTAT API, allowing users to:

- explore the hierarchical structure of indicators,
- download data in tidy formats,
- and generate both static and interactive visualizations.

It is particularly useful for **researchers, analysts, and students** working with Latin American and Caribbean statistics.

---

## Main Features

- Access CEPALSTAT indicators directly from R
- Retrieve metadata and hierarchical structures
- Download indicator data in tidy format
- List countries available in CEPALSTAT
- Generate demographic visualizations (population pyramids)
- Create SDG indicator rankings
- Explore indicators via interactive tables and thematic maps

---

## Installation

Install the development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("Henry-Osorto/CepalStatR")
