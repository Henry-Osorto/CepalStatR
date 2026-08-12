# CepalStatR 0.9.1

## SoftwareX peer-review revision

### Testing and validation

* Added automated tests using `testthat` for the main exported functions.
* Added opt-in tests against the live CEPALSTAT API.
* Added tests for provenance metadata.
* Validated the revised package with `R CMD check` with 0 errors,
  0 warnings, and 0 notes.

### API and internal architecture

* Centralized API communication through the internal `cepal_get()` helper.
* Improved validation and handling of API responses.
* Removed duplicated internal utilities.

### Reproducibility

* Added provenance metadata to retrieved objects, including retrieval
  timestamp, package version, indicator identifier, language, and API endpoint.

### Data structures

* Indicator identifiers are now handled as character values to prevent
  unintended numeric formatting.
* Improved consistency of returned objects.

### Dependencies

* Removed unused dependencies and simplified the package dependency set.

### Documentation and visualization

* Improved `viewer.indicators()` display of indicator identifiers.
* Updated `topic_map()` examples to better expose the thematic hierarchy.
* Updated documentation associated with the revised software architecture.

This GitHub release contains changes introduced during peer review of the
SoftwareX manuscript. Version 0.9.0 remains the current CRAN release.
