## Test environments

- Local: Windows 11, R 4.5.3
- devtools::check(): 0 errors | 0 warnings | 0 notes
- devtools::check(cran = TRUE): 0 errors | 0 warnings | 0 notes
- devtools::check_win_release(): 0 errors | 0 warnings | 2 notes

## R CMD check results

0 errors | 0 warnings | 0 notes (local CRAN checks)

## Notes

- The package interfaces with the public CEPALSTAT API.
- No authentication or API key is required.
- Functions include basic error handling for API connectivity.
- Examples are lightweight and designed to avoid long execution times.

## Additional comments

- This is the first submission of the package.
- The NOTE regarding "possibly misspelled words" in DESCRIPTION refers to
  "CEPALSTAT" and "ECLAC", which are official names of institutions and not misspellings.
- The NOTE related to HTML validation was corrected by removing improperly encoded text
  in the documentation.

The package is ready for submission.
