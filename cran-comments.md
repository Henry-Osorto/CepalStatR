## Test environments

- Local: Windows 11, R 4.5.3
- devtools::check(): 0 errors | 0 warnings | 0 notes
- devtools::check(cran = TRUE): 0 errors | 0 warnings | 0 notes
- win-builder (R-release): checked successfully after corrections

## Resubmission

This is a resubmission.

In the previous submission, CRAN incoming pretests reported 2 NOTEs:
- top-level non-standard file `cran-comments.md`
- spelling-related NOTE in `DESCRIPTION`

These issues have been fixed:
- `cran-comments.md` was excluded from the package bundle via `.Rbuildignore`
- `Title` and `Description` were revised to avoid flagged terms

## Additional comments

- The package provides an interface to access and explore data from the public ECLAC statistical portal.
- No authentication or API key is required.
- All examples are lightweight and designed to run quickly.
- The package includes both static and interactive visualization tools.
