REPO_template ReadMe
================
Last compiled on 26 February, 2024

<!-- README.md is generated from README.RMD; knit at end -->

### NEARMI Repository Template

This repository contains the file structure and guidance for annotation
for repositories within the NEARMI GitHub organization.

This template was last updated by [Riley
Mummah](mailto:%20rmummah@usgs.gov) and on 26 February, 2024.

### Example figure

If you’d like to include a figure, you can do so using the code below.
To turn off the code, change `echo = TRUE` to `echo = FALSE`.

``` r
knitr::include_graphics("figures/MA-salamanders.jpg")
```

![](figures/MA-salamanders.jpg)<!-- -->

### Repositiory Structure

- `data/` contains all data used for XX
- `code/` contains all code for data processing and analysis
- `functions/` contains all R functions for ZZ
- `models/` contains all model scripts for XX
- `output/` contains compiled reports and figures

Include a README.Rmd in each folder that outlines all files in the
folder.

We suggest also including all parameter and function information in the
README to orient users to the repository and analysis.

### Parameter definitions

| Model Term | Equation notation | Definition |
|------------|-------------------|------------|
| alpha      | $\alpha$          | intercept  |
| B          | $\beta$           | slope      |

### Functions

f(x) ~ $\alpha$ + $\beta$\*y
