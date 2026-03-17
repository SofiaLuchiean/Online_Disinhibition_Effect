# A formal model of the Online Disinhibition Effect

The reproducible code provided in the `/simulation` subfolder allows for full replication of the simulations, statistical analyses, and visualizations of a model derived from the formalized theory.

All analyses were performed the using the software program R, version 4.4.2 (R Core Team, 2024) and RStudio (Posit team, 2025). The following packages were used:

-   ggplot2
-   multcomp
-   MBESS
-   haven
-   here

to install, please run\
`install.packages(c("ggplot2", "multcomp", "MBESS", "haven", "here"))`

Raw data in `/raw_data` used in `/additional analysis 01 - base_resp distribution.R` is adapted from "Transcendent Accountability Scale Development Data and Materials" on OSF by Witvliet et. al. (2022). The original data is licensed under CC-BY 4.0

## How to reproduce the simulation

To reproduce the full simulation, run the R scripts in numerical order. Scripts 02 and 04 generate plots and can be skipped if visualization is not required.

## References

Kelley, K. (2023). *MBESS: The MBESS R package* (Version 4.9.3). <https://CRAN.R-project.org/package=MBESS>

Hothorn, T., Bretz, F., & Westfall, P. (2008). Simultaneous inference in general parametric models. *Biometrical Journal, 50*(3), 346–363.

Müller, K. (2025). *here: A simpler way to find your files* (Version 1.0.2). <https://CRAN.R-project.org/package=here>

Posit team (2025). *RStudio: Integrated Development Environment for R.* Posit Software, PBC, Boston, MA. <http://www.posit.co/>

R Core Team (2024). *R: A Language and Environment for Statistical Computing. R Foundation for Statistical Computing*. Vienna, Austria. <https://www.R-project.org>

Wickham, H. (2016). *ggplot2: Elegant graphics for data analysis.* Springer-Verlag New York. <https://ggplot2.tidyverse.org>

Wickham, H., Miller, E., & Smith, D. (2025). *haven: Import and export 'SPSS', 'Stata' and 'SAS' files* (Version 2.5.5). <https://CRAN.R-project.org/package=haven>

Witvliet, C. V. O., Johnson, B. R., Roberts, R., Jang, S. J., Evans, C. S., Peteet, J., Berry, J. W., Leman, J., Torrance, A., & Bradshaw, M. (2022). *Transcendent Accountability Scale Development Data and Materials.* <https://doi.org/10.17605/OSF.IO/GV6ZS>
