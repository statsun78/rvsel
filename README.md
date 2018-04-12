# rvsel
[R package] Rare variant selection procedure

## Overview

Rare Variant Selection Procedure: When a gene or a genetic region is significantly associated with a disease or a trait, the rare variant selection procedure is able to distinguish causal (risk or protective) rare variants from noncausal rare variants located within the same gene or the same genetic region. 

## Installation

```
## "devtools" pacakge is required if you don't have it.  
install.packages('devtools')

## For window users, you need to download and install Rtools from https://cran.r-project.org/bin/windows/Rtools

library(devtools)
install_github("statsun78/rvsel")
```

## References

* **Sun, H.** and Wang, S. (2014) A Power Set Based Statistical Selection Procedure to Locate Susceptible Rare Variants Associated with Complex Diseases with Sequencing Data, *Bioinformatics* 30(16), p.2317-2323.
* Kim, S., Lee, K. and **Sun, H.** (2015) Statistical Selection Strategy for Risk and Protective Rare Variants Associated with Complex Traits, *Journal of Computational Biology* 22(11), p.1034-1043.
* Lee, G. and **Sun, H.** (2017) Selection Probability for Rare Variant Association Studies, *Journal of Computational Biology* 24(5), p.400-411
