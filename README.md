
# hbrc: Quantifying ecosystem services

<!-- badges: start -->
<!-- badges: end -->

This R package was developed as part of the LiDAR partnership programme between Hawke’s Bay Regional Council and Manaaki Whenua - Landcare Research. The sub-project “Quantifying ecosystem services” was a seed / pilot project that aimed to investigate methods for using LiDAR data products in combination with other national- or global- scale datasets to estimate indicators of some ecosystem services.

The authors take no responsibility for the quality and accuracy of the models included in this R package. Ecosystem services modelling is highly uncertain, and the outputs of these models should not be solely relied on for decision-making.

## Installation

You can install the development version of hbrc like this:

``` r
install.packages("devtools")
devtools::install_github("manaakiwhenua/hbrc")
```

The installation hinges on having a relatively up-to-date version of R, as well as all the requisites for the devtools package.
To use this software you will also need the whitebox, terra, insol, and rayshader packages installed. Insol is not currently available through CRAN, but can be installed from the historical archived versions or from Github [here](https://github.com/cran/insol/tree/master). 

## Tutorial
The hbrc package includes some test datasets from a small region near Wairoa. A more detailed explanation of the functions and the ecosystem service models that underlie them is included in the package [Introduction](https://github.com/manaakiwhenua/hbrc/blob/master/intro-hbrc-v3.pdf). This document also includes some caveats and limitations of the models, references, and an example of quantitatively considering uncertainty when modelling ecosystem services.

## Acknowledgements
The work was partially supported by the Strategic Science Investment Funding for Crown Research Institutes from the New Zealand Ministry of Business, Innovation and Employment’s Science and Innovation Group. This work was also supported by the Environmental Science section of Hawke's Bay Regional Council via the LiDAR tools partnership project Contract Number HBRC-22-716.

The test datasets included in this package were provided by Jan Schindler. The methods for preparing these data were developed with support from the MBIE Catalyst: Strategic Fund through the programme "Bridging the gap between remote sensing and tree modelling" Contract C09X1923.

## Citation
Most of the ecosystem services models used in the package can be cited via the original references given in the documentation, and collectively via our study applying ecosystem service models to Mackenzie District;

Richards DR, Herzig A, Abbott A, Ausseil A-G, Guo J, Sood A, Lavorel S (2023). Diverse contributions of nature to climate change adaptation in an upland landscape. Ecosystems and People 19, 2225647.

If you'd like to cite the package itself, something like this would be fine;

Richards DR (2023). hbrc: Functions for estimating ecosystem services indicators using high-resolution spatial datasets. [https://github.com/manaakiwhenua/hbrc ](https://github.com/manaakiwhenua/hbrc)


