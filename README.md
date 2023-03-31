# Soybean-BioCro

The code in this repository reproduces the results in [He & Matthews (2023)](https://doi.org/10.1016/j.fcr.2023.108907). Feel free to contact us or create a new issue on Github if you have any questions.

There are two main parts of the code: 1) Running Soybean-BioCro to generate model outputs; and 2) Plotting using the model outputs. There are some figures that were generated using Matlab regarding the leaf-level FvCB model, which is much easier to run since it needs only a few Matlab files.

The 10-year climate data from 2006-2015 at Bondville, IL are provided here. For more information on the data preparation, please refer to the original Soybean-BioCro paper in [Matthews et al (2022)](https://doi.org/10.1093/insilicoplants/diab032).

The version of BioCro associated with this manuscript is included as a submodule in this repository. To clone this repository with the submodule using the following command:

`git clone --recurse-submodules https://github.com/cropsinsilico/Soybean-Sensitivity.git`

## Running sensitivity experiments with Soybean-BioCro
- Install Soybean-BioCro. See the [BioCro README](https://github.com/ebimodeling/biocro/tree/5cbccbf9f0832e2a2be0a91a41a3f7048782b3cb#readme) to install BioCro.
- Run a 10-year sensitivity: **running/SoybeanBioCro-Sensitivity.R**
- Run a 1000-year (bootstrap) sensitivity: **running/SoybeanBioCro-Sensitivity_Bootstrap.R**

## Plotting
All plotting scripts can be found under the **plotting** folder. Each figure can be reproduced using the respective script as provided. The required model outputs are also included for running these scripts.


The code in this repository was developed and tested on Mac OS 12.5 with R 4.2.1 and on CentOS Linux 7 with R 3.6.0.
