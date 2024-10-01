SeqCalifornia
==================
*Sequence analysis of political trajectories over Californian data*

* Copyright 2021-23 Noémie Févrat & Vincent Labatut 

SeqCalifornia is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation. For source availability and license information see `licence.txt`

* Lab site: http://lia.univ-avignon.fr/
* GitHub repo: https://github.com/CompNet/SeqCalifornia
* Contact: Noemie Fevrat <noemie.fevrat@gmail.com>, Vincent Labatut <vincent.labatut@univ-avignon.fr>

-----------------------------------------------------------------------

# Description
This set of scripts takes advantage of data on California elected officials compiled from research previously carried out by Pr. Thad Kousser at the University of California, San Diego. The results concerning the re-election of Californian elected officials presented in Noémie Fevrat's PhD dissertation [[F'24](#references)] are based on these scripts.

In addition, these scripts reproduce the experiments described in the paper published in issue 40 of *Politique Américaine* by Noémie Fevrat [[F'23](#references)]. 


# Data
The scripts are meant to be applied to a corpus of data on California State Legislative elected officials between 1980 and 2012, compiled by Pr.Thad Kousser. The data has been completed and updated, now covering the period 1980-2022. It is available within this GitHub repository. 


# Organization
Here are the folders composing the project:
* Folder `data`: state Legislative elected officials, legislature after legislature.
* Folder `plots`: plots generated by the scripts.
* Folder `src` : set of scripts.


# Installation
1. Install the [`R` language](https://www.r-project.org/)
2. Download this project from GitHub and unzip the archive.
3. Use the `install.R` script to install the required packages.


# Use
In order to replicate the experiments from the article, perform the following operations:

1. Open the `R` console.
2. Set the current projet directory as the working directory, using `setwd("my/path/to/the/project/SignedBenchmark")`.
3. Run the main script `src/main.R


# Dependencies
* [`igraph`](http://igraph.org/r/) package: used to build and handle graphs.
* [`readr`](https://cran.r-project.org/web/packages/readr/index.html) package: read rectangular text data.
* [`dplyr`](https://cran.r-project.org/web/packages/dplyr/index.html) package: data manipulation.
* [`TraMineR`]([https://cran.r-project.org/web/packages/dplyr/index.html](http://traminer.unige.ch/)) package: sequence analysis.
* [`GDAtools`](https://cran.r-project.org/web/packages/GDAtools/index.html) package: geometric data analysis.


# References
* **[F'23]** Noémie Févrat. "The Effects of U.S State Legislative Term Limits on Political Representation and Professionalization". *Politique Américaine*, 2023, I (40), pp.51-80. ⟨hal-04160170⟩
* **[F'24]** Noémie Févrat. *Le "mandat de trop" ? La réélection des parlementaires et des maires en France et les conditions de sa remise en cause*. PHD Thesis, Avignon Université, 2024. ⟨tel-04550896⟩
