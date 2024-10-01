SeqCalifornia
==================
*....*

* Copyright 2021-23 Noémie Févrat & Vincent Labatut 

CaliforniaMandates is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation. For source availability and license information see `licence.txt`

* Lab site: http://lia.univ-avignon.fr/
* GitHub repo: https://github.com/CompNet/CaliforniaMandates
* Contact: Vincent Labatut <vincent.labatut@univ-avignon.fr>
* Contact : Noemie Fevrat <noemie.fevrat@gmail.com>

-----------------------------------------------------------------------

# Description

This set of scripts takes advantage of data on California elected officials compiled from research previously carried out by **Pr. Thad Kousser** at the **University of California, San Diego**. The results concerning the re-election of Californian elected officials presented in @NoemieFevrat's PhD dissertation are based on these scripts.

In addition, these scripts reproduce the experiments described in the paper published in issue 40 of *Politique Américaine* by @NoemieFevrat (pp.51-80). 

# Data

The scripts are meant to be applied to a corpus of data on California State Legislative elected officials between 1980 and 2012, compiled by Pr.Thad Kousser. The data has been completed and updated, now covering the period 1980-2022. 


# Organization
Here are the folders composing the project:
* Folder `data`: State Legislative elected officials, legislature after legislature
* Folder `plots`: Graphics
* Folder `src` : Set of scripts


# Installation
1. Install the [`R` language](https://www.r-project.org/)
2. Install the following R packages:
   * [`igraph`](http://igraph.org/r/): required (tested with version 1.0.1).
   * [`expm`](https://cran.r-project.org/web/packages/expm/index.html): required for certain signed graph layouts (tested with version 	0.999-2).
3. Download this project from GitHub and unzip the archive.


# Use
In order to replicate the experiments from the article, perform the following operations:

1. Open the `R` console.
2. Set the current projetct directory as the working directory, using `setwd("my/path/to/the/project/SignedBenchmark")`.
3. Run `src/main.R`
  

# Dependencies
* [`igraph`](http://igraph.org/r/) package: used to build and handle graphs.
* [`expm`](https://cran.r-project.org/web/packages/expm/index.html) package: power of matrices.



# References
* Noémie Févrat. "The Effects of U.S State Legislative Term Limits on Political Representation and Professionalization". *Politique Américaine*, 2023, I (40), pp.51-80. ⟨hal-04160170⟩
* Noémie Févrat. *LE "MANDAT DE TROP" ? La réélection des parlementaires et des maires en France et les conditions de sa remise en cause*. Science politique. Avignon Université, 2024. Français. ⟨NNT : ⟩. ⟨tel-04550896⟩
