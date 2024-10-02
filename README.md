### Table of contents

* [Project description](#Project)

* [Manuscript title](#Article)

* [Abstract](#Abstract)

* [Usage](#Usage)

* [Citation guidelines](#Citation-guidelines)


### Project

- **SAFEGUARD** (Safeguarding European wild pollinators). Horizon 2020 (No. 101003476). Task 1.5: Mapping European Pollinator species interactions and community assembly

### Article

- **EuPPollNet: A European database of plant-pollinator networks**

### Abstract

- **Motivation**: Pollinators play a crucial role in maintaining Earth’s terrestrial biodiversity and human food production by mediating sexual reproduction for most flowering plants. Indeed, the network of interactions formed by plants and pollinators constitutes the backbone of plant-pollinator community stability and functioning. However, rapid human-induced environmental changes are compromising the long-term persistence of plant-pollinator interaction networks. One of the major challenges for pollinator conservation is the lack of robust generalisable data capturing how plant-pollinator communities are structured across space and time. Here, we present the EuPPollNet (European Plant-Pollinator Networks) database, a fully open and reproducible European-level database containing harmonized taxonomic data on plant-pollinator interactions referenced in both space and time, along with other ecological variables of interest. This database offers an open workflow that allows researchers to track data-curation decisions and edit them according to their preferences. We present the taxonomic and sampling coverage of EuPPollNet, and summarize key structural properties in plant-pollinator networks. We hope EuPPollNet will stimulate future research that fills the taxonomic, ecological, and geographical data gaps on plant-pollinator interactions that we have identified. Further, the variation in the structure of the networks in EuPPollNet provides a strong basis for future studies aimed at quantifying drivers of plant-pollinator network change and guiding future conservation planning for plants and pollinators.

- **Main Types of Variables Included**: EuPPollNet contains 1,162,913 interactions between plants and pollinators from 1,864 distinct networks (i.e., distinct sampling event in space or time), which belong to 54 different studies distributed across 23 European countries. In addition, information about sampling methodology, habitat type, bio-climatic region, and further taxonomic rank information for both plant and pollinator species are also provided (i.e., genus, family and order).

- **Spatial location and grain**: The database contains 1,214 different sampling locations from 13 different natural and anthropogenic habitats that fall in 7 different bio-climatic regions. All records are geo-referenced and presented in the World Geodetic System 1984 (WGS84).

- **Time period and grain**: Species interaction data was collected between 2004 and 2021. All records are time-referenced and most of the studies documented interactions within a single flowering season (68.52%).

- **Major taxa and level of measurement**: The database contains interaction data at the species level for 94.39% of the records, including a total of 1,411 plant and 2,223 pollinator species. The database covers 5.56% of the European species of flowering plants, 34.38% of bees, 26.21% of butterflies, and 33.63% of syrphid species at the European level.

- **Software format**: The database was built with the R programming language and is stored as “.rds” and “.csv” formats. The construction of the database is fully reproducible and can be accessed in this repository.

### Usage

The [interaction data](Data/3_Final_data/Interaction_data.rds) and [flower count data](Data/3_Final_data/Flower_counts.rds) data can be found in the folder `Data/3_Final_data/` and both datasets can be merged by the 'Flower_data_merger' column. Raw networks can be accessed at `Data/1_Raw_data/` and all code to produce this work is located in the `Scripts/` folder. To run the scripts, it is advised to use `renv::restore()` to restore the libraries to their required versions. The `renv.lock` file contains specific information about the version of each library.


### Citation guidelines

If you use this database in your research, please make sure to cite this data paper.





