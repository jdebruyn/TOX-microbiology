# TOX-microbiology

This project contains metadata associated with a field study conducted between February 2018 - August 2021, in which we begin to address questions about inter-individual variation in vertebrate decomposition attributed to intrinsic factors, that is, properties of the carcass or cadaver itself. Additionally, this project contains code that is associated with the publication: Mason et al. (2022). Body Mass Index (BMI) impacts soil chemical and microbial response to human.

This folder contains the following items:

  TOX_16S_RMD_pub.Rmd - R markdown containing all code for 16S data analysis. This includes r code for relative abundance, alpha diversity, and beta diverisy. This file will use the main metadata file (TOX_data_all_pub.xlsx) and sequencing metadatafile (treatments_pub.xlsx).

  TOX_16S_alphadiv.xlsx - data file contaning chao1, richness, and inverse simpson estimates from the code found in the TOX_16S_RMD_pub.Rmd file. This data is also used in the TOX_stats_pub.Rmd for hierarchical linear mixed effects models.

  TOX_ITS_RMD_pub.Rmd - R markdown containing all code for ITS data analysis. This includes r code for relative abundance, alpha diversity, and beta diverisy. This file will use the main metadata file (TOX_data_all_pub.xlsx) and sequencing metadatafile (treatments_pub.xlsx).
  
  TOX_ITS_alphadiv.xlsx - data file contaning chao1, richness, and inverse simpson estimates from the code found in the TOX_ITS_RMD_pub.Rmd file. This data is also used in the TOX_stats_pub.Rmd for hierarchical linear mixed effects models.
  
  TOX_data_all_pub.xlsx - Main metadata file for all soil samples collected in the field study. This includes antemortem metadata, sample collection information, and soil biogeochemical data. A full list of variables can be found in ReadMe sheet of the excel workbook.

TOX_stats_pub.Rmd - R markdown containing all code for statistical analyses conducted with our data. This includes subject demographics and analyses for soil biogeochemical variables and both 16S and ITS Chao1 and inverse simposon. 

treatments_pub.xlsx - Main metadata file descibing all DNA sequencing samples. This file is used as the base metadata file for importing data into phyloseq found in both the TOX_16S_RMD_pub.Rmd and TOX_ITS_RMD_pub.Rmd r-markdown files. A full list of variables in this file can be found in the 'Varibale guide' sheet of the excel workbook.
