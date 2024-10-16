# TOX-microbiology

This repository contains files and code associated with the Human Decomposition/Toxicology project and the following publications: 

  1. Mason, A. R., McKee-Zech, H. S., Hoeland, K. M., Davis, M. C., Campagna, S. R., Steadman, D. W., and DeBruyn, J. M. (2022) Body Mass Index (BMI) Impacts Soil Chemical and Microbial Response to Human Decomposition, mSphere 7, e00325-00322. [DOI: doi:10.1128/msphere.00325-22](https://doi.org/10.1128/msphere.00325-22)
  2. Mason AR, McKee-Zech HS, Steadman DW, DeBruyn JM (2024) Environmental predictors impact microbial-based postmortem interval (PMI) estimation models within human decomposition soils. PLoS ONE 19(10): e0311906. https://doi.org/10.1371/journal.pone.0311906
    
    Data and files associated with publication 1 are in the Mason_SoilMicrobialEcology-Decomp-BMI_mSphere_2022 dir, which contains the following files:

      TOX_16S_RMD_pub.Rmd - R markdown containing all code for 16S data analysis. This includes r code for relative abundance, alpha diversity, and beta diverisy. This file will use the main metadata file (TOX_data_all_pub.xlsx) and sequencing metadatafile (treatments_pub.xlsx).

      TOX_16S_alphadiv.xlsx - data file contaning chao1, richness, and inverse simpson estimates from the code found in the TOX_16S_RMD_pub.Rmd file. This data is also used in the TOX_stats_pub.Rmd for hierarchical linear mixed effects models.

      TOX_ITS_RMD_pub.Rmd - R markdown containing all code for ITS data analysis. This includes r code for relative abundance, alpha diversity, and beta diverisy. This file will use the main metadata file (TOX_data_all_pub.xlsx) and sequencing metadatafile (treatments_pub.xlsx).
  
      TOX_ITS_alphadiv.xlsx - data file contaning chao1, richness, and inverse simpson estimates from the code found in the TOX_ITS_RMD_pub.Rmd file. This data is also used in the TOX_stats_pub.Rmd for hierarchical linear mixed effects models.
  
      TOX_data_all_pub.xlsx - Main metadata file for all soil samples collected in the field study. This includes antemortem metadata, sample collection information, and soil biogeochemical data. A full list of variables can be found in ReadMe sheet of the excel workbook.

      TOX_stats_pub.Rmd - R markdown containing all code for statistical analyses conducted with our data. This includes subject demographics and analyses for soil biogeochemical variables and both 16S and ITS Chao1 and inverse simposon. 

      treatments_pub.xlsx - Main metadata file descibing all DNA sequencing samples. This file is used as the base metadata file for importing data into phyloseq found in both the TOX_16S_RMD_pub.Rmd and TOX_ITS_RMD_pub.Rmd r-markdown files. A full list of variables in this file can be found in the 'Varibale guide' sheet of the excel workbook.

