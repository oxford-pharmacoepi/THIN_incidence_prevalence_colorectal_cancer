# Incidence, Prevalence and Characterisation of Colorectal Cancer across Europe
<img src="https://img.shields.io/badge/Study%20Status-Started-blue.svg" alt="Study Status: Started">

- **Study title**: Incidence, Prevalence and Characterisation of Colorectal Cancer across Europe
- **Study start date**: 24/05/24
- **Study leads**: Patri Pedregal Pascual (patri.pedregalpascual@ndorms.ox.ac.uk)
- **Study end date**:
- **PhenotypeR ShinyApp 1**: https://dpa-pde-oxford.shinyapps.io/eocrc_PhenotypeRShiny/
- **Study Shiny**: https://dpa-pde-oxford.shinyapps.io/THIN_incidence_prevalence_colorectal_cancer/
- **Publications**:

---
## Repository organisation

This repo is organized as follows:
- [1_Diagnostics](https://github.com/oxford-pharmacoepi/THIN_incidence_prevalence_cancers/tree/main/1_Diagnostics): Please find the code for developement of cancer definitions and cohortdiagnostics.
- [2_Study](https://github.com/oxford-pharmacoepi/THIN_incidence_prevalence_cancers/tree/main/2_Study): Please find there the relevant code to obtain the study results.
- [3_Reporting](https://github.com/oxford-pharmacoepi/THIN_incidence_prevalence_cancers/tree/main/3_Reporting): Please find there the code to visualise the results with the shiny app and generate the report with the plots and tables.

## Download the repository
1) Download this entire repository (you can download as a zip folder using Code -> Download ZIP, or you can use GitHub Desktop). 

## Running the cohort diagnostics using PhenotypeR
1) Navigate to [PhenotypeR](https://github.com/oxford-pharmacoepi/THIN_incidence_prevalence_colorectal_cancer/tree/main/1_Diagnostics/PhenotypeR) in the 1_Diagnostics folder. Open the project <i>PhenotypeR.Rproj</i> in RStudio (when inside the project, you will see its name on the top-right of your RStudio session) in 1_Diagnostics/PhenotypeR
2) Open and work though the <i>CodeToRun.R</i> file which should be the only file that you need to interact with. Run the lines in the file, adding your database specific information and so on (see comments within the file for instructions). The last line of this file will run the study <i>(source(here("RunStudy.R"))</i>.     
3) After running you should then have a zip folder with results to share in your output folder.

## Running the main study
1) Open the folder 2_Study and then the project <i>THINCancerIncidencePrevalence.Rproj</i> in RStudio (when inside the project, you will see its name on the top-right of your RStudio session)
2) Open and work though the <i>CodeToRun.R</i> file which should be the only file that you need to interact with. Run the lines in the file, adding your database specific information and so on (see comments within the file for instructions). The last line of this file will run the study <i>(source(here("RunStudy.R"))</i>.     
3) After running you should then have a zip folder with results to share in your output folder.

## Running the reporting shiny
1) Open the 3_Reporting folder and open the project <i>Shiny.Rproj</i>
2) Place the results from the main study into the data file of the shiny
3) Run app and your results will be available to review locally
