# Manage project dependencies ------
# the following will prompt you to install the various packages used in the study
# install.packages("renv")
# renv::activate()
renv::restore()

# packages ---
library(Capr)
library(here)
library(DBI)
library(CDMConnector)
library(dplyr)
library(tidyr)
library(CodelistGenerator)
library(ggplot2)
library(CohortConstructor)

# db with vocab ----
server_dbi <- Sys.getenv("DB_SERVER_cdm_thin_fr_202308_dbi")
user       <- Sys.getenv("DB_USER")
password   <- Sys.getenv("DB_PASSWORD")
port       <- Sys.getenv("DB_PORT")
host       <- Sys.getenv("DB_HOST")

# connect
db <- DBI::dbConnect(RPostgres::Postgres(),
                     dbname = server_dbi,
                     port = port,
                     host = host,
                     user = user,
                     password = password)

# Set database details -----
# The name of the schema that contains the OMOP CDM with patient-level data
cdm_database_schema <- "public"

# The name of the schema that contains the vocabularies
# (often this will be the same as cdm_database_schema)
vocabulary_database_schema <- cdm_database_schema

# The name of the schema where results tables will be created
results_database_schema <- "results"

# database metadata and connection details -----
# The name/ acronym for the database
db_name<-"THIN_fr"

# Name of outcome table in the result table where the outcome cohorts will be stored
# Note, if there is an existing table in your results schema with the same names
# it will be overwritten
table_stem <- "eocrc"

# create cdm reference ----
cdm <- CDMConnector::cdm_from_con(con = db,
                                  cdm_schema = cdm_database_schema,
                                  write_schema = c("schema" = results_database_schema,
                                                   "prefix" = table_stem),
                                  cdm_name = db_name,
                                  achilles_schema = results_database_schema )


# check patient numbers
cdm$person %>%
  tally()

# check vocab version
# getVocabVersion(cdm = cdm)

getConceptClassId(cdm,
                  standardConcept = "Standard")

# H pylori cohorts

## OCAM + OCA

drug_codes <- getDrugIngredientCodes(cdm,
                                     name = c("amoxicillin", "clarithromycin", "metronidazole"))
drug_codes

cdm$ocam <- conceptCohort(cdm = cdm,
                                 conceptSet = drug_codes,
                                 name = "ocam")
settings(cdm$ocam)

cdm$intersection_ocam <- cdm$ocam %>%
  CohortConstructor::intersectCohorts(
    gap = 0,
    returnOnlyComb = FALSE,
    name = "intersection_ocam"
  )

settings (cdm$intersection_ocam)

## PYLERA

drug_codes2 <- getDrugIngredientCodes(cdm,
                                      name = c("bismuth subcitrate", "metronidazole", "tetracycline"))
drug_codes2

cdm$pylera <- conceptCohort(cdm = cdm,
                            conceptSet = drug_codes2,
                            name = "pylera")

cdm$intersection_pylera <- cdm$pylera %>%
  CohortConstructor::intersectCohorts(
    gap = 0,
    returnOnlyComb = TRUE,
    name = "intersection_pylera"
  )

settings (cdm$intersection_pylera)

## H PYLORI DIAGNOSTIC

cdm$hpylori_condition <- xxx
