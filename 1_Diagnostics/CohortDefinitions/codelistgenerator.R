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

# [1] "Clinical Finding"  "Context-dependent" "HCPCS Modifier"    "ICDO Condition"
# [5] "Procedure"

# Colorectal ---------------
colorectalcancer_codes <- getCandidateCodes(
  cdm = cdm,
  keywords = c("malignant tumor of colon",
               "malignant tumor of rectum",
               "malignant neoplasm of colon",
               "malignant neoplasm of rectum") ,
  exclude = c("melanoma",
              "lymphoma",
              "sarcoma",
              "secondary",
              "metastasis",
              "lymphocytic",
              "benign",
              "hodgkin",
              "neuroendocrine",
              "rhabdomyosarcoma",
              "angiomyosarcoma",
              "fibrosarcoma",
              "leiomyosarcoma",
              "hemangiosarcoma",
              "pseudosarcomatous",
              "carcinosarcoma",
              "leukemia",
              "blastoma",
              "T-cell",
              "atelectasis",
              "plasmacytoma",
              "mesenchymoma",
              "heavy chain disease" ,
              "ectomesenchymoma",
              "myeloproliferative",
              "sezary",
              "lymphoid",
              "epithelioid hemangioendothelioma"

  ) ,
  domains = c("Condition", "Observation")
)

write.csv(colorectalcancer_codes, here::here("preliminary_cohorts" , "codelists",
                                             paste0(cdmName(cdm), "_colorectalCancerBroad.csv")), row.names = FALSE)

#trying out orphan codes for colorectal cancer
colorectalcancer_orphan_codes <- findOrphanCodes(x = list("colorectal_cancer" = colorectalcancer_codes$concept_id),
                                                 cdm = cdm,
                                                 domains = c("Condition", "Observation"),
                                                 standardConcept = "Standard",
                                                 searchInSynonyms = FALSE,
                                                 searchNonStandard = FALSE,
                                                 includeDescendants = TRUE,
                                                 includeAncestor = TRUE)


colorectalcancer_orphan_codes <- colorectalcancer_orphan_codes %>%
  separate(additional_level, into = c("Description", "Concept ID"), sep = " ; ")

write.csv(colorectalcancer_orphan_codes, here::here("preliminary_cohorts" , "codelists" ,
                                                    paste0(cdmName(cdm), "_colorectalCancer_orphan.csv")), row.names = FALSE)


# Creating cohort files ------------

# read in reviewed list of codelists
reviewed_code_list_colorectal_cancer <- read.csv2(here::here("preliminary_cohorts" ,"reviewed_codelists" ,
                                                            paste0(cdmName(cdm), "_colorectalCancerBroad_reviewed_ppp.csv")))

colorectal_cancer <- reviewed_code_list_colorectal_cancer %>%
  filter(include == "y" &
           domain_id == "Condition"
  ) %>%
  pull(concept_id)

colon_cancer <- reviewed_code_list_colorectal_cancer %>%
  filter(colon == "y" &
           domain_id == "Condition"
  ) %>%
  pull(concept_id)

rectal_cancer <- reviewed_code_list_colorectal_cancer %>%
  filter(rectum == "y" &
           domain_id == "Condition"
  ) %>%
  pull(concept_id)

# create cohorts
colorectal_cancer <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(colorectal_cancer, name = "colorectal_cancer"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)

writeCohort(colorectal_cancer, here::here("preliminary_cohorts", "jsons" , "colorectal_cancer.json"))


colon_cancer <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(colon_cancer, name = "colon_cancer"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)

writeCohort(colon_cancer, here::here("preliminary_cohorts", "jsons" , "colon_cancer.json"))


rectal_cancer <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(rectal_cancer, name = "rectal_cancer"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)

writeCohort(rectal_cancer, here::here("preliminary_cohorts", "jsons" , "rectal_cancer.json"))


# prevalence
colorectal_cancer_prev <- reviewed_code_list_colorectal_cancer %>%
  filter(include_prev == "y" &
           domain_id == "Condition"
  ) %>%
  pull(concept_id)

colon_cancer_prev <- reviewed_code_list_colorectal_cancer %>%
  filter(colon_prev == "y" &
           domain_id == "Condition"
  ) %>%
  pull(concept_id)

rectal_cancer_prev <- reviewed_code_list_colorectal_cancer %>%
  filter(rectum_prev == "y" &
           domain_id == "Condition"
  ) %>%
  pull(concept_id)


# colorectal
total_prev_colorectal_cancer <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(colorectal_cancer_prev, name = "total_prev_colorectal_cancer"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)

writeCohort(total_prev_colorectal_cancer, here::here("preliminary_cohorts", "jsons",
                                                     "colorectal_cancer_end.json"))


# prev 5 year
partial_prev5y_colorectal_cancer <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(colorectal_cancer_prev, name = "colorectal_cancer_cancer_5y"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(fixedExit("startDate", 1825L)
  )
)

writeCohort(partial_prev5y_colorectal_cancer, here::here("preliminary_cohorts", "jsons",
                                                         "colorectal_cancer_5y.json"))

# prev 10 year
partial_prev10y_colorectal_cancer <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(colorectal_cancer_prev, name = "colorectal_cancer_10y"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(fixedExit("startDate", 3650L)
  )
)

writeCohort(partial_prev10y_colorectal_cancer, here::here("preliminary_cohorts", "jsons",
                                                          "colorectal_cancer_10y.json"))


# colon
total_prev_colon_cancer <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(colon_cancer_prev, name = "total_prev_colon_cancer"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)

writeCohort(total_prev_colon_cancer, here::here("preliminary_cohorts", "jsons",
                                                "colon_cancer_end.json"))


# prev 5 year
partial_prev5y_colon_cancer <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(colon_cancer_prev, name = "colon_cancer_cancer_5y"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(fixedExit("startDate", 1825L)
  )
)

writeCohort(partial_prev5y_colon_cancer, here::here("preliminary_cohorts", "jsons",
                                                    "colon_cancer_5y.json"))

# prev 10 year
partial_prev10y_colon_cancer <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(colon_cancer_prev, name = "colon_cancer_10y"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(fixedExit("startDate", 3650L)
  )
)

writeCohort(partial_prev10y_colon_cancer, here::here("preliminary_cohorts", "jsons",
                                                     "colon_cancer_10y.json"))


# rectal
total_prev_rectal_cancer <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(rectal_cancer_prev, name = "total_prev_rectal_cancer"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)

writeCohort(total_prev_rectal_cancer, here::here("preliminary_cohorts", "jsons",
                                                 "rectal_cancer_end.json"))


# prev 5 year
partial_prev5y_rectal_cancer <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(rectal_cancer_prev, name = "rectal_cancer_cancer_5y"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(fixedExit("startDate", 1825L)
  )
)

writeCohort(partial_prev5y_rectal_cancer, here::here("preliminary_cohorts", "jsons",
                                                     "rectal_cancer_5y.json"))

# prev 10 year
partial_prev10y_rectal_cancer <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(rectal_cancer_prev, name = "rectal_cancer_10y"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(fixedExit("startDate", 3650L)
  )
)

writeCohort(partial_prev10y_rectal_cancer, here::here("preliminary_cohorts", "jsons",
                                                      "rectal_cancer_10y.json"))

