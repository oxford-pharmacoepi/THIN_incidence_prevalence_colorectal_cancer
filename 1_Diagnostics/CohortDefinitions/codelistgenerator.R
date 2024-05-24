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
reviewed_code_list_colorectal_cancer <- read.csv(here::here("preliminary_cohorts" ,"reviewed_codelists" ,
                                                            paste0(cdmName(cdm), "_colorectalCancerBroad_reviewed.csv")))

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


# Broad colorectal cancer incidence
broad_inc <- reviewed_code_list %>%
  filter(broad_inc == "y") %>%
  pull(concept_id)

# narrow colorectal cancer incidence
narrow_inc <- reviewed_code_list %>%
  filter(narrow_inc == "y") %>%
  pull(concept_id)

# Broad colorectal cancer prevalence
broad_prev <- reviewed_code_list %>%
  filter(Broad_prev == "y") %>%
  pull(concept_id)

# narrow lung cancer prevalence
narrow_prev <- reviewed_code_list %>%
  filter(narrow_prev == "y") %>%
  pull(concept_id)

# small cell lung cancer
sclc <- reviewed_code_list %>%
  filter(SCLC == "y") %>%
  pull(concept_id)

# create cohorts
# 1 broad incidence
lung_cancer_incident_broad <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(broad_inc, name = "lung_cancer_broad_inc"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)

writeCohort(lung_cancer_incident_broad, here::here("preliminary_cohorts",
                                                     "lung_cancer_incident_broad.json"))

# 2 narrow incidence
lung_cancer_incident_narrow <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(narrow_inc, name = "lung_cancer_narrow_inc"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)

writeCohort(lung_cancer_incident_narrow, here::here("preliminary_cohorts",
                                                   "lung_cancer_incident_narrow.json"))


# 3 sclc incidence
small_cell_lung_cancer <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(sclc, name = "small_cell_lung_cancer"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)

writeCohort(small_cell_lung_cancer, here::here("preliminary_cohorts",
                                                    "small_cell_lung_cancer.json"))


# 4 broad prev end
total_prev_lung_cancer_broad <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(broad_prev, name = "broad_lung_cancer_end"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)

writeCohort(total_prev_lung_cancer_broad, here::here("preliminary_cohorts",
                                                     "broad_lung_cancer_end.json"))


# 5 broad prev 2 year
partial_prev2y_lung_cancer_broad <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(broad_prev, name = "broad_lung_cancer_2y"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(fixedExit("startDate", 720L)
  )
)

writeCohort(partial_prev2y_lung_cancer_broad, here::here("preliminary_cohorts",
                                                         "broad_lung_cancer_2y.json"))

# 6 broad prev 5 year
partial_prev5y_lung_cancer_broad <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(broad_prev, name = "broad_lung_cancer_5y"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(fixedExit("startDate", 1825L)
  )
)

writeCohort(partial_prev5y_lung_cancer_broad, here::here("preliminary_cohorts",
                                                         "broad_lung_cancer_5y.json"))

# 7 narrow prev end
total_prev_lung_cancer_narrow <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(narrow_prev, name = "narrow_lung_cancer_end"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)

writeCohort(total_prev_lung_cancer_narrow, here::here("preliminary_cohorts",
                                                     "narrow_lung_cancer_end.json"))


# 8 narrow prev 2 year
partial_prev2y_lung_cancer_narrow <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(narrow_prev, name = "narrow_lung_cancer_2y"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(fixedExit("startDate", 720L)
  )
)

writeCohort(partial_prev2y_lung_cancer_narrow, here::here("preliminary_cohorts",
                                                         "narrow_lung_cancer_2y.json"))

# 9 narrow prev 5 year
partial_prev5y_lung_cancer_narrow <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(narrow_prev, name = "narrow_lung_cancer_5y"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(fixedExit("startDate", 1825L)
  )
)

writeCohort(partial_prev5y_lung_cancer_narrow, here::here("preliminary_cohorts",
                                                         "narrow_lung_cancer_5y.json"))

# 10 sclc prev 2 year
partial_prev2y_sclc <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(sclc, name = "sclc_2y"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(fixedExit("startDate", 720L)
  )
)

writeCohort(partial_prev2y_sclc, here::here("preliminary_cohorts",
                                                          "small_cell_lung_cancer_2y.json"))


# 11 sclc prev 5 year
partial_prev5y_sclc <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(sclc, name = "sclc_5y"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(fixedExit("startDate", 1825L)
  )
)

writeCohort(partial_prev5y_sclc, here::here("preliminary_cohorts",
                                                          "small_cell_lung_cancer_5y.json"))



# lung cancer plus stage ------
# lung cancer condition OR measurement - unable to get nested to work so will take the codes below and create in ATLAS

# broad stage 1
# lung_cancer_incident_broad_stage1 <- cohort(
#   entry = entry(
#     conditionOccurrence(getConceptSetDetails(cs(broad_inc, name = "lung_cancer_broad_stage_1"), db, vocabularyDatabaseSchema = "public")),
#
#     measurement(getConceptSetDetails(cs(stage1_codes$concept_id , name = "stage1"), db, vocabularyDatabaseSchema = "public")),
#     observationWindow = continuousObservation(0L, 0L),
#     primaryCriteriaLimit = "First"
#   ),
#   exit = exit(
#     endStrategy = observationExit()
#   )
# )
#
# writeCohort(lung_cancer_incident_broad_stage1, here::here("preliminary_cohorts",
#                                                    "lung_cancer_incident_broad_s1.json"))
#
#
# # broad stage 2
# lung_cancer_incident_broad_stage2 <- cohort(
#   entry = entry(
#     conditionOccurrence(getConceptSetDetails(cs(broad_inc, name = "lung_cancer_broad_stage_2"), db, vocabularyDatabaseSchema = "public")),
#     measurement(getConceptSetDetails(cs(stage2_codes$concept_id , name = "stage2"), db, vocabularyDatabaseSchema = "public")),
#     observationWindow = continuousObservation(0L, 0L),
#     primaryCriteriaLimit = "First"
#   ),
#   exit = exit(
#     endStrategy = observationExit()
#   )
# )
#
# writeCohort(lung_cancer_incident_broad_stage2, here::here("preliminary_cohorts",
#                                                           "lung_cancer_incident_broad_s2.json"))
#
#
#
# # broad stage 3
# lung_cancer_incident_broad_stage3 <- cohort(
#   entry = entry(
#     conditionOccurrence(getConceptSetDetails(cs(broad_inc, name = "lung_cancer_broad_stage_3"), db, vocabularyDatabaseSchema = "public")),
#     measurement(getConceptSetDetails(cs(stage3_codes$concept_id , name = "stage3"), db, vocabularyDatabaseSchema = "public")),
#     observationWindow = continuousObservation(0L, 0L),
#     primaryCriteriaLimit = "First"
#   ),
#   exit = exit(
#     endStrategy = observationExit()
#   )
# )
#
# writeCohort(lung_cancer_incident_broad_stage3, here::here("preliminary_cohorts",
#                                                           "lung_cancer_incident_broad_s3.json"))
#
#
#
# # broad stage 4
# lung_cancer_incident_broad_stage4 <- cohort(
#   entry = entry(
#     conditionOccurrence(getConceptSetDetails(cs(broad_inc, name = "lung_cancer_broad_stage_4"), db, vocabularyDatabaseSchema = "public")),
#     measurement(getConceptSetDetails(cs(stage4_codes$concept_id , name = "stage4"), db, vocabularyDatabaseSchema = "public")),
#     observationWindow = continuousObservation(0L, 0L),
#     primaryCriteriaLimit = "First"
#   ),
#   exit = exit(
#     endStrategy = observationExit()
#   )
# )
#
# writeCohort(lung_cancer_incident_broad_stage4, here::here("preliminary_cohorts",
#                                                           "lung_cancer_incident_broad_s4.json"))
#

# smoking status
reviewed_code_list_smoking <- read.csv(here::here("preliminary_cohorts" , "reviewed" ,
                                          paste0(cdmName(cdm), "_smokingBroad_reviewed.csv")))


# smoker
smoker_obs <- reviewed_code_list_smoking %>%
  filter(Smoker == "y" &
           domain_id == "Observation" ) %>%
  pull(concept_id)

smoker_cond <- reviewed_code_list_smoking %>%
  filter(Smoker == "y" &
           domain_id == "Condition" ) %>%
  pull(concept_id)

# previous smoker
pre_smoker_obs <- reviewed_code_list_smoking %>%
  filter(Previous.smoker == "y" &
           domain_id == "Observation" ) %>%
  pull(concept_id)

pre_smoker_cond <- reviewed_code_list_smoking %>%
  filter(Previous.smoker == "y" &
           domain_id == "Condition" ) %>%
  pull(concept_id)

#non smoker
non_smoker_obs <- reviewed_code_list_smoking %>%
  filter(non.smoker == "y" &
           domain_id == "Observation" ) %>%
  pull(concept_id)

non_smoker_cond <- reviewed_code_list_smoking %>%
  filter(non.smoker == "y" &
           domain_id == "Condition" ) %>%
  pull(concept_id)


# smoker
smoker <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(smoker_cond, name = "smoker_cond"), db, vocabularyDatabaseSchema = "public")),
    observation(getConceptSetDetails(cs(smoker_obs , name = "smoker_obs"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)

writeCohort(smoker, here::here("preliminary_cohorts", "smoker.json"))


# former smoker
prev_smoker <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(pre_smoker_cond, name = "prev_smoker_cond"), db, vocabularyDatabaseSchema = "public")),
    observation(getConceptSetDetails(cs(pre_smoker_obs , name = "prev_smoker_obs"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)

writeCohort(prev_smoker, here::here("preliminary_cohorts", "prev_smoker.json"))

# non smoker
non_smoker <- cohort(
  entry = entry(
    conditionOccurrence(getConceptSetDetails(cs(non_smoker_cond, name = "non_smoker_cond"), db, vocabularyDatabaseSchema = "public")),
    observation(getConceptSetDetails(cs(non_smoker_obs , name = "non_smoker_obs"), db, vocabularyDatabaseSchema = "public")),
    observationWindow = continuousObservation(0L, 0L),
    primaryCriteriaLimit = "First"
  ),
  exit = exit(
    endStrategy = observationExit()
  )
)

writeCohort(non_smoker, here::here("preliminary_cohorts", "non_smoker.json"))
