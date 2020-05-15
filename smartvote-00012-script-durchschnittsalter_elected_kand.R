# ==============================================================================
# smartvote-00012-script-durchschnittsalter_elected_kand.R
#
# Skript zur Berechnung des Durschnittalters der gewaehlten Kandidierenden
# 
# R-Script - Bachelor Thesis - smartvote
#
# Author: wackt1.bfh@gmail.com
# Date: 22.05.2020
# ==============================================================================

# ------------------------------------------------------------------------------
# IMPORT PACKAGES
# ------------------------------------------------------------------------------
library(jsonlite)
library(tidyverse)
library(ggplot2)
library(yaml)

# ------------------------------------------------------------------------------
# CONSTANTS
# ------------------------------------------------------------------------------
config_file = yaml.load_file("_parameter.yml")

PARAM_JSON_URL_CANDIDATES <- config_file$param_JSON_URL$JSON_URL_CANDIDATES_DATA
PARAM_DISTRICT <- config_file$params_wahlkreis$WAHLKREIS
YEAR_OF_ELECTION = 2019

# ------------------------------------------------------------------------------
# MAIN
# ------------------------------------------------------------------------------

# --- read input
smartvotedata_json_candidates <- jsonlite::fromJSON(PARAM_JSON_URL_CANDIDATES)

# --- preprocess
if(PARAM_DISTRICT != "NA") {
    smartvotedata_json_candidates <- filter(smartvotedata_json_candidates, district == PARAM_DISTRICT)
}

# --- analyze
yob_cand_elected_data <- select(smartvotedata_json_candidates, "elected", "year_of_birth")
yob_cand_elected_data <- filter(yob_cand_elected_data, elected == 1)
yob_cand_elected_data <- select(yob_cand_elected_data, -c(elected))
yob_cand_elected_data <- yob_cand_elected_data$year_of_birth

mean_yob_cand_elected <- mean(yob_cand_elected_data)

mean_age_cand_elected <- round(YEAR_OF_ELECTION - mean_yob_cand_elected, digits = 2)

# --- visualize
mean_age_cand_elected

# ==============================================================================
# END
# ==============================================================================

