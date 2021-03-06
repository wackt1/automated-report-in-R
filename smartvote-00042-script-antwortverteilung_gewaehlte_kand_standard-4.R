# ==============================================================================
# smartvote-00042-script-antwortverteilung_gewaehlte_kand_standard-4.R
#
# Skript zur Visualisierung der Antwortverteilung gewaehlte Kandidierende - Fragetyp Standard-4
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
library(viridis)

# ------------------------------------------------------------------------------
# CONSTANTS
# ------------------------------------------------------------------------------
config_file = yaml.load_file("_parameter.yml")

PARAM_JSON_URL_CANDIDATES <- config_file$param_JSON_URL$JSON_URL_CANDIDATES_DATA
PARAM_JSON_URL_QUESTIONS <- config_file$param_JSON_URL$JSON_URL_QUESTIONS_DATA
PARAM_DISTRICT <- config_file$params_wahlkreis$WAHLKREIS

# ------------------------------------------------------------------------------
# FUNCTIONS
# ------------------------------------------------------------------------------
wrapper <- function(x, ...) 
{
    paste(strwrap(x, ...), collapse = "\n")
}

# ------------------------------------------------------------------------------
# MAIN
# ------------------------------------------------------------------------------

# --- read input
smartvotedata_json_candidates <- jsonlite::fromJSON(PARAM_JSON_URL_CANDIDATES)
smartvotedata_json_questions <- jsonlite::fromJSON(PARAM_JSON_URL_QUESTIONS)

# --- preprocess
if(PARAM_DISTRICT != "NA") {
    smartvotedata_json_candidates <- filter(smartvotedata_json_candidates, district == PARAM_DISTRICT)
}

# --- analyze
data_unnested <- unnest(smartvotedata_json_candidates, answers)

data_unnested_questions <- select(data_unnested, "questionId")
data_unnested_questions %<>%
    count(questionId) %>%
    select(-n) %>%
    as_tibble()

data_unnested_filtered <- select(data_unnested, "party_short", "elected", "questionId", "answer")
data_unnested_filtered %<>%
    filter(questionId == 3392, elected == TRUE) %>%
    count(answer, party_short, elected) %>%
    group_by(party_short) %>%
    as_tibble()

smartvotedata_json_questions %<>%
    filter(ID_question == 3392) %>%
    as_tibble()

data_unnested_filtered$answer[data_unnested_filtered$answer == 25] <- 33
data_unnested_filtered$answer[data_unnested_filtered$answer == 75] <- 67

question_title <- smartvotedata_json_questions$question

question_chart_vis_party <- ggplot(data = data_unnested_filtered, aes(x = answer, y = reorder(party_short, answer), fill = n)) + 
    geom_tile(color = "white") +
    scale_fill_viridis(name = "Anzahl\nKandidierende") +
    scale_x_continuous(breaks = round(seq(min(data_unnested_filtered$answer), max(100), by = 33.33), 0)) +
    xlab(paste0("Antworten\n\nSkala:   0 = \"Nein\"   -   33 = \"Eher Nein\"   -   67 = \"Eher Ja\"   -   100 = \"Ja\"")) +
    ylab("Partei") +
    ggtitle(wrapper(paste0("Antwortverteilung zur Frage (pro Partei - alle Kandidierenden): ", question_title))) +
    theme_bw()

# --- visualize
question_chart_vis_party

# ==============================================================================
# END
# ==============================================================================

