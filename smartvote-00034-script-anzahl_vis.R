# ==============================================================================
# smartvote-00034-script-anzahl_vis.R
#
# Skript zur Visualisierung der smartvote-Teilnahme Kandidierende und Gewaehlte
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
library(ggpubr)
library(scales)
library(ggpubr)

# ------------------------------------------------------------------------------
# CONSTANTS
# ------------------------------------------------------------------------------
config_file = yaml.load_file("_parameter.yml")

PARAM_JSON_URL_CANDIDATES <- config_file$param_JSON_URL$JSON_URL_CANDIDATES_DATA
PARAM_DISTRICT <- config_file$params_wahlkreis$WAHLKREIS

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
data_subset_all <- select(smartvotedata_json_candidates, "n_answers")
data_subset_all %<>%
    mutate(confirmed = (data_subset_all$n_answers == 75)) %>%
    as_tibble(data_subset_all) %>%
    count(confirmed) %>%
    mutate(confirmed_label = confirmed) %>%
    mutate(confirmed = ifelse(confirmed, 'Ja', 'Nein')) %>%
    mutate(percentage = n / sum(n) *100) %>%
    mutate(percentage = round(percentage, digits = 2)) %>%
    mutate(label_text_1 = paste(n, "(")) %>%
    mutate(label_text_2 = paste(label_text_1, percentage, "%", ")", sep="")) %>%
    rename(Bestaetigt = confirmed)

data_subset_elected <- select(smartvotedata_json_candidates, "elected", "n_answers")
data_subset_elected %<>%
    mutate(confirmed = (data_subset_elected$n_answers == 75)) %>%
    filter(elected == TRUE) %>%
    as_tibble(data_subset_elected) %>%
    count(confirmed) %>%
    mutate(confirmed_label = confirmed) %>%
    mutate(confirmed = ifelse(confirmed, 'Ja', 'Nein')) %>%
    mutate(percentage = n / sum(n) * 100) %>%
    mutate(percentage = round(percentage, digits = 2)) %>%
    mutate(label_text_1 = paste(n, "(")) %>%
    mutate(label_text_2 = paste(label_text_1, percentage, "%", ")", sep="")) %>%
    rename(Bestaetigt = confirmed)

ymid <- 0.5

colors_chart_2 <- c("#132b43", "#56b1f7")
colors_chart_1 <- c("#56b1f7")

# --- visualize
plot_all_vis <- ggplot(data_subset_all, aes(x = n, y = reorder(Bestaetigt, confirmed_label), label = percentage)) +
    geom_col(width = 0.6, position = position_dodge(width = 1.6),
             fill = if(nrow(data_subset_all) == 2) {
                 colors_chart_2
             } else {
                 colors_chart_1
             },
             alpha = 0.8) +
    geom_text(aes(label = label_text_2), hjust = ifelse(data_subset_all$confirmed_label < ymid, -0.1, 1.1), size = 5) +
    ggtitle("smartvote-Teilnahme aller Kandidierenden") +
    xlab("Anzahl Kandidierende") + 
    ylab("Bestaetigt") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))

plot_elected_vis <- ggplot(data_subset_elected, aes(x = n, y = reorder(Bestaetigt, confirmed_label), label = percentage)) +
    geom_col(width = 0.6, position = position_dodge(width = 1.6),
             fill = if(nrow(data_subset_elected) == 2) {
                 colors_chart_2
             } else {
                 colors_chart_1
             },
             alpha = 0.8) +
    geom_text(aes(label = label_text_2), hjust = ifelse(data_subset_elected$confirmed_label < ymid, -0.1, 1.1), size = 5) +
    ggtitle("smartvote-Teilnahme der gewaehlten Kandidierenden") +
    xlab("Anzahl Kandidierende") + 
    ylab("Bestaetigt") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggarrange(plot_all_vis, plot_elected_vis, ncol = 1, nrow = 2)

# ==============================================================================
# END
# ==============================================================================

