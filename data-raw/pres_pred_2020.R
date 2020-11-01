# data processing for generating pres_pred_2020
#
# Author: mjskay
###############################################################################

# raw US Presidential electoral vote probabilities from 538 as of Oct 27, 2020
# This source CSV is copyright FiveThirtyEight under CC BY 4.0
# Source file from: https://projects.fivethirtyeight.com/2020-general-data/presidential_ev_probabilities_2020.csv
# Metadata described at: https://github.com/fivethirtyeight/data/tree/master/election-forecasts-2020
# Interactive visualizations at: https://projects.fivethirtyeight.com/2020-election-forecast/
# License: https://creativecommons.org/licenses/by/4.0/
raw = read.csv("data-raw/pres_pred_2020.csv", stringsAsFactors = FALSE)

# convert timestamp column
raw$modeldate = as.Date(raw$modeldate, format = "%m/%d/%Y")
raw$timestamp = as.POSIXct(raw$timestamp, format = "%H:%M:%S %d %b %Y", tz = "EST")

# drop the 3rd candidate columns since they aren't used
raw$candidate_3rd = NULL
raw$evprob_3rd = NULL

# sort final data frame
pres_pred_2020 = raw[order(raw$total_ev),]

usethis::use_data(pres_pred_2020, overwrite = TRUE, compress = 'xz')
