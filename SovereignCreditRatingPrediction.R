## ----Download-BibTex-References, warning = FALSE, echo = FALSE--------------------------------------------------------------------------------------
dir.create("Untitled_files")
download.file("https://raw.githubusercontent.com/georgeaholt/Capstone/main/SovereignCreditRatingPredictionreferences.bib",
              "Untitled_files/reprex.bib")


## ----Install-Required-Packages, echo = FALSE, include = FALSE---------------------------------------------------------------------------------------
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(purrr)) install.packages("purrr", repos = "http://cran.us.r-project.org")
if(!require(matrixStats)) install.packages("matrixStats", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(WDI)) install.packages("WDI", repos = "http://cran.us.r-project.org")
if(!require(IMFData)) install.packages("IMFData", repos = "http://cran.us.r-project.org")
if(!require(imfr)) install.packages("imfr", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(MASS)) install.packages("MASS", repos = "http://cran.us.r-project.org")
if(!require(ordinal)) install.packages("ordinal", repos = "http://cran.us.r-project.org")
if(!require(ordinalForest)) install.packages("ordinalForest", repos = "http://cran.us.r-project.org")
if(!require(psych)) install.packages("psych", repos = "http://cran.us.r-project.org")
if(!require(pander)) install.packages("pander", repos = "http://cran.us.r-project.org")
if(!require(jsonlite)) install.packages("jsonlite", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(httr)) install.packages("httr", repos = "http://cran.us.r-project.org")
if(!require(vtreat)) install.packages("vtreat", repos = "http://cran.us.r-project.org")
if(!require(vcd)) install.packages("vcd", repos = "http://cran.us.r-project.org")
if(!require(ModelMetrics)) install.packages("ModelMetrics", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(cowplot)) install.packages("cowplot", repos = "http://cran.us.r-project.org")
if(!require(broom)) install.packages("broon", repos = "http://cran.us.r-project.org")
if(!require(irr)) install.packages("irr", repos = "http://cran.us.r-project.org")
if(!require(ggmosaic)) install.packages("ggmosaic", repos = "http://cran.us.r-project.org")


## ----Load-Libraries, echo = FALSE, include = FALSE--------------------------------------------------------------------------------------------------
# Load required libraries
library(tidyverse)
library(caret)
library(data.table)
library(stringr)
library(lubridate)
library(purrr)
library(matrixStats)
library(e1071)
library(knitr)
library(rvest)
library(dplyr, warn.conflicts = FALSE)
library(kableExtra)
library(WDI)
library(IMFData)
library(imfr)
library(RColorBrewer)
library(MASS)
library(ordinal)
library(ordinalForest)
library(psych)
library(pander)
library(jsonlite)
library(readr)
library(httr)
library(vtreat)
library(VGAM)
library(RCurl)
library(vcd)
library(ModelMetrics)
library(gridExtra)
library(cowplot)
library(broom)
library(irr)
library(ggmosaic)

## ---- Set-Options, include = FALSE------------------------------------------------------------------------------------------------------------------
options(tinytex.verbose = TRUE)
options(dplyr.summarise.inform = FALSE)

## ----Define-Chunk-Timer, echo = FALSE---------------------------------------------------------------------------------------------------------------
knitr::knit_hooks$set(time_it = local({
  now <- NULL
  function(before, options) {
    if (before) {
      # record the current time before each chunk
      now <<- Sys.time()
    } else {
      # calculate the time difference after a chunk
      res <- difftime(Sys.time(), now)
      # return a character string to show the time
      paste("Time for this code chunk to run:", res)
    }
  }
}))


## ----Set-Up-ISO-Country-Codes, message = FALSE, echo = FALSE----------------------------------------------------------------------------------------
# Set up sovereign iso codes
country_codes_url <- "https://raw.githubusercontent.com/georgeaholt/Capstone/main/Country%20Codes.csv"
iso_country_codes <- read.csv(country_codes_url)
colnames(iso_country_codes) <- c("country", "iso2_code")

## ----Download-Website-Ratings-HTML, echo = FALSE----------------------------------------------------------------------------------------------------
# Download the ratings page HTML from the website
country_economics <- "https://countryeconomy.com"
country_economics_ratings <- "https://countryeconomy.com/ratings"
country_economics_html <- read_html(country_economics_ratings)

## ----Access-Country-Listing-Table, echo = FALSE-----------------------------------------------------------------------------------------------------
# Access the country listing table in the webpage
table_body <- country_economics_html %>% html_node("table") %>% html_node("tbody")

## ----Access-Country-Listing-Table-Rows, echo = FALSE------------------------------------------------------------------------------------------------
# Access the rows in the country listing table
table_rows_html <- table_body %>% html_nodes("tr")

## ----Initialize-Country-Links, echo = FALSE---------------------------------------------------------------------------------------------------------
# Initialize a list of relative urls for all countries
country_links <- tibble(country = character(), rel_url = character())

## ----Construct-Country-Links-List, warnings = FALSE, echo = FALSE-----------------------------------------------------------------------------------
# Iterate over the rows in the country list to construct a list of country links for all countries 
# that contains the country name and the relative urls for each country. Print the number of 
# countries and the first 10 country links.
for (row_value in table_rows_html) {
  row_string <- as.character(row_value)
  relative_url <- str_sub(str_extract(row_string, "/.+\""), 1, -2)
  country_name <- sub(" \\[", "", sub("\">", "", str_extract(row_string, "\">.+\\[")))
  country_links <- rbind(country_links, c(country_name, relative_url))
}
num_countries <- dim(country_links)[1]
country_relative_urls <- as_tibble(country_links)
colnames(country_relative_urls) <- c("country", "relurl")

## ----Extract-Long-Term-Rating-Descriptions, warnings = FALSE, echo = FALSE--------------------------------------------------------------------------
# The following code extracts the long-term credit rating descriptions for the credit rating
# agencies. This information is contained on each country webpage, so only the first country 
# webpage needs to be retrieved. 
# Get the country url for the first country and download the html code for the country webpage
country_url <- paste(country_economics,country_links[1, 2], sep = "")
country_html <- read_html(country_url)
# Find the rating agency tables in the country ratings webpage
country_tables <- country_html %>% html_nodes("table")
# Get the rating category descriptions for the rating agencies
long_term_rating_description_rows <- country_tables[4] %>% html_nodes("tbody") %>% html_nodes("tr")
# Set up an empty credit rating description tibble
credit_rating_descriptions <- tibble(Grade = character(),
                                     Moodys = character(),
                                     SandP = character(),
                                     Fitch = character())
# Iterate over the rows in the rating description table
for (description in 1:length(long_term_rating_description_rows)) {
  # Determine whether the "rowspan" attribute value applies to the row
  rowspan <- str_detect(as.character(long_term_rating_description_rows[description]), "rowspan=\".+\" ")
  # Determine whether the "class" attribute value applies to the row
  classrow <- str_detect(as.character(long_term_rating_description_rows[description]), "class=\"wborder")
  # Extracting rating descriptions for each row in the table
  long_term_rating_description_row_data_nodes <- long_term_rating_description_rows[description] %>% html_nodes("td")
  # Extract the rating items from the table row
  rating_items <- character()
  # If the row is a "rowspan" or "class row, it will contain 4 items
  if (rowspan | classrow) {
    for (item in 1:length(long_term_rating_description_row_data_nodes)) {
      rating_item <- sub("<", "", sub(">", "", 
                                      str_extract(as.character(long_term_rating_description_row_data_nodes[item]), ">.*<")))
      rating_items <- append(rating_items, rating_item)
    } 
    # Otherwise, the row will start with an empty cell followed by 3 items
  } else {
    rating_item <- " "
    rating_items <- append(rating_items, rating_item)
    for (item in 1:length(long_term_rating_description_row_data_nodes)) {
      rating_item <- sub("<", "", sub(">", "", 
                                      str_extract(as.character(long_term_rating_description_row_data_nodes[item]), ">.*<")))
      rating_items <- append(rating_items, rating_item)
    }
  }
  row_tibble <- tibble(Grade = rating_items[1], Moodys = rating_items[2], SandP = rating_items[3], Fitch = rating_items[4])
  # Append the items to the credit rating description tibble
  credit_rating_descriptions <- rbind(credit_rating_descriptions, row_tibble)
}

## ----Display-Credit-Rating-Descriptions, echo = FALSE-----------------------------------------------------------------------------------------------
#credit_rating_descriptions <- readRDS("CreditRatingDescriptions.rds")
credit_rating_descriptions <- credit_rating_descriptions %>% 
  mutate(Grade = ifelse(str_detect(Grade, "br>"), sub("<", "", sub("br>", "", Grade)), Grade))
knitr::kable(credit_rating_descriptions, caption = "Long-Term Credit Rating Categories", format = "latex", booktabs = TRUE)

## ----Define-Agency-Rating-Order, echo = FALSE-------------------------------------------------------------------------------------------------------
# Define the credit rating category order for each rating agency
moodys_rating_list <- credit_rating_descriptions$Moodys[!(credit_rating_descriptions$Moodys == "")]
moodys_rating_categories <- factor(moodys_rating_list, levels = moodys_rating_list, ordered = TRUE)
moodys_rating_order <- 1:length(moodys_rating_categories)
names(moodys_rating_order) <- moodys_rating_categories
sandp_rating_list <- credit_rating_descriptions$SandP[!(credit_rating_descriptions$SandP == "")]
sandp_rating_categories <- factor(sandp_rating_list, levels = sandp_rating_list, ordered = TRUE)
sandp_rating_order <- 1:length(sandp_rating_categories)
names(sandp_rating_order) <- sandp_rating_categories
fitch_rating_list <- credit_rating_descriptions$Fitch[!(credit_rating_descriptions$Fitch == "")]
fitch_rating_categories <- factor(fitch_rating_list, levels = fitch_rating_list, ordered = TRUE)
fitch_rating_order <- 1:length(fitch_rating_categories)
names(fitch_rating_order) <- fitch_rating_categories


## ----Define-Credit_Rating_Agency-Labels, echo = FALSE-----------------------------------------------------------------------------------------------
# Define labels for the three credit rating agencies
agencies <- c('Moodys','SandP','Fitch')

## ----Create-Current-Ratings-Tibble, echo = FALSE----------------------------------------------------------------------------------------------------
# Create an empty current long-term credit ratings tibble
current_sovereign_ratings <- tibble(country = character(), moodys = character(), sandp = character(), fitch = character())
colnames(current_sovereign_ratings) <- c("country", "moodys", "sandp", "fitch")

## ----Extract-Current-Country-Ratings, echo = FALSE--------------------------------------------------------------------------------------------------
# Get html text for all countries
country_html <- table_rows_html %>% 
  html_nodes("td")
# Iterate over the rows in the country list to extract the current country long-term credit ratings
for (i in 1: length(country_html)) {   
  if ((i %% 4) == 1) {
    country_label <- sub("\\[", "", sub("\">", "", str_extract(as.character(country_html[i]), "\">.+\\[")))
    }
  else
  if ((i %% 4) == 2) {
    agency_rating_1 <- sub(" </", "", sub("left\"> ", "", str_extract(as.character(country_html[i]), "left\">.+</")))
    }
  else
  if ((i %% 4) == 3) {
    agency_rating_2 <- sub(" </", "", sub("left\"> ", "", str_extract(as.character(country_html[i]), "left\">.+</")))
    }
  else
  if ((i %% 4) == 0) {
    agency_rating_3 <- sub(" </", "", sub("left\"> ", "", str_extract(as.character(country_html[i]), "left\">.+</")))
    current_sovereign_ratings <- rbind(current_sovereign_ratings, c(country_label, agency_rating_1, agency_rating_2, agency_rating_3))
    colnames(current_sovereign_ratings) <- c("country", "Moodys", "SandP", "Fitch")
  }
}


## ----Create-List-of-Rated-Sovereigns, echo = FALSE--------------------------------------------------------------------------------------------------
# Create list of rated sovereigns and display the list
current_sovereign_ratings_countries_A <- current_sovereign_ratings$country[1:48]
current_sovereign_ratings_countries_B <- current_sovereign_ratings$country[49:96]
current_sovereign_ratings_countries_C <- current_sovereign_ratings$country[97:144]
current_sovereign_ratings_countries_tibble <- tibble(Sovereigns = current_sovereign_ratings_countries_A, 
                                                     Sovereigns. = current_sovereign_ratings_countries_B, 
                                                     Sovereigns.. = current_sovereign_ratings_countries_C)
knitr::kable(current_sovereign_ratings_countries_tibble, caption = "Rated Sovereigns", format = "latex", booktabs = TRUE) %>% 
      kable_styling(latex_options = c("striped", "HOLD_position"), font_size = 8)


## ----Display-Number-Of-Ratings-by-Rating-Category-Moodys, fig.cap = "**Number of Sovereigns by Moody's Rating Category**", echo = FALSE-------------
# Display number of ratings by rating category for Moodys in a figure
moodys_current_ratings <- current_sovereign_ratings$Moodys[!is.na(current_sovereign_ratings$Moodys)]
moodys_current_ratings_table <- table(moodys_current_ratings)
moodys_current_ratings_categories <- names(moodys_current_ratings_table)
moodys_current_ratings_index <- moodys_rating_order[names(moodys_current_ratings_table)]
moodys_current_ratings_counts <- as.integer(moodys_current_ratings_table)
moodys_current_ratings_tibble <- tibble(rating_categories = moodys_current_ratings_categories, 
                                       rating_index = moodys_current_ratings_index, 
                                       rating_count = moodys_current_ratings_counts) %>% 
                                    arrange(rating_index)
moodys_current_ratings_tibble %>% 
  arrange(rating_index) %>% 
  ggplot(aes(reorder(rating_categories, rating_index), rating_count)) +
    geom_col(width = 0.5, fill="lightblue") +
    xlab("Moodys Rating Categpry") +
    ylab("Number of Sovereigns") +
    ylim(0, 15) +
    scale_fill_brewer() +
    theme(axis.text.x = element_text(angle = 45, hjust=1))


## ----Display-Number-Of-Ratings-by-Rating-Category-SandP, fig.cap = "**Number of Sovereigns by Standard & Poors Rating Category**", echo = FALSE-----
# Display number of ratings by rating category for S and P in a figure
sandp_current_ratings <- current_sovereign_ratings$SandP[!is.na(current_sovereign_ratings$SandP)]
sandp_current_ratings_table <- table(sandp_current_ratings)
sandp_current_ratings_categories <- names(sandp_current_ratings_table)
sandp_current_ratings_index <- sandp_rating_order[names(sandp_current_ratings_table)]
sandp_current_ratings_counts <- as.integer(sandp_current_ratings_table)
sandp_current_ratings_tibble <- tibble(rating_categories = sandp_current_ratings_categories, 
                                       rating_index = sandp_current_ratings_index, 
                                       rating_count = sandp_current_ratings_counts) %>% 
                                    arrange(rating_index)
sandp_current_ratings_tibble %>% 
  arrange(rating_index) %>% 
  ggplot(aes(reorder(rating_categories, rating_index), rating_count)) +
    geom_col(width = 0.5, fill="lightblue") +
    xlab("Standard & Poors Rating Categpry") +
    ylab("Number of Sovereigns") +
    ylim(0, 15) +
    scale_fill_brewer() +
    theme(axis.text.x = element_text(angle = 45, hjust=1))


## ----Display-Number-Of-Ratings-by-Rating-Category-Fitch, fig.cap = "**Number of Sovereigns by Fitch Rating Category**", echo = FALSE----------------
# Display number of ratings by rating category for Fitch in a figure
fitch_current_ratings <- current_sovereign_ratings$Fitch[!is.na(current_sovereign_ratings$Fitch)]
fitch_current_ratings_table <- table(fitch_current_ratings)
fitch_current_ratings_categories <- names(fitch_current_ratings_table)
fitch_current_ratings_index <- fitch_rating_order[names(fitch_current_ratings_table)]
fitch_current_ratings_counts <- as.integer(fitch_current_ratings_table)
fitch_current_ratings_tibble <- tibble(rating_categories = fitch_current_ratings_categories, 
                                       rating_index = fitch_current_ratings_index, 
                                       rating_count = fitch_current_ratings_counts) %>% 
                                    arrange(rating_index)
fitch_current_ratings_tibble %>% 
  arrange(rating_index) %>% 
  ggplot(aes(reorder(rating_categories, rating_index), rating_count)) +
    geom_col(width = 0.5, fill="lightblue") +
    xlab("Fitch Rating Categpry") +
    ylab("Number of Sovereigns") +
    ylim(0, 15) +
    scale_fill_brewer() +
    theme(axis.text.x = element_text(angle = 45, hjust=1))


## ----Initialize-Credit-Rating-Actions-Tibble, echo = FALSE------------------------------------------------------------------------------------------
# An empty credit rating actions history tibble is created to retain the historical credit
# rating actions. This tibble has columns for the country, rating agency, and the rating 
# actions. The rating actions columns cover long-term and short-term credit ratings for 
# foreign currency and local currency credit rating actions, as well as the date and action 
# label for each action (for a total of 8 columns).
credit_rating_actions_tibble <- tibble(
  long_term_foreign_currency_rating_action_dates = character(),
  long_term_foreign_currency_rating_action_values = character(),
  long_term_local_currency_rating_action_dates = character(),
  long_term_local_currency_rating_action_values = character(),
  short_term__term_foreign_currency_rating_action_dates = character(),
  short_term_foreign_currency_rating_action_values = character(),
  short_term_local_currency_rating_action_dates = character(),
  short_term_local_currency_rating_action_values = character())
colnames(credit_rating_actions_tibble) <- c("Country",
                                            "Agency",
                                            "LTFC_Date",
                                            "LTFC_Action",
                                            "LTLC_Date",
                                            "LTLC_Action",
                                            "STFC_Date",
                                            "STFC_Action",
                                            "STLC_Date",
                                            "STLC_Action")



## ----Iteration-Over-Countries-Agencies-Ratings, warning = FALSE, echo = FALSE-----------------------------------------------------------------------
# The following code extracts historical credit rating actions for all countries by the 
# three credit rating agencies. The processing steps are as follows:
#   * Create country credit rating web page urls for all countries
#   * Extract country webpage url and download the html code
#   * Find the table nodes for the agencies in the webpage
#   * Find the table body for each rating agency table
#   * Find the table rows for each table body
#   * Convert table row html to a list of rating action values
#   * Update the credit rating actions history tibble with the rating action values
# Create country web page urls for all countries
country_economics_country_rating_urls <- country_relative_urls %>% 
  mutate(country_url = paste(country_economics, relurl, sep = ""))
# Iterate over all country webpage urls
for (i in 1:dim(country_economics_country_rating_urls[1]))  {
# Get the country url and download the html code for the country webpage
  country_url <- country_economics_country_rating_urls$country_url[i]
  country_html <- read_html(country_url)
# Find the rating agency tables in the country ratings webpage
  country_tables <- country_html %>% html_nodes("table")
# Start by iterating over the rating agency tables in the web page for the country
  for (j in 1:3) {
# Extracting ratings for successive rating agencies
    agency_name <- agencies[j]
    agency_table <- country_tables[j] %>% html_nodes("tbody")
# Get table rows for the credit rating agency    
    agency_table_rows <- agency_table %>% html_nodes("tr")
# Extracting ratings for each credit rating agency
    for (k in 1:length(agency_table_rows)) {
# Convert html to character string with "|" in empty columns
      agency_table_row_string <- gsub("<td></td>", "<td>|</td>", as.character(agency_table_rows[k]))
# Extract the rating data from the table row
      agency_table_row_values <- sub("</td>", "", sub("<td>", "", str_extract_all(agency_table_row_string, "<td>.+</td>", simplify = TRUE)))
# Extract the rating actions from the rating data
      long_term_foreign_currency_rating_action_dates <- 
        ifelse(agency_table_row_values[c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)] == "|", " ", 
               agency_table_row_values[c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)])
      long_term_foreign_currency_rating_action_values <- agency_table_row_values[c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)]
      long_term_local_currency_rating_action_dates <- 
        ifelse(agency_table_row_values[c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)] == "|", " ", 
               agency_table_row_values[c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)])
      long_term_local_currency_rating_action_values <- agency_table_row_values[c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)]
      short_term__term_foreign_currency_rating_action_dates <- 
        ifelse(agency_table_row_values[c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)] == "|", " ", 
               agency_table_row_values[c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)])
      short_term_foreign_currency_rating_action_values <- agency_table_row_values[c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)]
      short_term_local_currency_rating_action_dates <- 
        ifelse(agency_table_row_values[c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE)] == "|", " ", 
               agency_table_row_values[c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE)])
      short_term_local_currency_rating_action_values <- agency_table_row_values[c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)]
      rating_actions <- tibble(country_name, agency_name, 
                                      long_term_foreign_currency_rating_action_dates, 
                                      long_term_foreign_currency_rating_action_values,
                                      long_term_local_currency_rating_action_dates,
                                      long_term_local_currency_rating_action_values,
                                      short_term__term_foreign_currency_rating_action_dates,
                                      short_term_foreign_currency_rating_action_values,
                                      short_term_local_currency_rating_action_dates,
                                      short_term_local_currency_rating_action_values
                                      )
      colnames(rating_actions) <- c("Country",
                                      "Agency",
                                      "LTFC_Date",
                                      "LTFC_Action",
                                      "LTLC_Date",
                                      "LTLC_Action",
                                      "STFC_Date",
                                      "STFC_Action",
                                      "STLC_Date",
                                      "STLC_Action")

# Append the rating actions to the credit rating actions tibble
      credit_rating_actions_tibble <- rbind(credit_rating_actions_tibble, rating_actions)
    }
  }
}
credit_rating_actions <- credit_rating_actions_tibble

## ----Load-Credit-Rating-Actions, warning = FALSE, message = FALSE, echo = FALSE---------------------------------------------------------------------
credit_rating_actions_url <- "https://raw.githubusercontent.com/georgeaholt/Capstone/main/CreditRatingActions.csv"
credit_rating_actions <- read_csv(credit_rating_actions_url)



## ----Select-Long-Term-Credit-Rating-Actions, echo = FALSE-------------------------------------------------------------------------------------------
# Extract only long-term foreign currency actions
LTFC_credit_rating_actions <- credit_rating_actions[,1:4]


## ----Extract-Credit-Rating-Actions-Separating-Qualifiers-Droping-NAs, echo = FALSE------------------------------------------------------------------
# Extract credit rating actions
LTFC_credit_rating_actions_cleaned <- LTFC_credit_rating_actions %>% 
  filter(!is.na(LTFC_Date) & !is.na(LTFC_Action)) %>% 
  mutate(Rating_Date = LTFC_Date,
         Rating_Category = sub(" \\(", "", str_extract(LTFC_Action, ".* \\(")),
         Qualifier = str_extract(LTFC_Action, "\\(.*\\)")) %>% 
  filter(!is.na(Rating_Category))


## ----Display-Rating-Actions-Over-Time, fig.cap = "**Credit Rating Actions by Year**", echo = FALSE--------------------------------------------------
# Display credit rating actions over timr ina figure
LTFC_credit_rating_actions_cleaned %>% 
  mutate(Rating_Year = substr(Rating_Date,1 , 4)) %>% 
  group_by(Agency, Rating_Year) %>% 
  summarize(num_actions = n(), .groups = "keep") %>% 
  ggplot(aes(x = Rating_Year, num_actions, fill = Agency)) +
    geom_bar(stat="identity", position = position_dodge()) +
    xlab("Rating Year") +
    ylab("Number of Sovereign Rating Actions") +
    scale_fill_brewer(palette = "Paired") +
    theme(axis.text.x = element_text(angle=90, hjust=1))


## ----Define-Rating-Ranking-Function, echo = FALSE---------------------------------------------------------------------------------------------------
# Define rating ranking for Moodys
credit_rating_descriptions_moodys <- credit_rating_descriptions$Moodys[credit_rating_descriptions$Moodys != ""]
moodys_rating_ranking <- 1:length(credit_rating_descriptions_moodys)
names(moodys_rating_ranking) <- credit_rating_descriptions_moodys
# Define rating ranking for SandP
credit_rating_descriptions_sandp <- credit_rating_descriptions$SandP[credit_rating_descriptions$SandP != ""]
sandp_rating_ranking <- 1:length(credit_rating_descriptions_sandp)
names(sandp_rating_ranking) <- credit_rating_descriptions_sandp
# Define rating ranking for Fitch
credit_rating_descriptions_fitch <- credit_rating_descriptions$Fitch[credit_rating_descriptions$Fitch != ""]
fitch_rating_ranking <- 1:length(credit_rating_descriptions_fitch)
names(fitch_rating_ranking) <- credit_rating_descriptions_fitch

## ----Define-Credit-Rating-Ranking-Function, echo = FALSE--------------------------------------------------------------------------------------------
get_rating_ranking <- function(agency_name, rating_category) {
  if (agency_name == "Moodys") {
    return(moodys_rating_ranking[rating_category])
  } else if (agency_name == "SandP") {
    return(sandp_rating_ranking[rating_category])
  } else if (agency_name == "Fitch") {
    return(fitch_rating_ranking[rating_category])
  } 
}

## ----Define-Get-Credit-Rating-Category, echo = FALSE------------------------------------------------------------------------------------------------
# Define function to get rating category for rating ranking
get_rating_category <- function(agency_name, rating_ranking) {
  if (agency_name == "Moodys") {
    return(names(moodys_rating_ranking)[rating_ranking])
  } else if (agency_name == "SandP") {
    return(names(sandp_rating_ranking)[rating_ranking])
  } else if (agency_name == "Fitch") {
    return(names(fitch_rating_ranking)[rating_ranking])
  } 
}


## ----Iteratively-Generate-Historical-Credit-Ratings, warning = FALSE, message = FALSE, echo = FALSE, tidy = TRUE------------------------------------
# Set up historical credit ratings tibble
historical_credit_ratings <- tibble(country_name = character(),
                                    agency_name = character(),
                                    year = character(),
                                    rating = character())
# Set initial and final years for credit ratings
initial_rating_year <- 1999
final_rating_year <- 2020
# Get the list of all rated sovereigns
countries <- unique(LTFC_credit_rating_actions_cleaned$Country)
# Iterate over the countries
for (country_name in countries) {
  # Iterate over the rating agencies
  for (agency_name in agencies) {
    # Set up the annual credit ratings by year vector
    credit_rating_by_year <- initial_rating_year:final_rating_year * NA
    names(credit_rating_by_year) <- initial_rating_year:final_rating_year
    # Set up the year-ending credit ratings by year vector
    year_ending_credit_rating_by_year <- initial_rating_year:final_rating_year * NA
    names(year_ending_credit_rating_by_year) <- initial_rating_year:final_rating_year
    # Set up the year-starting credit ratings by year vector
    year_starting_credit_rating_by_year <- initial_rating_year:final_rating_year * NA
    names(year_starting_credit_rating_by_year) <- initial_rating_year:final_rating_year
    # Get the long-term credit rating actions for a country and rating agency
    actions_country <- LTFC_credit_rating_actions_cleaned %>% 
      filter(Country == country_name)
    actions_agency <- actions_country %>% 
      filter(Agency == agency_name)
    actions <- actions_agency %>% 
      dplyr::select(-c(LTFC_Date, LTFC_Action, Qualifier))
    # Check weather there are credit rating actions for the country and rating agency
    # If not, there will be no annual credit ratings for the country and rating agency
    if (nrow(actions) > 0) {
      # get the average, first, and last credit rating action ranks for each year
      credit_rating_actions_ranking <- actions %>% 
        mutate(action_year = substr(Rating_Date, 1, 4),
               Rating_Ranking = get_rating_ranking(agency_name, Rating_Category)) %>% 
        filter(action_year >= 1994) %>% 
        group_by(action_year) %>% 
        summarize(first_rating_action_rank = min(Rating_Ranking),
                  last_rating_action_rank = max(Rating_Ranking),
                  average_rating_action_rank = round(mean(Rating_Ranking), digits = 0))
      # Get rating categories corresponding to average rating action rankings as annual credit ratings
      annual_credit_ratings <- credit_rating_actions_ranking %>% 
        mutate(annual_rating = get_rating_category(agency_name, average_rating_action_rank))
      # Get rating categories corresponding to last rating action rankings as year-end credit ratings
      year_ending_credit_ratings <- credit_rating_actions_ranking %>% 
        mutate(year_ending_rating = get_rating_category(agency_name, last_rating_action_rank))
      # Get rating categories corresponding to first rating action rankings as year-starting credit ratings
      year_starting_credit_ratings <- credit_rating_actions_ranking %>% 
        mutate(year_starting_rating = get_rating_category(agency_name, first_rating_action_rank))
      # Iterate over the annual credit ratings to produce credit ratings per year
      for (k in 1:length(annual_credit_ratings$action_year)) {
        year <- annual_credit_ratings$action_year[k]
        credit_rating_by_year[year] <- annual_credit_ratings$annual_rating[k]
      }
      # Iterate over the year-end credit ratings to produce year-end ratings per year
      for (k in 1:length(year_ending_credit_ratings$action_year)) {
        year <- year_ending_credit_ratings$action_year[k]
        year_ending_credit_rating_by_year[year] <- year_ending_credit_ratings$year_ending_rating[k]
      }
      # Iterate over the year-starting credit ratings to produce year-starting ratings per year
      for (k in 1:length(year_starting_credit_ratings$action_year)) {
        year <- year_starting_credit_ratings$action_year[k]
        year_starting_credit_rating_by_year[year] <- year_starting_credit_ratings$year_starting_rating[k]
      }
      
    # Find the index of the first rating in the ratings by year list
      first_rating_index <- min(which(!is.na(credit_rating_by_year) == TRUE))
      # Set the ratings for the previous years to the year starting rating category for the first rating index
      for (m in 1:first_rating_index - 1) {
        credit_rating_by_year[m] <- year_starting_credit_rating_by_year[first_rating_index]
      }
      # Iterate through years subsequent to the first rating year
      # Set the ratings for the following years where no rating actions were issued
      for (m in (first_rating_index + 1):length(credit_rating_by_year)) {
        # Check for no rating value for each subsequent year and 
        if (is.na(credit_rating_by_year[m])) {
          # If no rating, assign year-ending credit rating for previous year
          credit_rating_by_year[m] <- year_ending_credit_rating_by_year[m - 1]
          year_ending_credit_rating_by_year[m] <- year_ending_credit_rating_by_year[m - 1]
        }
      }
      # Create a credit ratings tibble for the country and rating agency
      credit_ratings_tibble <- tibble(country = country_name, 
                                      agency = agency_name, 
                                      year = names(credit_rating_by_year), 
                                      rating = as.vector(credit_rating_by_year))
      historical_credit_ratings <- rbind(historical_credit_ratings, credit_ratings_tibble)
    }
  }
}


## ----Create-Sovereigns-With-Historical-Ratings-List, echo = FALSE-----------------------------------------------------------------------------------
# Create sovereigns with historical ratings list
sovereigns_with_historical_ratings <- unique(historical_credit_ratings["country"])
sovereigns_with_historical_ratings_list <- sovereigns_with_historical_ratings %>% 
  left_join(iso_country_codes, by = "country") %>% 
  filter(!is.na(iso2_code))

## ----Create-List-of-Selected-Sovereigns, echo = FALSE-----------------------------------------------------------------------------------------------
# Define selected sovereigns
selected_sovereigns <- c("Brazil", "China", "India", "Italy", "Malaysia", "Mexico", "Spain", "South Africa", "Turkey", "United States")


## ----Get-GNI-Per-Capita-Data, fig.cap = "**GNI Per Capita by Year**", message = FALSE, warning = FALSE, echo = FALSE--------------------------------
gni_per_capita_url <- "https://raw.githubusercontent.com/georgeaholt/Capstone/main/GNI%20Per%20Capita.csv"
gni_per_capita_temp <- read_csv(gni_per_capita_url)
gni_per_capita <- gni_per_capita_temp %>% 
  dplyr::select(-c("Series Name", "Series Code")) %>% 
  pivot_longer(!c("Country Name", "Country Code"), names_to = "yearval", values_to = "per_capita_gni_str") %>% 
  filter(!(per_capita_gni_str == "..")) %>% 
  mutate(year = as.numeric(substr(yearval, 1, 4)),
         GNI_per_capita = as.numeric(per_capita_gni_str)) %>% 
  dplyr::select(-c(yearval, per_capita_gni_str))
colnames(gni_per_capita) <- c("country", "country_code", "year", "per_capita_gni")
gni_per_capita %>% 
  filter(country %in% selected_sovereigns) %>% 
  ggplot(aes(year, per_capita_gni, color = country)) + 
    geom_line(lwd = 1) +
    xlab("Year") +
    ylab("GNI Per Capita") +
    scale_colour_brewer("Sovereigns", palette="Paired")


## ----Get-GNI-Growth-Data, fig.cap = "**GNI Growth by Year**", message = FALSE, warning = FALSE, echo = FALSE----------------------------------------
# Get GNI growth data and display it in a figure
gni_growth_url <- "https://raw.githubusercontent.com/georgeaholt/Capstone/main/GNI%20Growth.csv"
gni_growth_temp <- read_csv(gni_growth_url)
gni_growth <- gni_growth_temp %>% 
  dplyr::select(-c("Series Name", "Series Code")) %>% 
  pivot_longer(!c("Country Name", "Country Code"), names_to = "yearval", values_to = "growth_gni_str") %>% 
  filter(!(growth_gni_str == "..")) %>% 
  mutate(year = as.numeric(substr(yearval, 1, 4)),
         GNI_growth = as.numeric(growth_gni_str)) %>% 
  dplyr::select(-c(yearval, growth_gni_str))
colnames(gni_growth) <- c("country", "country_code", "year", "growth_gni")
gni_growth %>% 
  filter(country %in% selected_sovereigns) %>% 
  ggplot(aes(year, growth_gni, color = country)) + 
    geom_line(lwd = 1) +
    xlab("Year") +
    ylab("GNI Growth Rate (%)") +
    scale_colour_brewer("Sovereigns", palette="Paired")



## ----Get-CPI-Inflation-Data, fig.cap = "**CPI Inflation Rate by Year**", message = FALSE, warning = FALSE, echo = FALSE-----------------------------
# Get CPI inflation data and display it ina figure
infllation_cpi_url <- "https://raw.githubusercontent.com/georgeaholt/Capstone/main/CPI%20Inflation.csv"
inflation_cpi_temp <- read_csv(infllation_cpi_url)
inflation_cpi <- inflation_cpi_temp %>% 
  dplyr::select(-c("Series Name", "Series Code")) %>% 
  pivot_longer(!c("Country Name", "Country Code"), names_to = "yearval", values_to = "inflation_cpi_str") %>% 
  filter(!(inflation_cpi_str == "..")) %>% 
  mutate(year = as.numeric(substr(yearval, 1, 4)),
         CPI_inflation = as.numeric(inflation_cpi_str)) %>% 
  dplyr::select(-c(yearval, inflation_cpi_str))
colnames(inflation_cpi) <- c("country", "country_code", "year", "cpi_inflation")
inflation_cpi %>% 
  filter(country %in% selected_sovereigns) %>% 
  ggplot(aes(year, cpi_inflation, color = country)) + 
    geom_line(lwd = 1) +
    xlab("Year") +
    ylab("CPI Inflation Rate (%)") +
    scale_colour_brewer("Sovereigns", palette="Paired")



## ----Get-Current-Account-Balance-Data, fig.cap = "**Current Account Balance by Year**", message = FALSE, warning = FALSE, echo = FALSE--------------
# Get current account data and display it in a figure
current_account_balance_url <- "https://raw.githubusercontent.com/georgeaholt/Capstone/main/Current%20Account%20Balance.csv"
current_account_balance_temp <- read_csv(current_account_balance_url)
current_account_balance <- current_account_balance_temp %>% 
  dplyr::select(-c("Series Name", "Series Code")) %>% 
  pivot_longer(!c("Country Name", "Country Code"), names_to = "yearval", values_to = "current_account_balance_str") %>% 
  filter(!(current_account_balance_str == "..")) %>% 
  mutate(year = as.numeric(substr(yearval, 1, 4)),
         balance_current_account = as.numeric(current_account_balance_str)) %>% 
  dplyr::select(-c(yearval, current_account_balance_str))
colnames(current_account_balance) <- c("country", "country_code", "year", "current_account_ratio")
current_account_balance %>% 
  filter(country %in% selected_sovereigns) %>% 
  ggplot(aes(year, current_account_ratio, color = country)) + 
    geom_line(lwd = 1) +
    xlab("Year") +
    ylab("Current Account Balance (% of GNI)") +
    scale_colour_brewer("Sovereigns", palette="Paired")



## ----Get-External-Debt-Data, fig.cap = "**External Debt Ratio by Year**", message = FALSE, warning = FALSE, echo = FALSE----------------------------
# Get external debt data and display it in a figure
external_debt_url <- "https://raw.githubusercontent.com/georgeaholt/Capstone/main/External%20Debt.csv"
external_debt_temp <- read_csv(external_debt_url)
external_debt_temp <- external_debt_temp %>% 
  rename(Country_Name = "Country Name", Country_Code = "Country Code")
external_debt <- external_debt_temp %>% 
  dplyr::select(-c("Series Name", "Series Code")) %>% 
  pivot_longer(!c(Country_Name, Country_Code), names_to = "yearval", values_to = "external_debt_str") %>% 
  filter(!(external_debt_str == "..")) %>% 
  mutate(year = as.numeric(substr(yearval, 2, 5)),
         debt_external = as.numeric(external_debt_str)) %>% 
  dplyr::select(-c(yearval, external_debt_str))
colnames(external_debt) <- c("country", "country_code", "year", "external_debt_ratio")
external_debt %>% 
  filter(country %in% selected_sovereigns) %>% 
  ggplot(aes(year, external_debt_ratio, color = country)) + 
    geom_line(lwd = 1) +
    xlab("Year") +
    ylab("Central Government Debt (% of GNI)") +
    scale_colour_brewer("Sovereigns", palette="Paired")



## ----Download-Developing-Country-List, echo = FALSE-------------------------------------------------------------------------------------------------
developing_countries_url <- "https://raw.githubusercontent.com/georgeaholt/Capstone/main/DevelopingCountries.csv"
developing_countries <- read.csv(developing_countries_url)
developing_country_list <- unlist(developing_countries$Country)


## ----Create-Indicators-Tibble, warnings = FALSE, message = FALSE, echo = FALSE----------------------------------------------------------------------
# Get raw indicators by joining the individual indicator tibbles
indicators_joined <- left_join(left_join(left_join(gni_per_capita, 
                      gni_growth), inflation_cpi), current_account_balance)
# Drop rows that have NAs
indicators_filtered <- indicators_joined %>% 
  filter(!is.na(per_capita_gni) & !is.na(growth_gni) & !is.na(cpi_inflation) & 
          !is.na(current_account_ratio))

indicators <- indicators_filtered %>% 
  mutate(developing = ifelse(country %in% developing_country_list, 1, 0))


## ----Generate-Moodys-Observations, warnings = FALSE, echo = FALSE-----------------------------------------------------------------------------------
# Generate Moodys observations
moodys_historical_ratings <- historical_credit_ratings %>% 
  filter(agency == "Moodys") %>% 
  mutate(rating = factor(rating, ordered = TRUE, levels = names(moodys_rating_order)),
         year = as.integer(year))
moodys_observations <- merge(moodys_historical_ratings, indicators, by = c("country", "year")) %>% 
  mutate(per_capita_gni = log(per_capita_gni)) %>% 
  dplyr::select(-c(agency, country_code))

## ----Generate-SandP-Observations, warning = FALSE, echo = FALSE-------------------------------------------------------------------------------------
# Generate S and P observations
sandp_historical_ratings <- historical_credit_ratings %>% 
  filter(agency == "SandP") %>% 
  mutate(rating = factor(rating, ordered = TRUE, levels = names(sandp_rating_order)),
         year = as.integer(year))
sandp_observations <- merge(sandp_historical_ratings, indicators, by = c("country", "year")) %>% 
  mutate(per_capita_gni = log(per_capita_gni)) %>% 
  dplyr::select(-c(agency, country_code))

## ----Generate-Fitch-Observations, warning = FALSE, echo = FALSE-------------------------------------------------------------------------------------
# Generate Fitch observations
fitch_historical_ratings <- historical_credit_ratings %>% 
  filter(agency == "Fitch") %>% 
  mutate(rating = factor(rating, ordered = TRUE, levels = names(fitch_rating_order)),
         year = as.integer(year))
fitch_observations <- merge(fitch_historical_ratings, indicators, by = c("country", "year")) %>% 
  mutate(per_capita_gni = log(per_capita_gni)) %>% 
  dplyr::select(-c(agency, country_code))


## ----Display-Ratings-vs-Per-Capita-GNI, fig.cap = "**Per Capita GNI vs Agency Rating Category**", echo = FALSE--------------------------------------
# Plot ratings vs per capita GNI for 3 agencies
per_capita_gni_plot_moodys <- 
  moodys_observations %>% 
  ggplot(aes(per_capita_gni, rating, fill = "lightblue")) +
    geom_boxplot(fill = "lightblue") +
    xlab("ln Per Capita GNI") +
    ylab("Moody's Rating")
per_capita_gni_plot_sandp <- 
  sandp_observations %>% 
  ggplot(aes(per_capita_gni, rating, fill = "lightblue")) +
    geom_boxplot(fill = "lightblue") +
    xlab("ln Per Capita GNI") +
    ylab("S & P Rating")
per_capita_gni_plot_fitch <- 
  fitch_observations %>% 
  ggplot(aes(per_capita_gni, rating, fill = "lightblue")) +
    geom_boxplot(fill = "lightblue") +
    xlab("ln Per Capita GNI") +
    ylab("Fitch Rating")
  
plot_grid(per_capita_gni_plot_moodys, per_capita_gni_plot_sandp, per_capita_gni_plot_fitch, align = "h", nrow = 1)


## ----Display-Ratings-vs-GNI-Growth, fig.cap = "**GNI Growth Rate vs Agency Rating Category**", echo = FALSE-----------------------------------------
# Plot ratings vs GNI growth for 3 agencies
gni_growth_plot_moodys <- 
  moodys_observations %>% 
  ggplot(aes(growth_gni, rating, fill = "lightblue")) +
    geom_boxplot(fill = "lightblue") +
    xlab("GNI Growth Rate") +
    ylab("Moody's Rating")
gni_growth_plot_sandp <- 
  sandp_observations %>% 
  ggplot(aes(growth_gni, rating, fill = "lightblue")) +
    geom_boxplot(fill = "lightblue") +
    xlab("GNI Growth Rate") +
    ylab("S & P Rating")
gni_growth_plot_fitch <- 
  fitch_observations %>% 
  ggplot(aes(growth_gni, rating, fill = "lightblue")) +
    geom_boxplot(fill = "lightblue") +
    xlab("GNI Growth Rate") +
    ylab("Fitch Rating")
  
plot_grid(gni_growth_plot_moodys, gni_growth_plot_sandp, gni_growth_plot_fitch, align = "h", nrow = 1)


## ----Display-Ratings-vs-CPI-Inflation, fig.cap = "**CPI Inflation Rate vs Agency Rating Category**", echo = FALSE-----------------------------------
# Plot ratings vs CPI inflation for 3 agencies
cpi_inflation_plot_moodys <- 
  moodys_observations %>% 
  ggplot(aes(cpi_inflation, rating, fill = "lightblue")) +
    geom_boxplot(fill = "lightblue") +
    xlab("CPI Inflation Rate") +
    ylab("Moody's Rating")
cpi_inflation_plot_sandp <- 
  sandp_observations %>% 
  ggplot(aes(cpi_inflation, rating, fill = "lightblue")) +
    geom_boxplot(fill = "lightblue") +
    xlab("CPI Inflation Rate") +
    ylab("S & P Rating")
cpi_inflation_plot_fitch <- 
  fitch_observations %>% 
  ggplot(aes(cpi_inflation, rating, fill = "lightblue")) +
    geom_boxplot(fill = "lightblue") +
    xlab("CPI Inflation Rate") +
    ylab("Fitch Rating")
  
plot_grid(cpi_inflation_plot_moodys, cpi_inflation_plot_sandp, cpi_inflation_plot_fitch, align = "h", nrow = 1)


## ----Display-Ratings-vs-Current-Account-Ratio, fig.cap = "**Current Account Ratio vs Agency Rating Category**", echo = FALSE------------------------
# Plot ratings vs current account ratio for 3 agencies
current_account_ratio_plot_moodys <- 
  moodys_observations %>% 
  ggplot(aes(current_account_ratio, rating, fill = "lightblue")) +
    geom_boxplot(fill = "lightblue") +
    xlab("Current Account Ratio") +
    ylab("Moody's Rating")
current_account_ratio_plot_sandp <- 
  sandp_observations %>% 
  ggplot(aes(current_account_ratio, rating, fill = "lightblue")) +
    geom_boxplot(fill = "lightblue") +
    xlab("Current Account Ratio") +
    ylab("S & P Rating")
current_account_ratio_plot_fitch <- 
  fitch_observations %>% 
  ggplot(aes(current_account_ratio, rating, fill = "lightblue")) +
    geom_boxplot(fill = "lightblue") +
    xlab("Current Account Ratio") +
    ylab("Fitch Rating")
  
plot_grid(current_account_ratio_plot_moodys, current_account_ratio_plot_sandp, current_account_ratio_plot_fitch, align = "h", nrow = 1)


## ----Display-Developing-vs-Developed, fig.cap = "**Agency Rating Category Developing vs Developed**", echo = FALSE----------------------------------
# Plot developing and developed distributions for 3 agencies
developing_plot_moodys <- moodys_observations %>% 
  filter(developing == 1) %>% 
  ggplot(aes(rating)) +
    geom_bar() +
    theme(axis.text.x = element_text(angle = 90)) +
    ggtitle("Developing") +
    xlab("Moody's Rating")
developed_plot_moodys <- moodys_observations %>% 
  filter(developing == 0) %>% 
  ggplot(aes(rating)) +
    geom_bar() +
    theme(axis.text.x = element_text(angle = 90)) +
    ggtitle("Developed") +
    xlab("Moody's Rating")
developing_plot_sandp <- sandp_observations %>% 
  filter(developing == 1) %>% 
  ggplot(aes(rating)) +
    geom_bar() +
    theme(axis.text.x = element_text(angle = 90)) +
    ggtitle("Developing") +
    xlab("S & P Rating")
developed_plot_sandp <- sandp_observations %>% 
  filter(developing == 0) %>% 
  ggplot(aes(rating)) +
    geom_bar() +
    theme(axis.text.x = element_text(angle = 90)) +
    ggtitle("Developed") +
    xlab("S & P Rating")
developing_plot_fitch <- fitch_observations %>% 
  filter(developing == 1) %>% 
  ggplot(aes(rating)) +
    geom_bar() +
    theme(axis.text.x = element_text(angle = 90)) +
    ggtitle("Developing") +
    xlab("Fitch Rating")
developed_plot_fitch <- fitch_observations %>% 
  filter(developing == 0) %>% 
  ggplot(aes(rating)) +
    geom_bar() +
    theme(axis.text.x = element_text(angle = 90)) +
    ggtitle("Developed") +
    xlab("Fitch Rating")

plot_grid(developing_plot_moodys, developed_plot_moodys, 
          developing_plot_sandp, developed_plot_sandp,
          developing_plot_fitch, developed_plot_fitch,
          align = "h", nrow = 3, ncol = 2)


## ----Randomly-Partition-Moodys-Observations, warning = FALSE, message = FALSE, echo = FALSE---------------------------------------------------------
# Randomly partition Moody's observations into train and test tibbles
set.seed(7, sample.kind="Rounding")
moodys_test_size <- floor(0.3 * nrow(moodys_observations))
test_index <- sample(seq_len(nrow(moodys_observations)), size = moodys_test_size)
moodys_observations_train <- moodys_observations[-test_index,]
moodys_observations_test <- moodys_observations[test_index,]


## ----Randomly-Partition-SandP-Observations, warning = FALSE, message = FALSE, echo = FALSE----------------------------------------------------------
# Randomly partition SandP observations into train and test tibbles
set.seed(7, sample.kind="Rounding")
sandp_test_size <- floor(0.3 * nrow(sandp_observations))
test_index <- sample(seq_len(nrow(sandp_observations)), size = sandp_test_size)
sandp_observations_train <- sandp_observations[-test_index,]
sandp_observations_test <- sandp_observations[test_index,]

## ----Randomly-Partition-Fitch-Observations, warning = FALSE, message = FALSE, echo = FALSE----------------------------------------------------------
# Randomly partition Fitch observations into train and test tibbles
set.seed(7, sample.kind="Rounding")
fitch_test_size <- floor(0.3 * nrow(fitch_observations))
test_index <- sample(seq_len(nrow(fitch_observations)), size = fitch_test_size)
fitch_observations_train <- fitch_observations[-test_index,]
fitch_observations_test <- fitch_observations[test_index,]


## ----Fit-Moodys-Rating-Ordinal-Forest-Model, warning = FALSE, message = FALSE, echo = FALSE---------------------------------------------------------
# Fit Moodys ordinalforest model
moodys_observations_train_data <- moodys_observations_train %>% 
  dplyr::select(-c("country", "year")) 
ordfor_fit_moodys <- ordfor(depvar = "rating", data = moodys_observations_train_data, 
                            nsets=100, ntreeperdiv=100, ntreefinal=500, 
                            perffunction = "probability")

## ----Display-Moodys-Ordinal-Forest-Fit, comment = "", size = "scriptsize", warning = FALSE, echo = FALSE--------------------------------------------
# Display Moodys ordinal forest object
ordfor_fit_moodys


## ----Display-Moodys-Ordinal-Forest-Importance, comment = "", warning = FALSE, echo = FALSE----------------------------------------------------------
# Display Moodys ordinal forest variable importance
ordfor_fit_moodys$varimp


## ----Predict-Moodys-Ordinal-Forest-Ratings, echo = FALSE--------------------------------------------------------------------------------------------
# Predict Moodys ordinal forest ratings
moodys_observations_test_data <- moodys_observations_test %>% 
  dplyr::select(-c(country, year))
moodys_rating_predictions_ordinal_forest <- predict(ordfor_fit_moodys, newdata = moodys_observations_test_data)

## ----Generate-And-Display-Moodys-Prediction-Counts-Ordinal-Forest, echo = FALSE---------------------------------------------------------------------
# Produce Moodys ordinal forest confusion matrix
moodys_observed_ratings_ordinal_forest <- factor(moodys_observations_test_data$rating, 
                                                 levels = moodys_rating_categories, ordered = TRUE)
moodys_predicted_ratings_ordinal_forest <- factor(moodys_rating_predictions_ordinal_forest$ypred, 
                                                  levels = moodys_rating_categories, ordered = TRUE)
moodys_prediction_counts_ordinal_forest <- table(moodys_predicted_ratings_ordinal_forest, 
                                                 moodys_observed_ratings_ordinal_forest)

moodys_ordinal_forest_tibble <- tibble(moodys_predicted_ratings_ordinal_forest, moodys_observed_ratings_ordinal_forest)

knitr::kable(moodys_prediction_counts_ordinal_forest, 
             caption = "Moody's Ordinal Forest Model Confusion Matrix", 
             format = "latex", 
             booktabs = TRUE) %>% 
  kable_styling(latex_options = c("striped", "scale_down")) %>% 
    add_header_above(header = c("Actual Rating " = 1, "Predicted Rating" = dim(moodys_prediction_counts_ordinal_forest)[1] - 1))



## ----Plot-Moodys-Ordinal-Forest-Mosaic, fig.cap = "Moody's Ordinal Forest Model Prediction Mosaic", fig.height = 8, fig.width = 8, echo = FALSE-----
# Produce Moodys ordinal forest mosaic chart
mosaicplot(moodys_prediction_counts_ordinal_forest,
           main = "Moody's Ordinal Forest Credit Ratings Mosaic Chart",
           color = c("slateblue", "slateblue1"),
           xlab = "Observed Rating", 
           ylab = "Predicted Rating")



## ----Fit-SandP-Rating-Ordinal-Forest-Model, warnings = FALSE, message = FALSE, echo = FALSE---------------------------------------------------------
# Fit S and P ordinalforest model
sandp_observations_train_data <- sandp_observations_train %>% 
  dplyr::select(-c("country", "year"))
ordfor_fit_sandp <- ordfor(depvar = "rating", data = sandp_observations_train_data, 
                            nsets=100, ntreeperdiv=100, ntreefinal=500, 
                            perffunction = "probability")

## ----Display-SandP-Ordinal-Forest-Fit, comment = "", echo = FALSE-----------------------------------------------------------------------------------
# Display S and P ordinal forest object
ordfor_fit_sandp


## ----Display-SandP-Ordinal-Forest-Importance, comment = "", echo = FALSE----------------------------------------------------------------------------
# Display S and P ordinal forest variable importance
ordfor_fit_sandp$varimp


## ----Predict-SandP-Ordinal-Forest-Ratings, echo = FALSE---------------------------------------------------------------------------------------------
# Predict S and P ordinal forest ratings
sandp_observations_test_data <- sandp_observations_test %>% 
  dplyr::select(-c(country, year))
sandp_rating_predictions_ordinal_forest <- predict(ordfor_fit_sandp, newdata = sandp_observations_test_data)

## ----Generate-And-Display-SandP-Prediction-Counts, echo = FALSE-------------------------------------------------------------------------------------
# Produce S and P ordinal forest confusion matrix
sandp_observed_ratings_ordinal_forest <- factor(sandp_observations_test_data$rating, 
                                                levels = sandp_rating_categories, ordered = TRUE)
sandp_predicted_ratings_ordinal_forest <- factor(sandp_rating_predictions_ordinal_forest$ypred, 
                                                 levels = sandp_rating_categories, ordered = TRUE)
sandp_prediction_counts_ordinal_forest <- table(sandp_predicted_ratings_ordinal_forest, 
                                                sandp_observed_ratings_ordinal_forest)

sandp_ordinal_forest_tibble <- tibble(sandp_predicted_ratings_ordinal_forest, sandp_observed_ratings_ordinal_forest)

knitr::kable(sandp_prediction_counts_ordinal_forest, 
             caption = "Standard and Poors Ordinal Forest Model Confusion Matrix", 
             format = "latex", 
             booktabs = TRUE) %>% 
  kable_styling(latex_options = c("striped", "scale_down")) %>% 
    add_header_above(header = c("Actual Rating " = 1, "Predicted Rating" = dim(sandp_prediction_counts_ordinal_forest)[1] - 1))



## ----Plot-SandP-Ordinal-Forest-Mosaic, fig.cap = "Standard & Poors Ordinal Forest Model Prediction Mosaic", fig.height = 8, fig.width = 8, echo = FALSE----
# Produce S and P ordinal forest mosaic chart
mosaicplot(sandp_prediction_counts_ordinal_forest,
           main = "Standard & Poors Credit Ratings Mosaic Chart",
           color = c("slateblue", "slateblue1"),
           xlab = "Observed Rating", 
           ylab = "Predicted Rating")



## ----Fit-Fitch-Rating-Ordinal-Forest-Model, warnings = FALSE, message = FALSE, echo = FALSE---------------------------------------------------------
# Fit Fitch ordinalforest model
fitch_observations_train_data <- fitch_observations_train %>% 
  dplyr::select(-c("country", "year"))
ordfor_fit_fitch <- ordfor(depvar = "rating", data = fitch_observations_train_data, 
                            nsets=100, ntreeperdiv=100, ntreefinal=500, 
                            perffunction = "probability")

## ----Display-Fitch-Ordinal-Forest-Fit, comment = "", echo = FALSE-----------------------------------------------------------------------------------
# Display Fitch ordinal forest object
ordfor_fit_fitch


## ----Display-Fitch-Ordinal-Forest-Importance, comment = "", echo = FALSE----------------------------------------------------------------------------
# Display Fitch ordinal forest variable importance
ordfor_fit_fitch$varimp


## ----Predict-Fitch-Ordinal-Forest-Ratings, echo = FALSE---------------------------------------------------------------------------------------------
# Predict Fitch ordinal forest ratings
fitch_observations_test_data <- fitch_observations_test %>% 
  dplyr::select(-c(country, year))
fitch_rating_predictions_ordinal_forest <- predict(ordfor_fit_fitch, newdata = fitch_observations_test_data)

## ----Generate-And-Display-Fitch-Prediction-Counts, echo = FALSE-------------------------------------------------------------------------------------
# Produce Fitch ordinal forest confusion matrix
fitch_observed_ratings_ordinal_forest <- factor(fitch_observations_test_data$rating, 
                                                levels = fitch_rating_categories, ordered = TRUE)
fitch_predicted_ratings_ordinal_forest <- factor(fitch_rating_predictions_ordinal_forest$ypred, 
                                                 levels = fitch_rating_categories, ordered = TRUE)
fitch_prediction_counts_ordinal_forest <- table(fitch_predicted_ratings_ordinal_forest, 
                                                fitch_observed_ratings_ordinal_forest)

fitch_ordinal_forest_tibble <- tibble(fitch_predicted_ratings_ordinal_forest, fitch_observed_ratings_ordinal_forest)

knitr::kable(fitch_prediction_counts_ordinal_forest, 
             caption = "Fitch Ordinal Forest Model Confusion Matrix", 
             format = "latex", 
             booktabs = TRUE) %>% 
  kable_styling(latex_options = c("striped", "scale_down")) %>% 
    add_header_above(header = c("Actual Rating" = 1, "Predicted Rating" = dim(fitch_prediction_counts_ordinal_forest)[1] - 1))



## ----Plot-Fitch-Ordinal-Forest-Mosaic, fig.cap = "Fitch Ordinal Forest Model Prediction Mosaic", fig.height = 8, fig.width = 8, echo = FALSE--------
# Produce Fitch ordinal forest mosaic chart
mosaicplot(fitch_prediction_counts_ordinal_forest,
           main = "Fitch Credit Ratings Mosaic Chart",
           color = c("slateblue", "slateblue1"),
           xlab = "Observed Rating", 
           ylab = "Predicted Rating")



## ----Fit-Moodys-LDA-Model, warning = FALSE, message = FALSE, echo = FALSE---------------------------------------------------------------------------
# Fit Moodys LDA model
lda_fit_moodys <- lda(rating ~ per_capita_gni + growth_gni + cpi_inflation + 
                      current_account_ratio + developing, 
                      data = moodys_observations_train_data)


## ----Display-Moodys-LDA-Fit, comment = "", echo = FALSE---------------------------------------------------------------------------------------------
# Display Moodys LDA object
lda_fit_moodys


## ----Plot-Moodys-LDA-Fit, fig.cap = "**Moody's Linear Discriminant Functions**", echo = FALSE-------------------------------------------------------
# Plot Moodys LDA discriminant functions
plot(lda_fit_moodys)


## ----Predict-Moodys-LDA-Ratings, echo = FALSE-------------------------------------------------------------------------------------------------------
# Predict Moodys LDA ratings
moodys_observations_test_data <- moodys_observations_test %>% 
  dplyr::select(-c(country, year))
moodys_rating_predictions_LDA <- predict(lda_fit_moodys, newdata = moodys_observations_test_data)

## ----Generate-And-Display-Moodys-Prediction-Counts-LDA, echo = FALSE--------------------------------------------------------------------------------
# Produce and display Moodys LDA confusion matrix
moodys_observed_ratings_LDA <- factor(moodys_observations_test_data$rating, 
                                                 levels = moodys_rating_categories, ordered = TRUE)
moodys_predicted_ratings_LDA <- factor(moodys_rating_predictions_LDA$class, 
                                                  levels = moodys_rating_categories, ordered = TRUE)
moodys_prediction_counts_LDA <- table(moodys_predicted_ratings_LDA, 
                                                 moodys_observed_ratings_LDA)

moodys_LDA_tibble <- tibble(moodys_predicted_ratings_LDA, moodys_observed_ratings_LDA)

knitr::kable(moodys_prediction_counts_LDA, 
             caption = "Moody's Linear Discriminant Analysis Model Confusion Matrix", 
             format = "latex", 
             booktabs = TRUE) %>% 
  kable_styling(latex_options = c("striped", "scale_down")) %>% 
    add_header_above(header = c("Actual Rating " = 1, "Predicted Rating" = dim(moodys_prediction_counts_LDA)[1] - 1))



## ----Plot-Moodys-LDA-Mosaic, fig.cap = "Moody's Linear Discriminant Model Prediction Mosaic", fig.height = 8, fig.width = 8, echo = FALSE-----------
# Plot Moodys LDA mosaic chart
mosaicplot(moodys_prediction_counts_LDA,
           main = "Moody's Credit Ratings Mosaic Chart",
           color = c("slateblue", "slateblue1"),
           xlab = "Observed Rating", 
           ylab = "Predicted Rating")



## ----Fit-Sandp-LDA-Model, warning = FALSE, message = FALSE, echo = FALSE----------------------------------------------------------------------------
# Fit S and P LDA model
lda_fit_sandp <- lda(rating ~ per_capita_gni + growth_gni + cpi_inflation + 
                      current_account_ratio + developing, 
                      data = sandp_observations_train_data)



## ----Display-SandP-LDA-Fit, comment = "", echo = FALSE----------------------------------------------------------------------------------------------
# Display S and P LDA object
lda_fit_sandp


## ----Plot-SandP-LDA-Fit, fig.cap = "**Standard and Poors Linear Discriminant Functions**", echo = FALSE---------------------------------------------
# Plot S and P LDA discriminant functions
plot(lda_fit_sandp)


## ----Predict-SandP-LDA-Ratings, echo = FALSE--------------------------------------------------------------------------------------------------------
# Predict S and P LDA ratings
sandp_observations_test_data <- sandp_observations_test %>% 
  dplyr::select(-c(country, year))
sandp_rating_predictions_LDA <- predict(lda_fit_sandp, newdata = sandp_observations_test_data)

## ----Generate-And-Display-SandP-Prediction-Counts-LDA, echo = FALSE---------------------------------------------------------------------------------
# Produce and display S and P LDA confusion matrix
sandp_observed_ratings_LDA <- factor(sandp_observations_test_data$rating, 
                                                 levels = sandp_rating_categories, ordered = TRUE)
sandp_predicted_ratings_LDA <- factor(sandp_rating_predictions_LDA$class, 
                                                  levels = sandp_rating_categories, ordered = TRUE)
sandp_prediction_counts_LDA <- table(sandp_predicted_ratings_LDA, 
                                                 sandp_observed_ratings_LDA)
                                     
sandp_LDA_tibble <- tibble(sandp_predicted_ratings_LDA, sandp_observed_ratings_LDA)

knitr::kable(sandp_prediction_counts_LDA, 
             caption = "Standard and Poors Linear Discriminant Analysis Model Confusion Matrix", 
             format = "latex", 
             booktabs = TRUE) %>% 
  kable_styling(latex_options = c("striped", "scale_down")) %>% 
    add_header_above(header = c("Actual Rating " = 1, "Predicted Rating" = dim(sandp_prediction_counts_LDA)[1] - 1))


## ----Plot-SandP-LDA-Mosaic, fig.cap = "Standard & Poors Linear Discriminant Model Prediction Mosaic", fig.height = 8, fig.width = 8, echo = FALSE----
# Plot S and P LDA mosaic chart
mosaicplot(sandp_prediction_counts_LDA,
           main = "Standard & Poors Credit Ratings Mosaic Chart",
           color = c("slateblue", "slateblue1"),
           xlab = "Observed Rating", 
           ylab = "Predicted Rating")



## ----Fit-Fitch-LDA-Model, warning = FALSE, message = FALSE, echo = FALSE----------------------------------------------------------------------------
# Fit Fitch LDA model
lda_fit_fitch <- lda(rating ~ per_capita_gni + growth_gni + cpi_inflation + 
                      current_account_ratio, 
                      data = fitch_observations_train_data)



## ----Display-Fitch-LDA-Fit, comment = "", echo = FALSE----------------------------------------------------------------------------------------------
# Display Fitch LDA object
lda_fit_fitch


## ----Plot-Fitch-LDA-Fit, fig.cap = "**Fitch Linear Discriminant Functions**", echo = FALSE----------------------------------------------------------
# Plot Fitch LDA discriminant functions
plot(lda_fit_fitch)


## ----Predict-Fitch-LDA-Ratings, echo = FALSE--------------------------------------------------------------------------------------------------------
# Predict Fitch LDA ratings
fitch_observations_test_data <- fitch_observations_test %>% 
  dplyr::select(-c(country, year))
fitch_rating_predictions_LDA <- predict(lda_fit_fitch, newdata = fitch_observations_test_data)

## ----Generate-And-Display-Fitch-Prediction-Counts-LDA, echo = FALSE---------------------------------------------------------------------------------
# Produce and display Fitch LDA confusion matrix
fitch_observed_ratings_LDA <- factor(fitch_observations_test_data$rating, 
                                                 levels = fitch_rating_categories, ordered = TRUE)
fitch_predicted_ratings_LDA <- factor(fitch_rating_predictions_LDA$class, 
                                                  levels = fitch_rating_categories, ordered = TRUE)
fitch_prediction_counts_LDA <- table(fitch_predicted_ratings_LDA, 
                                                 fitch_observed_ratings_LDA)

knitr::kable(fitch_prediction_counts_LDA, 
             caption = "Fitch Linear Discriminant Analysis Model Confusion Matrix", 
             format = "latex", 
             booktabs = TRUE) %>% 
  kable_styling(latex_options = c("striped", "scale_down")) %>% 
    add_header_above(header = c("Actual Rating " = 1, "Predicted Rating" = dim(fitch_prediction_counts_LDA)[1] - 1))

fitch_LDA_tibble <- tibble(fitch_predicted_ratings_LDA, fitch_observed_ratings_LDA)

## ----Plot-Fitch-LDA-Mosaic, fig.cap = "Fitch Linear Discriminant Model Prediction Mosaic", fig.height = 8, fig.width = 8, echo = FALSE--------------
# Plot Fitch LDA mosaic chart
mosaicplot(fitch_prediction_counts_LDA,
           main = "Fitch Credit Ratings Mosaic Chart",
           color = c("slateblue", "slateblue1"),
           xlab = "Observed Rating", 
           ylab = "Predicted Rating")



## ----Display-kappa-Strength, warning = FALSE, echo = FALSE------------------------------------------------------------------------------------------
# Display kappa strength table
Value_of_kappa <- c("< 0", "0.01 - 0.20", "0.21 - 0.40", "0.41 - 0.60", "0.61 - 0.80", "0.81 - 1.00") 
strength_of_agreement <- c("Poor", "Slight", "Fair", "Moderate", "Substantial", "Almost perfect")
agreement_table <- tibble(Value_of_kappa, strength_of_agreement)
names(agreement_table) <- c("Value of kappa", "Strength")

knitr::kable(agreement_table, 
             caption = "Kappa Statistic Strength of Agreement", 
             format = "latex", 
             booktabs = TRUE)



## ----Define-Model-Performance-Function, echo = FALSE------------------------------------------------------------------------------------------------
model_performance <- function(response_tibble) {
  agree_result <- agree(response_tibble)
  num_obs <- agree_result$subjects
  method_agree <- agree_result$method
  value_agree <- agree_result$value
  kappa_unweighted_result <- kappa2(response_tibble, weight = "unweighted")
  Kappa_unweighted_method <- kappa_unweighted_result$method
  kappa_unweighted_value <- kappa_unweighted_result$value
  kappa_unweighted_stat.name <- kappa_unweighted_result$stat.name
  kappa_unweighted_statistic <- kappa_unweighted_result$statistic
  kappa_unweighted_p.value <- kappa_unweighted_result$p.value
  kappa_equalweight_result <- kappa2(response_tibble, weight = "equal")
  kappa_equalweight_method <- kappa_equalweight_result$method
  kappa_equalweight_value <- kappa_equalweight_result$value
  kappa_equalweight_stat.name <- kappa_equalweight_result$stat.name
  kappa_equalweight_statistic <- kappa_equalweight_result$statistic
  kappa_equalweight_p.value <- kappa_equalweight_result$p.value
  kappa_squared_result <- kappa2(response_tibble, weight = "squared")
  Kappa_squared_method <- kappa_squared_result$method
  kappa_squared_value <- kappa_squared_result$value
  kappa_squared_stat.name <- kappa_squared_result$stat.name
  kappa_squared_statistic <- kappa_squared_result$statistic
  kappa_squared_p.value <- kappa_squared_result$p.value
  kendall_W_result <- kendall(response_tibble)
  kendall_W_method <- kendall_W_result$method
  kendall_W_value <- kendall_W_result$value
  kendall_W_stat.name <- kendall_W_result$stat.name
  kendall_W_statistic <- kendall_W_result$statistic
  kendall_W_p.value <- kendall_W_result$p.value 
  spearman_r_method <- "Spearman rank correllation coefficient r"
  spearman_r_value <- 2 * kendall_W_value - 1
  spearman_r_stat.name <- "z"
  spearman_r_statistic <- sqrt((num_obs - 3) / 1.06) * atanh(spearman_r_value)
  performance_methods <- c(method_agree, Kappa_unweighted_method, kappa_equalweight_method, 
                           Kappa_squared_method, kendall_W_method, spearman_r_method)
  performance_values <- c(value_agree, kappa_unweighted_value, kappa_equalweight_value, 
                          kappa_squared_value, kendall_W_value, spearman_r_value)
  performance_stat.names <- c("", kappa_unweighted_stat.name, kappa_equalweight_stat.name, 
                              kappa_squared_stat.name, kendall_W_stat.name, spearman_r_stat.name)
  performance_statistics <- c(NA, kappa_unweighted_statistic, kappa_equalweight_statistic, 
                              kappa_squared_statistic, kendall_W_statistic, spearman_r_statistic)
  performance_p.values <- c(NA, kappa_unweighted_p.value, kappa_equalweight_p.value, 
                            kappa_squared_p.value, kendall_W_p.value, NA)
  performnce_tibble <- tibble(Method = performance_methods, Value = performance_values, 
                              Stat.Name = performance_stat.names, Stat.Value = performance_statistics, 
                              P.value = performance_p.values)
  return(performnce_tibble)
}


## ----Calculate-and-Display-Moodys-Ordinal-Forest-Model-Performance, echo = FALSE--------------------------------------------------------------------
# Calculate and display Moodys ordinal forest model performance
moodys_ordinal_forest_model_performance <- model_performance(moodys_ordinal_forest_tibble)

knitr::kable(moodys_ordinal_forest_model_performance, 
             caption = "Moody's Ordinal Forest Model Performance", 
             format = "latex", 
             booktabs = TRUE) %>% 
  kable_styling(latex_options = c("striped", "scale_down"))



## ----Calculate-and-Display-SandP-Ordinal-Forest-Model-Performance, echo = FALSE---------------------------------------------------------------------
# Calculate and display S and P ordinal forest model performance
sandp_ordinal_forest_model_performance <- model_performance(sandp_ordinal_forest_tibble)

knitr::kable(sandp_ordinal_forest_model_performance, 
             caption = "Standard and Poors Ordinal Forest Model Performance", 
             format = "latex", 
             booktabs = TRUE) %>% 
  kable_styling(latex_options = c("striped", "scale_down"))



## ----Calculate-and-Display-Fitch-Ordinal-Forest-Model-Performance, echo = FALSE---------------------------------------------------------------------
# Calculate and display Fitch ordinal forest model performance
fitch_ordinal_forest_model_performance <- model_performance(fitch_ordinal_forest_tibble)

knitr::kable(fitch_ordinal_forest_model_performance, 
             caption = "Fitch Ordinal Forest Model Performance", 
             format = "latex", 
             booktabs = TRUE) %>% 
  kable_styling(latex_options = c("striped", "scale_down"))



## ----Calculate-and-Display-Moodys-LDA-Model-Performance, echo = FALSE-------------------------------------------------------------------------------
# Calculate and display Moodys LDA model performance
moodys_LDA_model_performance <- model_performance(moodys_LDA_tibble)

knitr::kable(moodys_LDA_model_performance, 
             caption = "Moody's Linear Discriminant Analysis Model Performance", 
             format = "latex", 
             booktabs = TRUE) %>% 
  kable_styling(latex_options = c("striped", "scale_down"))


## ----Calculate-and-Display-SandP-LDA-Model-Performance, echo = FALSE--------------------------------------------------------------------------------
# Calculate and display S and P LDA model performance
sandp_LDA_model_performance <- model_performance(sandp_LDA_tibble)

knitr::kable(sandp_LDA_model_performance, 
             caption = "Standard and Poors Linear Discriminant Analysis Model Performance", 
             format = "latex", 
             booktabs = TRUE) %>% 
  kable_styling(latex_options = c("striped", "scale_down"))


## ----Calculate-and-Display-Fitch-LDA-Model-Performance, echo = FALSE--------------------------------------------------------------------------------
# Calculate and display  Fitch LDA model performance
fitch_LDA_model_performance <- model_performance(fitch_LDA_tibble)

knitr::kable(fitch_LDA_model_performance, 
             caption = "Fitch Linear Discriminant Analysis Model Performance", 
             format = "latex", 
             booktabs = TRUE) %>% 
  kable_styling(latex_options = c("striped", "scale_down"))


