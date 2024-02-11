# Authors:     LM
# Maintainers: LM
# Date: 24-Jan-24

# Importing covid cases data from PR health department
# ===========================================

# -- libs ---
if(!require(pacman))install.packages("pacman")
p_load(argparse,
       here,
       dplyr, 
       googlesheets4)

# args {{{
args <- list(input = "https://docs.google.com/spreadsheets/d/1gwA6FlA_mOLlwJCHvBjZ6mnRTcSltUbaFKVy9DS7h44/edit?usp=sharing",
             output = here("import/output/covid_cases_DS.csv"))
# }}}


# -- Import ---
df <- read_sheet(args$input,
                 sheet = "all",
                 col_types = "Dn") # Date and number data types


# -- Prep ---

out <- df %>% arrange(ymd(df$date_onset)) %>%  # Sorting to earliest to oldest
  mutate(mov_aver = rollmean(cases, k = 7, fill = NA)) %>%  # Moving average in a window of 7 days
  mutate(mov_aver = round(mov_aver)) # Rounding moving average



# -- output ---
write.csv(out, args$output)


