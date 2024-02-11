# Authors:     LM
# Maintainers: LM
# Date: 6/2/23
# =========================================


# --- libs --- 
if(!require(pacman))install.packages("pacman")
p_load(dplyr, here, readr, lubridate, ggplot2, googlesheets4)


files <- list(input = ("1jtAk5Fdm7GLDtcmSiFseDKPgfazQAqAd0EVQ9dWjSYQ"), #Tengo que cambiarlo 
              output = here::here("plots/output/rosediag_arrest.png"))


# --- Import data --- 
df_orig <- read_sheet(files$input,
                      col_types = "dcDcDnttnnnnc",
                      sheet = 4) # Col types format for googlesheets 


df <- df_orig


# -- Plot ---

# Add lines to the initial dataset
# Set a number of 'empty bar'
empty_bar <- 1

# Add lines to the initial dataset
to_add <- matrix(NA, empty_bar, ncol(df))
colnames(to_add) <- colnames(df)
df <- rbind(to_add,df,
            to_add)

df$id <- seq(1, nrow(df))
df$id <- as.numeric(df$id)


# Base plot
plt <- ggplot(df) + 
  annotate("rect", xmin = 1.2, xmax = 19, ymin = 0, ymax = 32, 
           alpha = .1)  +
  # Creating columns  
  geom_col(aes(
    x = id,
    y = change_arrest),
    position = "dodge2",
    show.legend = TRUE,
    width = .8,
    fill = "#FFAF3A",
    color = "black") +
  # Custom the theme: no axis title and no cartesian grid
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(0,4), "cm")) +
    
  geom_hline(aes(
  yintercept = y), 
  data.frame(y = c(3,5,10,20,30)), # Add hline within spaces 3-30
  color = "black",
  alpha = .3) + 

  annotate(
    x = 1.7, 
    y =60, 
    label = "MAR 15", 
    geom = "text", 
    color = "black",
    fontface = "bold",
    size = 4,
    angle = 75) +
  
  annotate(
    x = 6.3, 
    y = 60, 
    label = "MAY 16", 
    geom = "text", 
    color = "black",
    fontface = "bold",
    size = 4,
    angle = 5) + 
  
  annotate(
    x = 6.7, 
    y = 47, 
    label = "MAY 17", 
    geom = "text", 
    color = "black",
    fontface = "bold",
    size = 4,
    angle = 5) +

  
  geom_label(aes(x = 19.3,
                 y = 45,
                 label = "ENE 7"),
             size = 2,
             fill = "white",
             colour = "white") +
  
  annotate(
    x = 19.3, 
    y = 45, 
    label = "ENE 7", 
    geom = "text", 
    color = "black",
    fontface = "bold",
    size = 4,
    angle = 8) + 
  
  annotate(
    x = 25.3, 
    y = 30, 
    label = "MAY 23", 
    geom = "text", 
    color = "black",
    fontface = "bold",
    size = 4,
    angle = -75) +  
  

  geom_label(aes(x = 19.7,
                 y = 30,
                 label = "ENE 8"),
             size = 4,
             fill = "white",
             colour = "white") +
  
  annotate(
    x = 19.7, 
    y = 30, 
    label = "ENE 8", 
    geom = "text", 
    color = "black",
    fontface = "bold",
    size = 4,
    angle = 4) + 
  
  
  scale_y_log10() + 
  
  
  # Polar coordinates (circular plot)
  coord_polar(start = 0)

    
plt


# --- Labels for plot --- 

label_df <- df

# calculate the ANGLE of the labels
label_df$angle <- 90 - 360 * (label_df$id - 0.5) /nrow(label_df) # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)


label_df <- label_df %>% mutate(
  change_arrest = round(change_arrest, 0),
  hjust = case_when(
   #change_arrest <= 3 & angle <= -100 & id != 20 ~ .6, 
    id == 25 ~ -.15,
    id == 19 ~ -.15,
    id == 20 ~ -.15,
    change_arrest == 31 ~ 0.01,
    change_arrest == 11 ~ 0.3,
    change_arrest == 5 ~ 0.8,
    TRUE ~ 1),
  angle = ifelse(
    label_df$angle < -90, label_df$angle+180, label_df$angle))
  


  # Labels
plt <- plt + geom_text(data = label_df, aes(
    x = id,
    y = change_arrest + 4, 
    label = executive_order,
    hjust = hjust), 
    color ="black", 
    size = 4, 
    angle = label_df$angle,
    inherit.aes = FALSE) +

  # Labels of values
  geom_text(data = label_df, aes(
    x = id,
    y = change_arrest, 
    label = label_change_arrest,
    hjust = 1.1), # Esto me deja poner el label dentro de la grafica
    color = "black", 
    size = 6.5, 
    angle = label_df$angle,
    inherit.aes = FALSE,
    fontface = "bold"
  )

  # reference scale labels 2,3,5,10,30
plt <- plt  +
  geom_label(aes(x = 0.5,
                 y = 3,
                 label = "3"),
             size = 6,
             fill = "white",
             colour = "white") + 
  geom_label(aes(x = 0.5,
                 y = 5,
                 label = "5"),
             size = 6,
             fill = "white",
             colour = "white") + 
  
  geom_label(aes(x = 0.5,
                 y = 10,
                 label = "10"),
             size = 6,
             fill = "white",
             colour = "white") +
  geom_label(aes(x = 0.5,
                 y = 20,
                 label = "20"),
             size = 6,
             fill = "white",
             colour = "white") +
  
  geom_label(aes(x = 0.5,
                 y = 30,
                 label = "30"),
             size = 6,
             fill = "white",
             colour = "white") + 
  annotate(
    x = 0.5,
    y = 3, 
    label = "3", 
    geom = "text", 
    color = "black",
    fontface = "bold",
    size = 6) +
  
  annotate(
    x = 0.5,
    y = 10, 
    label = "10", 
    geom = "text", 
    color = "black",
    fontface = "bold",
    size = 6) + 
  
  annotate(
    x = 0.5,
    y = 20, 
    label = "20", 
    geom = "text", 
    color = "black",
    fontface = "bold",
    size = 6) + 
  
  annotate(
    x = 0.5,
    y = 30, 
    label = "30", 
    color = "black",
    fill = "white",
    geom = "text", 
    fontface = "bold",
    size = 6
  ) +
  
  
  
  annotate(
    x = 0.5, 
    y = 3, 
    label = "3", 
    geom = "text", 
    color = "black",
    fontface = "bold",
    size = 6
  ) + 
  annotate(
    x = 0.5,
    y = 5, 
    label = "5", 
    geom = "text", 
    color = "black",
    fontface = "bold",
    size = 6
  ) +
  annotate(
    x = 0.5,
    y = 10, 
    label = "10", 
    geom = "text", 
    color = "black",
    fontface = "bold",
    size = 6
  ) +  
  annotate(
    x = 0.5,
    y = 20, 
    label = "20", 
    geom = "text", 
    color = "black",
    fontface = "bold",
    size = 6
  ) + 
  annotate(
    x = 0.5,
    y = 30, 
    label = "30", 
    color = "black",
    geom = "text", 
    fontface = "bold",
    size = 6
  ) +
  annotate(
    x = 13.5,
    y = 25, 
    label = "2020", 
    geom = "text", 
    color = "black",
    fontface = "bold",
    size = 6) +
  
  annotate(
    x = 4.7,
    y = 28, 
    label = "2020", 
    geom = "text", 
    color = "black",
    fontface = "bold",
    size = 6) + 
  
  annotate(
    x = 22.8, 
    y = 13.9, 
    label = "2021", 
    geom = "text", 
    color = "black",
    fontface = "bold",
    size = 6) 

plt

ggsave(files$output, plt, width=9, height=12.6, device='png', dpi=600)
