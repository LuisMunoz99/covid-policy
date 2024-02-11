# Authors:     LM
# Maintainers: LM
# Date: 6/2/23
# =========================================


# --- libs --- 
if(!require(pacman))install.packages("pacman")
p_load(dplyr, here, readr, lubridate, ggplot2, googlesheets4)


args <- list(input = ("1jtAk5Fdm7GLDtcmSiFseDKPgfazQAqAd0EVQ9dWjSYQ"), #Tengo que cambiarlo 
              output = here::here("plots/output/rosediag_denounce.png"))


# --- Import data --- 
df_orig <- read_sheet(args$input,
                      col_types = "dcDcDnttnnnnc",
                      sheet = 6) # Col types format for googlesheets 



# --- Data cleaning ---
df <- df_orig
#df <- df %>% mutate(denounce_day = ifelse(denounce_day < 2.9,2.5,denounce_day)) #Luego mejorar eso 
#df$denounce_day <- round(df$denounce_day, 0)



# -- Plot ---

# Add lines to the initial dataset
# Set a number of 'empty bar'
empty_bar <- 1

# Add lines to the initial dataset
to_add <- matrix(NA, empty_bar, ncol(df))
colnames(to_add) <- colnames(df)
df <- rbind(to_add,df[1:6, ],
            df[7:18, ],
            df[19:24, ],
            to_add)

df$id <- seq(1, nrow(df))


# Line 1 breaks
line1.5 <- data.frame(x=1.6, xend=1.8, y=70, yend=70)
line1.6 <- data.frame(x=2.2, xend=2.8, y=70, yend=70)
line1.7 <- data.frame(x=3.2, xend=6.45, y=70, yend=70)

# line 2 breaks 
line2 <- data.frame(x=6.6, xend=9.8, y=55, yend=55) 
line2.5 <- data.frame(x=10.2, xend=19.4, y=55, yend=55) 


# line 3
line3 <- data.frame(x=19.6, xend=25.40, y=30, yend=30) 



plt <- ggplot(df) +
  annotate("rect", xmin = 1.2, xmax = 19, ymin = 0, ymax = 55, 
           alpha = .1) + 
  # Creating columns 
  geom_col(aes(
    x = id,
    y = change_denounce),
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
    data.frame(y = c(5,10,20,30)), # Add hline within spaces 3-30
    color = "black",
    alpha = .3) + 
  
  # Creating space in middle by expanding Y axis
  # scale_y_continuous(
  #limits = c(0, 35),
  #expand = c(0, 0),
  #breaks = 3) +
  
  annotate(
    x = 1.7, 
    y =120, 
    label = "MAR 15", 
    geom = "text", 
    color = "black",
    fontface = "bold",
    size = 4,
    angle = 75) +
  
  annotate(
    x = 6.35, 
    y = 120, 
    label = "MAY 16", 
    geom = "text", 
    color = "black",
    fontface = "bold",
    size = 4,
    angle = 5) + 
  
  annotate(
    x = 6.70, 
    y = 95, 
    label = "MAY 17", 
    geom = "text", 
    color = "black",
    fontface = "bold",
    size = 4,
    angle = 5) +
  
  
  geom_label(aes(x = 19.3,
                 y = 85,
                 label = "ENE 7"),
             size = 2,
             fill = "white",
             colour = "white") +
  
  annotate(
    x = 19.3, 
    y = 85, 
    label = "ENE 7", 
    geom = "text", 
    color = "black",
    fontface = "bold",
    size = 4,
    angle = 8) + 
  
  
  #geom_label(aes(x = 25.3,
  # y = 30,
  #label = "MAY 23"),
  #size = 4,
  # fill = "white",
  #colour = "white") +
  
  annotate(
    x = 25.25, 
    y = 50, 
    label = "MAY 23", 
    geom = "text", 
    color = "black",
    fontface = "bold",
    size = 4,
    angle = -75) + 
  
  
  
  geom_label(aes(x = 19.7,
                 y = 45,
                 label = "ENE 8"),
             size = 4,
             fill = "white",
             colour = "white") +
  
  annotate(
    x = 19.75, 
    y = 45, 
    label = "ENE 8", 
    geom = "text", 
    color = "black",
    fontface = "bold",
    size = 4,
    angle = 4) +
  
  
  
  
  
  
  
  
  
  
  scale_y_log10() + 
  
  # Polar coordinates (circular plot)
  coord_polar(start = 0) 





# --- Labels for plot --- 

label_df <- df

# calculate xthe ANGLE of the labels
label_df$angle <- 90 - 360 * (label_df$id - 0.5) /nrow(label_df) # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
# Puedo arreglar esto con un case_when en donde los valores menores a tal y con un grado -90 esten en cierto lado poner la justificacion como me de la gana
#angle = ifelse(label_df$angle < -90, label_df$angle+180, label_df$angle))


label_df <- label_df %>% mutate(
  change_denounce = round(change_denounce, 0),
  hjust = case_when(
    id == 25 ~ .5,
    id == 19 ~ .5,
    id == 20 ~ .5,
    #change_denounce <= 5 & angle <= -100 & id != 20 ~ 0, 
    #change_denounce <= 5 & angle > -100 ~ .4, # DONE
    #id == 20 ~ 0.2, 
    change_denounce == 5 ~ 0.7,
    change_denounce == 54 ~ -.05,
    change_denounce == 51 ~ -.07,
    change_denounce == 35 ~ 0, 
    change_denounce == 19 ~ 0.1, # DONE
    change_denounce == 6 ~ 0.45, # DONE
    change_denounce == 13 ~ 0.2 # DONE
  ),
  angle = ifelse(
    label_df$angle < -90, label_df$angle+180, label_df$angle))

# Labels
plt <- plt + geom_text(data = label_df, aes(
  x = id,
  y = change_denounce + 4, 
  label = executive_order,
  hjust = hjust), 
  color ="black", 
  size = 4, 
  angle = label_df$angle,
  inherit.aes = FALSE) +
  
  # Labels of values
  geom_text(data = label_df, aes(
    x = id,
    y = change_denounce, 
    label = label_change_denounce,
    hjust = 1.2), # Esto me deja poner el label dentro de la grafica
    color = "black", 
    size = 6, 
    angle = label_df$angle,
    inherit.aes = FALSE,
    fontface = "bold"
  ) 

# reference scale labels 2,3,5,10,30
plt <- plt +
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
    y = 5, 
    label = "5", 
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
    x = c(7.82,16.2),
    y = 37, 
    label = "2020", 
    geom = "text", 
    color = "black",
    fontface = "bold",
    size = 6) +
  
  annotate(
    x = 22.5,
    y = 20, 
    label = "2021", 
    geom = "text", 
    color = "black",
    fontface = "bold",
    size = 6) +
  
  annotate(
    x = 0.5,
    y = 80, 
    label = paste("Promedio diario\n", 
                  "de citas\n" ,"al tribunal"), 
    geom = "text", 
    color = "black",
    size = 4)



plt

ggsave(args$output, plt, width=9, height=12.6, device='png', dpi=600)


# Done 