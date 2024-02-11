#
# Creating moving average linear plot of new cases of covid between 15/03/2020 
# to 24/may/2021 
#
# Author:  LMN 
# Date:  03-10-2022
# Organization: Kilometro Cero 

# Set up ----------------------------------------------------------------------


#Load packages
if(!require(pacman))install.packages("pacman")
p_load(dplyr,
       here,
       lubridate,
       readr, 
       ggplot2,  
       showtext, 
       stringr, 
       plotly)

args <- list(input = here("import/output/covid_cases_DS.csv"), 
              output = here("not done"))


Sys.setlocale("LC_TIME", "es_ES") # Changing language to adapt labels in spanish 


# Importing Axia font for plots

fonts <- font_files() %>% tibble
Roboto_fonts <- fonts %>% filter(str_detect(family, "Roboto"))
font_add(family = "Roboto", regular = "Roboto-Regular.ttf")




# plot -------------------------------------------------------------------------

df <- read.csv(args$input)

# Base plot 
plot <- df %>% ggplot() +
  annotate("rect", xmin = as.Date("2020-03-15"), xmax = as.Date("2020-06-15"), ymin = 0, ymax = max(df$mov_aver), 
           alpha = .2) + 
  geom_line(aes(
    x = date_onset, 
    y = mov_aver), size = 1, color = "#FFAF3A") + 
  
  # Changing y scale  
  scale_y_continuous(
    limits = c(0, 450),
    breaks = c(seq(from = 0, to = 500, by = 50))) +
  
  # Changing x scale
  # scale https://r-graph-gallery.com/279-plotting-time-series-with-ggplot2.html
  
  scale_x_date(
    date_labels = "%b",
    date_breaks = "1 month",
    limits = c(as.Date("2020-03-15"), as.Date("2020-10-15")))



plot

# Data labels 
data_lab <- as.Date(c("2020-09-13","2020-07-13","2020-08-9")) # Dates that will be labeled


# Data labels 
data_lab <- as.Date(c("2020-09-13","2020-07-13","2020-08-9")) # Dates that will be labeled

plot <- plot + geom_text(data = df %>% filter(date_onset %in% data_lab), aes(
  x = date_onset,
  y = mov_aver + 10,
  label = mov_aver, 
  vjust = 1))


plot



# Annotations
plot <- plot + annotate(
  x = as.Date("2020-03-15"), 
  y = 20, 
  label = paste("Inicio del confinamiento",
                "15 de marzo",
                "(promedio de casos diarios = 5)",sep = "\n"),
  hjust = 0,
  geom = "text", 
  color = "#444444",
  size = 3) +
  
  annotate(
    x = as.Date("2020-06-16"), 
    y = 150, 
    label = paste("Final del confinamiento de 94 dias",
                  "16 de junio 2020",
                  "(promedio diario = 13)", sep = "\n"), 
    hjust = 0,
    geom = "text", 
    color = "#444444",
    size = 3) +
  
  annotate(
    x = as.Date("2020-08-01"), 
    y = 150, 
    label = paste("En este periodo los",
                  "casos diarios promedio",
                  "no bajaron de 181",sep = "\n"), 
    hjust = 0,
    geom = "text", 
    color = "#444444",
    size = 3) +
  
  annotate(
    x = as.Date("2020-09-18"), 
    y = 200, 
    label = paste("94 dias luego del fin del confinamiento",
                  "18 de septiembre", sep = "\n"), 
    hjust = 0,
    geom = "text", 
    color = "#444444",
    size = 3) + 
  
  # Adding arrows 
  geom_segment(
    x = as.Date("2020-03-15"),
    y = 60,
    xend = as.Date("2020-03-15"),
    yend = 150,
    lineend = "round", # See available arrow types in example above
    linejoin = "round",
    size = .8, 
    arrow = arrow(length = unit(0.2, "cm")),
    colour = "black") +
  
  geom_segment(
    x = as.Date("2020-03-15"),
    y = 60,
    xend = as.Date("2020-03-15"),
    yend = 150,
    lineend = "round", # See available arrow types in example above
    linejoin = "round",
    size = .8, 
    arrow = arrow(length = unit(0.2, "cm")),
    colour = "black") +
  
  geom_segment(
    x = as.Date("2020-03-15"),
    y = 60,
    xend = as.Date("2020-03-15"),
    yend = 150,
    lineend = "round", # See available arrow types in example above
    linejoin = "round",
    size = .8, 
    arrow = arrow(length = unit(0.2, "cm")),
    colour = "black") +
  
  geom_segment(
    x = as.Date("2020-03-15"),
    y = 60,
    xend = as.Date("2020-03-15"),
    yend = 150,
    lineend = "round", # See available arrow types in example above
    linejoin = "round",
    size = .8, 
    arrow = arrow(length = unit(0.2, "cm")),
    colour = "black")

plot

# Formating 
showtext_auto()

plot <- plot +
  labs(
    title = "Gráfica 1:El confinamiento para prevenir el covid19 fue inefectivo",
    subtitle = "Promedio diario de nuevos casos de covid19 (15 mar – 18 de sep, 2020)",
    y = "Promedio diario nuevos casos",
    x = " ",
    caption = "\nFuente: Departamento de Salud de Puerto (2022, septiembre 24). 
                      COVID-19 EN CIFRAS EN PUERTO RICO.\n", sep = "\n")

plot <- plot + theme(
  panel.background = element_rect(fill = "white", color = "white"),
  panel.grid = element_blank(),
  text = element_text(color = "black", family = "Roboto", size = 15),
  plot.title = element_text(size = 20),
  axis.title.y = element_text(margin = margin(r =10)))

plot
#ggplotly(plot)

