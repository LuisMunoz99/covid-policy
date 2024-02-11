# Authors:     LM
# Maintainers: LM
# Date: 6/2/23
# =========================================


# --- libs --- 

#Load packages
if(!require(pacman))install.packages("pacman")
p_load(dplyr,
       here, 
       readr, 
       lubridate, 
       ggplot2, 
       googlesheets4)


files <- list(input = ("https://docs.google.com/spreadsheets/d/1jtAk5Fdm7GLDtcmSiFseDKPgfazQAqAd0EVQ9dWjSYQ/edit?usp=sharing"), #Tengo que cambiarlo 
              output = here::here("not done"))


# Useful functions 
geom_segment_straight <- function(...) {
  layer <- geom_segment(...)
  new_layer <- ggproto(NULL, layer)
  old_geom <- new_layer$geom
  geom <- ggproto(
    NULL, old_geom,
    draw_panel = function(data, panel_params, coord, 
                          arrow = NULL, arrow.fill = NULL,
                          lineend = "butt", linejoin = "round",
                          na.rm = FALSE) {
      data <- ggplot2:::remove_missing(
        data, na.rm = na.rm, c("x", "y", "xend", "yend", 
                               "linetype", "size", "shape")
      )
      if (ggplot2:::empty(data)) {
        return(zeroGrob())
      }
      coords <- coord$transform(data, panel_params)
      # xend and yend need to be transformed separately, as coord doesn't understand
      ends <- transform(data, x = xend, y = yend)
      ends <- coord$transform(ends, panel_params)
      
      arrow.fill <- if (!is.null(arrow.fill)) arrow.fill else coords$colour
      return(grid::segmentsGrob(
        coords$x, coords$y, ends$x, ends$y,
        default.units = "native", gp = grid::gpar(
          col = alpha(coords$colour, coords$alpha),
          fill = alpha(arrow.fill, coords$alpha),
          lwd = coords$size * .pt,
          lty = coords$linetype,
          lineend = lineend,
          linejoin = linejoin
        ),
        arrow = arrow
      ))
      
    }
  )
  new_layer$geom <- geom
  return(new_layer)
}




# Modificar a que no sea por google docs eso no brega

# Import data 
df_orig <- read_sheet(files$input,
                      col_types = "dcDcttnn",
                      sheet = 2) # Col types format for googlesheets 


# Cleaning
df <- df_orig

df <- df %>% mutate(hour_start = # Changing 24hrs format to 12hrs
                      format(strptime(hour_start, "%Y-%m-%d %H:%M:%S"), "%I:%M %p"), 
                    executive_order = as.factor(executive_order)) 



# Base plot
# Lines for dates 
line1 <- data.frame(x=1.6, xend=7.45, y=14, yend=14)
line2 <- data.frame(x=8.60, xend= 20.40, y=14, yend=14)
line3 <- data.frame(x=21.58, xend= 21.9, y=14, yend=14)
line3.1 <- data.frame(x=21.58, xend= 22.8, y=14, yend=14)
line3.2 <- data.frame(x=23.2, xend= 23.8, y=14, yend=14)
line3.3 <- data.frame(x=24.2, xend= 26.8, y=14, yend=14)
line3.4 <- data.frame(x=27.2, xend= 27.40, y=14, yend=14)


# Add lines to the initial dataset
# Set a number of 'empty bar'
to_add2 <- matrix(NA, empty_bar, ncol(df))
to_add1 <- matrix(NA, try, ncol(df))
colnames(to_add2) <- colnames(df)
colnames(to_add1) <- colnames(df)
df <- rbind(to_add1,df[1:6, ],
            to_add1,df[7:18, ],
            to_add1,df[19:24, ],
            to_add1)

df$id <- seq(1, nrow(df))



plt <- ggplot(df) + 
  annotate("rect", xmin = 1.3, xmax = 20, ymin = 0, ymax = 14, 
           alpha = .1) +
  # Creating columns 
  geom_col(aes(
    x = id,
    y = scale),
    position = "dodge2",
    show.legend = TRUE,
    width = .8,
    fill = "#FFAF3A",
    color = "black") +
  
  geom_segment(data=line1, aes(x=x, xend=xend, y=y, yend=yend), colour="#C00000", size = 3) +
  geom_segment(data=line2, aes(x=x, xend=xend, y=y, yend=yend), colour="#C00000", size = 1.7) +
  geom_segment(data=line3, aes(x=x, xend=xend, y=y, yend=yend), colour="#C00000", size = 1.7) +
  geom_segment(data=line3.1, aes(x=x, xend=xend, y=y, yend=yend), colour="#C00000", size = 1.7) +
  geom_segment(data=line3.2, aes(x=x, xend=xend, y=y, yend=yend), colour="#C00000", size = 1.7) +
  geom_segment(data=line3.3, aes(x=x, xend=xend, y=y, yend=yend), colour="#C00000", size = 1.7) +
  geom_segment(data=line3.4, aes(x=x, xend=xend, y=y, yend=yend), colour="#C00000", size = 1.7) +
  
  
  
  
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-3,4), "cm")) +
  
  
  # Make reference circles
  geom_hline(aes(
    yintercept = y), 
    data.frame(y = c(7:12)), # Add hline within spaces 7-12 
    color = "black",
    alpha = 0.3) + 
  
  
  # Creating space in middle by expanding Y axis
  # scale_y_continuous(
  # limits = c(0, 25),
  #breaks = c(7:12)) +
  
  
  # Polar coordinates (circular plot)
  coord_polar() + 
  
  geom_segment_straight(
    aes(
      x = 3,
      y = 0,
      xend = 18,
      yend = 0)) 



plt



# --- Labels ---

label_df <- df

# calculate the ANGLE of the labels
label_df$angle <- 90 - 360 * (label_df$`id`- 0.5) /nrow(label_df) # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)



label_df <- label_df %>% mutate(hjust = ifelse( label_df$angle < -90, 1.1, -.01), # calculate the alignment of labels: right or left
                                angle = ifelse(label_df$angle < -90, label_df$angle+180, label_df$angle))#,
# color =  case_when(
# id ==  2 ~ "white",
#id ==  3 ~ "white",
#  id ==  4 ~ "white",
#id ==  5 ~ "white",
# id ==  6 ~ "white",
#  id ==  7 ~ "white",
#TRUE ~ "black"))


plt <- plt + geom_text(data = label_df, aes(
  x = id,
  y = scale + .5, 
  label = executive_order,
  hjust = hjust), 
  color = "black", 
  size = 4.2, 
  angle = label_df$angle,
  inherit.aes = FALSE) 


# --- reference scale labels --- 


plt <- plt + 
  geom_label(aes(x = .5,
                 y = 8,
                 label = "8 pm"),
             size = 5,
             fill = "white",
             colour = "white")  + 
  
  geom_label(aes(x = .5,
                 y = 10,
                 label = "10 pm"),
             size = 5,
             fill = "white",
             colour = "white") + 
  
  geom_label(aes(x = .5,
                 y = 12,
                 label = "12 pm"),
             size = 5,
             fill = "white",
             colour = "white") 

plt <- plt +
  annotate(
    x = .5,
    y = 8, 
    label = "8 pm", 
    geom = "text", 
    color = "black",
    size = 5,
    fontface = "bold") +
  
  
  annotate(
    x = 4.5,
    y = 16.5, 
    label = paste("Toques de queda\n", 
                  "de 24 horas"), 
    geom = "text", 
    color = "black",
    size = 5.5) + 
  
  
  
  annotate(
    x = 14.5,
    y = 14.5, 
    label = "2020", 
    geom = "text", 
    color = "black",
    fontface = "bold",
    size = 6) +
  
  annotate(
    x = 5.5,
    y = 15.3, 
    label = "2020", 
    geom = "text", 
    color = "black",
    fontface = "bold",
    size = 6) + 
  
  annotate(
    x = 24.5, 
    y = 15, 
    label = "2021", 
    geom = "text", 
    color = "black",
    fontface = "bold",
    size = 6) + 
  
  
  annotate(
    x = .5,
    y = 10, 
    label = "10 pm", 
    geom = "text", 
    color = "black",
    size = 5,
    fontface = "bold") +
  
  annotate(
    x = .5,
    y = 12, 
    label = "12 pm", 
    geom = "text", 
    color = "black",
    size = 5,
    fontface = "bold") +
  
  annotate(
    x = 8,
    y = 8, 
    label = "8 pm", 
    geom = "text", 
    color = "black",
    size = 5,
    fontface = "bold",
    angle = 265) +
  
  annotate(
    x = 8,
    y = 10, 
    label = "10 pm", 
    geom = "text", 
    color = "black",
    size = 5,
    fontface = "bold",
    angle = 265) +
  
  annotate(
    x = 8,
    y = 12, 
    label = "12 pm", 
    geom = "text", 
    color = "black",
    size = 5,
    fontface = "bold",
    angle = 265) +
  
  annotate(
    x = 21,
    y = 8, 
    label = "8 pm", 
    geom = "text", 
    color = "black",
    size = 5,
    fontface = "bold",
    angle = 95) +
  
  annotate(
    x = 21,
    y = 10, 
    label = "10 pm", 
    geom = "text", 
    color = "black",
    size = 5,
    fontface = "bold",
    angle = 95) +
  
  annotate(
    x = 21,
    y = 12, 
    label = "12 pm", 
    geom = "text", 
    color = "black",
    size = 5,
    fontface = "bold",
    angle = 95) 


plt <- plt +
  
  annotate(
    x = 1.7, 
    y =15.5, 
    label = "MAR 15", 
    geom = "text", 
    color = "black",
    fontface = "bold",
    size = 4,
    angle = 75) +
  
  annotate(
    x = 7.35, 
    y = 15.35, 
    label = "MAY 16", 
    geom = "text", 
    color = "black",
    fontface = "bold",
    size = 4,
    angle = 0) + 
  
  annotate(
    x = 8.72, 
    y = 15.3, 
    label = "MAY 17", 
    geom = "text", 
    color = "black",
    fontface = "bold",
    size = 4,
    angle = -13)  +
  
  annotate(
    x = 20.35, 
    y = 15, 
    label = "ENE 7", 
    geom = "text", 
    color = "black",
    fontface = "bold",
    size = 4,
    angle = 10) + 
  
  
  #geom_label(aes(x = 25.3,
  # y = 30,
  #label = "MAY 23"),
  #size = 4,
  # fill = "white",
  #colour = "white") +
  
  annotate(
    x = 27.36, 
    y = 15.35, 
    label = "MAY 23", 
    geom = "text", 
    color = "black",
    fontface = "bold",
    size = 4,
    angle = -78) + 
  
  
  annotate(
    x = 21.66, 
    y = 15.1, 
    label = "ENE 8", 
    geom = "text", 
    color = "black",
    fontface = "bold",
    size = 4,
    angle = -.5) +
  
  annotate(
    x = 0.5,
    y = 15, 
    label = paste("Hora de inicio\n", 
                  "toque de queda"), 
    geom = "text", 
    color = "black",
    size = 4)










# Formating 

plt

ggsave(files$output, plt, width=9.5, height=12.6, device='tiff', dpi=600)
