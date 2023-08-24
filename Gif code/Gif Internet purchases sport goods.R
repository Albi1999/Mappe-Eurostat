# EUROSTAT - Acquisti online di beni sportivi

# CARICAMENTO LIBRERIE ----
# devtools::install_github("thomasp85/gganimate@v1.0.7")

library(tidyverse)
library(sf)
library(rnaturalearth)
library(wbstats)
library(gganimate)
library(classInt)
library(transformr)
library(giscoR)
library(eurostat)
library(gifski)

# CARICAMENTO DATI ----
crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# NUTS2 & country shapefile ----
nuts2 <- giscoR::gisco_get_nuts(
  year = "2021",
  resolution = "3",
  nuts_level = "0",
  spatialtype = "RG"
) |>
  sf::st_transform(crsLONGLAT)

plot(sf::st_geometry(nuts2))
head(nuts2)

cntrys <- giscoR::gisco_get_countries(
  year = "2020",
  resolution = "3",
  region = c("Europe", "Asia")
) |>
  sf::st_transform(crsLONGLAT)

names(cntrys)

# BA = Bosnia & Herzegovina
# BY = Belarus; GE = Georgia
# MD = Moldova; RU = Russia
# UA = Ukraine
non_eu_list <- c(
  "BA", "BY", "GE",
  "MD", "RU", "UA"
)
eu_list <- c(unique(nuts2$CNTR_CODE))

eu <- cntrys |>
  filter(CNTR_ID %in% eu_list)

non_eu <- cntrys |>
  filter(CNTR_ID %in% non_eu_list)

plot(sf::st_geometry(non_eu))
plot(sf::st_geometry(eu))

# EUROSTAT DATA -----

toc <- eurostat::get_eurostat_toc()
i <- "sport"
tab <- subset(
  toc, grepl(i, title)
)
fix(tab)

# TOTAL ----

# get NUTS2-level data
get_eurostat_data <- function() {
  eurostat_df <- eurostat::get_eurostat(
    "isoc_ec_ibgs",
    time_format = "num"
  ) |>
    dplyr::filter(
        unit == "PC_IND" &
        ind_type == "IND_TOTAL" &
        indic_is == "I_BSPG"
    ) |>
    dplyr::select(geo, time, values)
  names(eurostat_df)[1] <- "CNTR_CODE"
  return(eurostat_df)
}

eurostat_df <- get_eurostat_data()
head(eurostat_df)

df <- nuts2 |>
  dplyr::left_join(eurostat_df, by = "CNTR_CODE", relationship = "one-to-many", keep = TRUE)




# LEGEND BREAKS ----
# let's find a natural interval with Jenks breaks
# df$values <- df$values / 1000    # TOGLIERE IL COMMENTO SOLO SE I VALORI SONO MOLTO GRANDI

ni <- classInt::classIntervals(df$values,
                               n = 6,
                               style = "fisher"
)$brks

# this function uses above intervals to create categories
labels <- c()
for (i in 1:length(ni)) {
  labels <- c(labels, paste0(
    round(ni[i], 0),
    "-",
    round(ni[i + 1], 0)
  ))
}
labels <- labels[1:length(labels) - 1]

# finally, carve out the categorical variable
# based on the breaks and labels above
df$cat <- cut(df$values,
              breaks = ni,
              labels = labels,
              include.lowest = T
)
levels(df$cat)

# label NAs, too
lvl <- levels(df$cat)
lvl[length(lvl) + 1] <- "No data"
df$cat <- factor(df$cat, levels = lvl)
df$cat[is.na(df$cat)] <- "No data"
levels(df$cat)

# BOUNDING BOX ----

crsLAEA <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs"
get_bounding_box_europe <- function() {
  xmin <- -10.6600
  xmax <- 40.5500
  ymin <- 28.5000
  ymax <- 85.0500
  
  bbox_laea <- sf::st_sfc(
    sf::st_polygon(list(cbind(
      c(xmin, xmax, xmax, xmin, xmin),
      c(ymin, ymin, ymax, ymax, ymin)
    ))),
    crs = crsLONGLAT
  ) |> sf::st_transform(crsLAEA)
  
  bbox <- sf::st_bbox(bbox_laea)
  return(bbox)
}
bbox <- get_bounding_box_europe()


# BREAKS ----
vmin <- min(df$`value`, na.rm = TRUE, finite = TRUE)
vmax <- max(df$`value`, na.rm = TRUE, finite = TRUE)
brk <- round(classInt::classIntervals(
  df$`value`,
  n = 6,
  style = "fisher"
)$brks, 1) |>
  head(-1) |>
  tail(-1) |>
  append(vmax)
breaks <- c(vmin, brk)

cols <- rev(c('#ff2800', '#e3342a', '#c73b42', '#aa3f57', '#89416c', '#604380', '#004395'))

# ANIMATE ----
get_animated_world_map <- function() {
  world_map <- ggplot(
    data = df,
    aes(fill = values)
  ) +
    geom_sf(color = "white", size = 0.0001) +
    geom_sf(data = eu, color = "grey20", size = 0.025, fill = "transparent") +
    geom_sf(
      data = non_eu, color = "grey20", size = 0.025, fill = "white"
    ) +
    scale_fill_gradientn(
      name = "",
      colours = cols,
      breaks = breaks,
      labels = round(breaks, 1),
      limits = c(vmin, vmax),
      na.value = "white"
    ) +
    coord_sf(
      crs = crsLAEA,
      xlim = c(bbox["xmin"], bbox["xmax"]),
      ylim = c(bbox["ymin"], bbox["ymax"])
    ) +
    guides(fill = guide_legend(
      direction = "horizontal",
      keyheight = unit(1.5, units = "mm"),
      keywidth = unit(15, units = "mm"),
      title.position = "top",
      title.hjust = .5,
      label.hjust = .5,
      nrow = 1,
      byrow = T,
      reverse = F,
      label.position = "bottom"
    )) +
    theme_minimal() +
    theme(
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.position = c(.5, -.015),
      legend.text = element_text(size = 11, color = "#25465d"),
      panel.grid.major = element_line(color = "white", linewidth = .002),
      panel.grid.minor = element_blank(),
      plot.title = element_text(
        face = "bold", size = 20,
        color = "#f0951e", hjust = .5, vjust = -1
      ),
      plot.subtitle = element_text(
        size = 20, color = "#00a2d2",
        hjust = .5, vjust = -1
      ),
      plot.caption = element_text( face = "italic",
                                   size = 10, color = "#25465d",
                                   hjust = .5, vjust = -10
      ),
      plot.margin = unit(c(t = 1, r = 1, b = 2, l = 1), "lines"),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = "white", color = NA),
      panel.border = element_blank()
    ) +
    labs(
      x = "",
      y = "",
      title = "Internet purchases of sports goods",
      subtitle = "Year: {as.integer(current_frame)}",
      caption = "©2023 Sport Business Lab Consultancy\nData: Eurostat"
    )
  
  return(world_map)
}

world_map <- get_animated_world_map()
print(world_map)

timelapse_world_map <- world_map +
  transition_manual(time) +
  enter_grow() +
  ease_aes("quadratic-in-out", interval = .2)


animated_world <- gganimate::animate(
  timelapse_world_map,
  nframes = 120,
  duration = 30,
  start_pause = 1,
  end_pause = 3,
  height = 6,
  width = 7.15,
  res = 300,
  units = "in",
  fps = 15,
  renderer = gifski_renderer(loop = T)
)

gganimate::anim_save(
  "Internet_purchases_sport_goods.gif", animated_world
)


# MALES ----

# get NUTS2-level data
get_eurostat_data <- function() {
  eurostat_df <- eurostat::get_eurostat(
    "isoc_ec_ibgs",
    time_format = "num"
  ) |>
    dplyr::filter(
      unit == "PC_IND" &
        ind_type == "M_Y16_74" &
        indic_is == "I_BSPG"
    ) |>
    dplyr::select(geo, time, values)
  names(eurostat_df)[1] <- "CNTR_CODE"
  return(eurostat_df)
}

eurostat_df <- get_eurostat_data()
head(eurostat_df)

df <- nuts2 |>
  dplyr::left_join(eurostat_df, by = "CNTR_CODE", relationship = "one-to-many", keep = TRUE)




# LEGEND BREAKS ----
# let's find a natural interval with Jenks breaks
# df$values <- df$values / 1000    # TOGLIERE IL COMMENTO SOLO SE I VALORI SONO MOLTO GRANDI

ni <- classInt::classIntervals(df$values,
                               n = 6,
                               style = "fisher"
)$brks

# this function uses above intervals to create categories
labels <- c()
for (i in 1:length(ni)) {
  labels <- c(labels, paste0(
    round(ni[i], 0),
    "-",
    round(ni[i + 1], 0)
  ))
}
labels <- labels[1:length(labels) - 1]

# finally, carve out the categorical variable
# based on the breaks and labels above
df$cat <- cut(df$values,
              breaks = ni,
              labels = labels,
              include.lowest = T
)
levels(df$cat)

# label NAs, too
lvl <- levels(df$cat)
lvl[length(lvl) + 1] <- "No data"
df$cat <- factor(df$cat, levels = lvl)
df$cat[is.na(df$cat)] <- "No data"
levels(df$cat)

# BOUNDING BOX ----

crsLAEA <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs"
get_bounding_box_europe <- function() {
  xmin <- -10.6600
  xmax <- 40.5500
  ymin <- 28.5000
  ymax <- 85.0500
  
  bbox_laea <- sf::st_sfc(
    sf::st_polygon(list(cbind(
      c(xmin, xmax, xmax, xmin, xmin),
      c(ymin, ymin, ymax, ymax, ymin)
    ))),
    crs = crsLONGLAT
  ) |> sf::st_transform(crsLAEA)
  
  bbox <- sf::st_bbox(bbox_laea)
  return(bbox)
}
bbox <- get_bounding_box_europe()


# BREAKS ----
vmin <- min(df$`value`, na.rm = TRUE, finite = TRUE)
vmax <- max(df$`value`, na.rm = TRUE, finite = TRUE)
brk <- round(classInt::classIntervals(
  df$`value`,
  n = 6,
  style = "fisher"
)$brks, 1) |>
  head(-1) |>
  tail(-1) |>
  append(vmax)
breaks <- c(vmin, brk)

cols <- rev(c('#0000ff', '#3745f7', '#5267f3', '#6a85f0', '#81a1ec', '#97bde9', '#add8e6'))

# ANIMATE ----
get_animated_world_map <- function() {
  world_map <- ggplot(
    data = df,
    aes(fill = values)
  ) +
    geom_sf(color = "white", size = 0.0001) +
    geom_sf(data = eu, color = "grey20", size = 0.025, fill = "transparent") +
    geom_sf(
      data = non_eu, color = "grey20", size = 0.025, fill = "white"
    ) +
    scale_fill_gradientn(
      name = "",
      colours = cols,
      breaks = breaks,
      labels = round(breaks, 1),
      limits = c(vmin, vmax),
      na.value = "white"
    ) +
    coord_sf(
      crs = crsLAEA,
      xlim = c(bbox["xmin"], bbox["xmax"]),
      ylim = c(bbox["ymin"], bbox["ymax"])
    ) +
    guides(fill = guide_legend(
      direction = "horizontal",
      keyheight = unit(1.5, units = "mm"),
      keywidth = unit(15, units = "mm"),
      title.position = "top",
      title.hjust = .5,
      label.hjust = .5,
      nrow = 1,
      byrow = T,
      reverse = F,
      label.position = "bottom"
    )) +
    theme_minimal() +
    theme(
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.position = c(.5, -.015),
      legend.text = element_text(size = 11, color = "#25465d"),
      panel.grid.major = element_line(color = "white", linewidth = .002),
      panel.grid.minor = element_blank(),
      plot.title = element_text(
        face = "bold", size = 20,
        color = "#f0951e", hjust = .5, vjust = -1
      ),
      plot.subtitle = element_text(
        size = 20, color = "#00a2d2",
        hjust = .5, vjust = -1
      ),
      plot.caption = element_text( face = "italic",
                                   size = 10, color = "#25465d",
                                   hjust = .5, vjust = -10
      ),
      plot.margin = unit(c(t = 1, r = 1, b = 2, l = 1), "lines"),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = "white", color = NA),
      panel.border = element_blank()
    ) +
    labs(
      x = "",
      y = "",
      title = "Internet purchases of sports goods (Males)",
      subtitle = "Year: {as.integer(current_frame)}",
      caption = "©2023 Sport Business Lab Consultancy\nData: Eurostat"
    )
  
  return(world_map)
}

world_map <- get_animated_world_map()
print(world_map)

timelapse_world_map <- world_map +
  transition_manual(time) +
  enter_grow() +
  ease_aes("quadratic-in-out", interval = .2)


animated_world <- gganimate::animate(
  timelapse_world_map,
  nframes = 120,
  duration = 30,
  start_pause = 1,
  end_pause = 3,
  height = 6,
  width = 7.15,
  res = 300,
  units = "in",
  fps = 15,
  renderer = gifski_renderer(loop = T)
)

gganimate::anim_save(
  "Internet_purchases_sport_goods_males.gif", animated_world
)


# FEMALES ----

# get NUTS2-level data
get_eurostat_data <- function() {
  eurostat_df <- eurostat::get_eurostat(
    "isoc_ec_ibgs",
    time_format = "num"
  ) |>
    dplyr::filter(
      unit == "PC_IND" &
        ind_type == "F_Y16_74" &
        indic_is == "I_BSPG"
    ) |>
    dplyr::select(geo, time, values)
  names(eurostat_df)[1] <- "CNTR_CODE"
  return(eurostat_df)
}

eurostat_df <- get_eurostat_data()
head(eurostat_df)

df <- nuts2 |>
  dplyr::left_join(eurostat_df, by = "CNTR_CODE", relationship = "one-to-many", keep = TRUE)




# LEGEND BREAKS ----
# let's find a natural interval with Jenks breaks
# df$values <- df$values / 1000    # TOGLIERE IL COMMENTO SOLO SE I VALORI SONO MOLTO GRANDI

ni <- classInt::classIntervals(df$values,
                               n = 6,
                               style = "fisher"
)$brks

# this function uses above intervals to create categories
labels <- c()
for (i in 1:length(ni)) {
  labels <- c(labels, paste0(
    round(ni[i], 0),
    "-",
    round(ni[i + 1], 0)
  ))
}
labels <- labels[1:length(labels) - 1]

# finally, carve out the categorical variable
# based on the breaks and labels above
df$cat <- cut(df$values,
              breaks = ni,
              labels = labels,
              include.lowest = T
)
levels(df$cat)

# label NAs, too
lvl <- levels(df$cat)
lvl[length(lvl) + 1] <- "No data"
df$cat <- factor(df$cat, levels = lvl)
df$cat[is.na(df$cat)] <- "No data"
levels(df$cat)

# BOUNDING BOX ----

crsLAEA <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs"
get_bounding_box_europe <- function() {
  xmin <- -10.6600
  xmax <- 40.5500
  ymin <- 28.5000
  ymax <- 85.0500
  
  bbox_laea <- sf::st_sfc(
    sf::st_polygon(list(cbind(
      c(xmin, xmax, xmax, xmin, xmin),
      c(ymin, ymin, ymax, ymax, ymin)
    ))),
    crs = crsLONGLAT
  ) |> sf::st_transform(crsLAEA)
  
  bbox <- sf::st_bbox(bbox_laea)
  return(bbox)
}
bbox <- get_bounding_box_europe()


# BREAKS ----
vmin <- min(df$`value`, na.rm = TRUE, finite = TRUE)
vmax <- max(df$`value`, na.rm = TRUE, finite = TRUE)
brk <- round(classInt::classIntervals(
  df$`value`,
  n = 6,
  style = "fisher"
)$brks, 1) |>
  head(-1) |>
  tail(-1) |>
  append(vmax)
breaks <- c(vmin, brk)

cols <- rev(c('#800080', '#96308d', '#ab4f99', '#c06ba6', '#d587b2', '#eaa4bf', '#ffc0cb'))

# ANIMATE ----
get_animated_world_map <- function() {
  world_map <- ggplot(
    data = df,
    aes(fill = values)
  ) +
    geom_sf(color = "white", size = 0.0001) +
    geom_sf(data = eu, color = "grey20", size = 0.025, fill = "transparent") +
    geom_sf(
      data = non_eu, color = "grey20", size = 0.025, fill = "white"
    ) +
    scale_fill_gradientn(
      name = "",
      colours = cols,
      breaks = breaks,
      labels = round(breaks, 1),
      limits = c(vmin, vmax),
      na.value = "white"
    ) +
    coord_sf(
      crs = crsLAEA,
      xlim = c(bbox["xmin"], bbox["xmax"]),
      ylim = c(bbox["ymin"], bbox["ymax"])
    ) +
    guides(fill = guide_legend(
      direction = "horizontal",
      keyheight = unit(1.5, units = "mm"),
      keywidth = unit(15, units = "mm"),
      title.position = "top",
      title.hjust = .5,
      label.hjust = .5,
      nrow = 1,
      byrow = T,
      reverse = F,
      label.position = "bottom"
    )) +
    theme_minimal() +
    theme(
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.position = c(.5, -.015),
      legend.text = element_text(size = 11, color = "#25465d"),
      panel.grid.major = element_line(color = "white", linewidth = .002),
      panel.grid.minor = element_blank(),
      plot.title = element_text(
        face = "bold", size = 20,
        color = "#f0951e", hjust = .5, vjust = -1
      ),
      plot.subtitle = element_text(
        size = 20, color = "#00a2d2",
        hjust = .5, vjust = -1
      ),
      plot.caption = element_text( face = "italic",
                                   size = 10, color = "#25465d",
                                   hjust = .5, vjust = -10
      ),
      plot.margin = unit(c(t = 1, r = 1, b = 2, l = 1), "lines"),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = "white", color = NA),
      panel.border = element_blank()
    ) +
    labs(
      x = "",
      y = "",
      title = "Internet purchases of sports goods (Females)",
      subtitle = "Year: {as.integer(current_frame)}",
      caption = "©2023 Sport Business Lab Consultancy\nData: Eurostat"
    )
  
  return(world_map)
}

world_map <- get_animated_world_map()
print(world_map)

timelapse_world_map <- world_map +
  transition_manual(time) +
  enter_grow() +
  ease_aes("quadratic-in-out", interval = .2)


animated_world <- gganimate::animate(
  timelapse_world_map,
  nframes = 120,
  duration = 30,
  start_pause = 1,
  end_pause = 3,
  height = 6,
  width = 7.15,
  res = 300,
  units = "in",
  fps = 15,
  renderer = gifski_renderer(loop = T)
)

gganimate::anim_save(
  "Internet_purchases_sport_goods_females.gif", animated_world
)


# 16 - 24 ----

# get NUTS2-level data
get_eurostat_data <- function() {
  eurostat_df <- eurostat::get_eurostat(
    "isoc_ec_ibgs",
    time_format = "num"
  ) |>
    dplyr::filter(
      unit == "PC_IND" &
        ind_type == "Y16_24" &
        indic_is == "I_BSPG"
    ) |>
    dplyr::select(geo, time, values)
  names(eurostat_df)[1] <- "CNTR_CODE"
  return(eurostat_df)
}

eurostat_df <- get_eurostat_data()
head(eurostat_df)

df <- nuts2 |>
  dplyr::left_join(eurostat_df, by = "CNTR_CODE", relationship = "one-to-many", keep = TRUE)




# LEGEND BREAKS ----
# let's find a natural interval with Jenks breaks
# df$values <- df$values / 1000    # TOGLIERE IL COMMENTO SOLO SE I VALORI SONO MOLTO GRANDI

ni <- classInt::classIntervals(df$values,
                               n = 6,
                               style = "fisher"
)$brks

# this function uses above intervals to create categories
labels <- c()
for (i in 1:length(ni)) {
  labels <- c(labels, paste0(
    round(ni[i], 0),
    "-",
    round(ni[i + 1], 0)
  ))
}
labels <- labels[1:length(labels) - 1]

# finally, carve out the categorical variable
# based on the breaks and labels above
df$cat <- cut(df$values,
              breaks = ni,
              labels = labels,
              include.lowest = T
)
levels(df$cat)

# label NAs, too
lvl <- levels(df$cat)
lvl[length(lvl) + 1] <- "No data"
df$cat <- factor(df$cat, levels = lvl)
df$cat[is.na(df$cat)] <- "No data"
levels(df$cat)

# BOUNDING BOX ----

crsLAEA <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs"
get_bounding_box_europe <- function() {
  xmin <- -10.6600
  xmax <- 40.5500
  ymin <- 28.5000
  ymax <- 85.0500
  
  bbox_laea <- sf::st_sfc(
    sf::st_polygon(list(cbind(
      c(xmin, xmax, xmax, xmin, xmin),
      c(ymin, ymin, ymax, ymax, ymin)
    ))),
    crs = crsLONGLAT
  ) |> sf::st_transform(crsLAEA)
  
  bbox <- sf::st_bbox(bbox_laea)
  return(bbox)
}
bbox <- get_bounding_box_europe()


# BREAKS ----
vmin <- min(df$`value`, na.rm = TRUE, finite = TRUE)
vmax <- max(df$`value`, na.rm = TRUE, finite = TRUE)
brk <- round(classInt::classIntervals(
  df$`value`,
  n = 6,
  style = "fisher"
)$brks, 1) |>
  head(-1) |>
  tail(-1) |>
  append(vmax)
breaks <- c(vmin, brk)

cols <- rev(c('#002400', '#004200', '#006400', '#0c8709', '#42a932', '#68cc54', '#8df075'))

# ANIMATE ----
get_animated_world_map <- function() {
  world_map <- ggplot(
    data = df,
    aes(fill = values)
  ) +
    geom_sf(color = "white", size = 0.0001) +
    geom_sf(data = eu, color = "grey20", size = 0.025, fill = "transparent") +
    geom_sf(
      data = non_eu, color = "grey20", size = 0.025, fill = "white"
    ) +
    scale_fill_gradientn(
      name = "",
      colours = cols,
      breaks = breaks,
      labels = round(breaks, 1),
      limits = c(vmin, vmax),
      na.value = "white"
    ) +
    coord_sf(
      crs = crsLAEA,
      xlim = c(bbox["xmin"], bbox["xmax"]),
      ylim = c(bbox["ymin"], bbox["ymax"])
    ) +
    guides(fill = guide_legend(
      direction = "horizontal",
      keyheight = unit(1.5, units = "mm"),
      keywidth = unit(15, units = "mm"),
      title.position = "top",
      title.hjust = .5,
      label.hjust = .5,
      nrow = 1,
      byrow = T,
      reverse = F,
      label.position = "bottom"
    )) +
    theme_minimal() +
    theme(
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.position = c(.5, -.015),
      legend.text = element_text(size = 11, color = "#25465d"),
      panel.grid.major = element_line(color = "white", linewidth = .002),
      panel.grid.minor = element_blank(),
      plot.title = element_text(
        face = "bold", size = 20,
        color = "#f0951e", hjust = .5, vjust = -1
      ),
      plot.subtitle = element_text(
        size = 20, color = "#00a2d2",
        hjust = .5, vjust = -1
      ),
      plot.caption = element_text( face = "italic",
                                   size = 10, color = "#25465d",
                                   hjust = .5, vjust = -10
      ),
      plot.margin = unit(c(t = 1, r = 1, b = 2, l = 1), "lines"),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = "white", color = NA),
      panel.border = element_blank()
    ) +
    labs(
      x = "",
      y = "",
      title = "Internet purchases of sports goods (AGE 16 - 24)",
      subtitle = "Year: {as.integer(current_frame)}",
      caption = "©2023 Sport Business Lab Consultancy\nData: Eurostat"
    )
  
  return(world_map)
}

world_map <- get_animated_world_map()
print(world_map)

timelapse_world_map <- world_map +
  transition_manual(time) +
  enter_grow() +
  ease_aes("quadratic-in-out", interval = .2)


animated_world <- gganimate::animate(
  timelapse_world_map,
  nframes = 120,
  duration = 30,
  start_pause = 1,
  end_pause = 3,
  height = 6,
  width = 7.15,
  res = 300,
  units = "in",
  fps = 15,
  renderer = gifski_renderer(loop = T)
)

gganimate::anim_save(
  "Internet_purchases_sport_goods_young.gif", animated_world
)

