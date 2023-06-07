# EUROSTAT PARTECIPAZIONE EVENTI SPORTIVI E CULTURALI (2006 - 2015)

# LINK VIDEO: https://www.youtube.com/watch?v=8kunLjakT0c

# CARICAMENTO LIBRERIE ----

library(tidyverse)
library(sf)
library(giscoR)
library(classInt)
library(eurostat)

# CARICAMENTO DATI ----
crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# NUTS2 & country shapefile ----

nuts2 <- giscoR::gisco_get_nuts(
  year = "2021",
  resolution = "3",
  nuts_level = "2"
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
# search for datasets with GDP in their title
toc <- eurostat::get_eurostat_toc()
i <- "sport"
tab <- subset(
  toc, grepl(i, title)
)
fix(tab)

## 2015 TOTALE----
# get NUTS2-level data
get_eurostat_data <- function() {
  eurostat_df <- eurostat::get_eurostat(
    "ilc_scp01",
    time_format = "num"
  ) |>
    dplyr::filter(
      time == 2015 &
        sex == "T" &
        age == "Y_GE16" &
        frequenc == "GE1" &
        isced11 == "TOTAL"
    ) |>
    dplyr::select(geo, time, values)
  names(eurostat_df)[1] <- "CNTR_CODE"
  return(eurostat_df)
}

eurostat_df <- get_eurostat_data()
head(eurostat_df)

# convert to wide format
wide_df <- tidyr::pivot_wider(
  eurostat_df,
  names_from = time, values_from = values
)

head(wide_df)

get_enriched_table <- function() {
  enriched_df <- wide_df |>
    dplyr::mutate(values = `2015`) |>
    dplyr::select(CNTR_CODE, values)
  
  # merge shp and data.frame
  df <- nuts2 |>
    dplyr::left_join(enriched_df, by = "CNTR_CODE")
  return(df)
}

df <- get_enriched_table()
head(df)


# LEGEND BREAKS ----
# let's find a natural interval with Jenks breaks
#df$values <- df$values / 1000

ni <- classInt::classIntervals(df$values,
                               n = 6,
                               style = "jenks"
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
  xmax <- 36.5500
  ymin <- 34.5000
  ymax <- 71.0500
  
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

# MAP ----

cols <- c('#f7e028', '#eeb320', '#f0951e', '#c4630e', '#aa3b06', '#8d0000', "#cacaca")

p <- ggplot() +
  geom_sf(
    data = filter(eu, CNTR_ID == "RS"),
    color = "grey20", size = 0.15, fill = "#cacaca"
  ) +
  geom_sf(data = df, aes(fill = cat), color = NA, size = 0) +
  geom_sf(data = eu, color = "grey20", size = 0.125, fill = "transparent") +
  geom_sf(
    data = non_eu, color = "grey20", size = 0.125, fill = "#cacaca"
  ) +
  coord_sf(
    crs = crsLAEA,
    xlim = c(bbox["xmin"], bbox["xmax"]),
    ylim = c(bbox["ymin"], bbox["ymax"])
  ) +
  scale_fill_manual(
    name = "Percentage",
    values = cols,
    drop = F
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
  labs(
    x = "Sport Business Lab Consultancy\nData: Eurostat",
    y = NULL,
    title = "Participation in any cultural or sport activities\nin the last 12 month - 2015",
    subtitle = ""
  ) +
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_text(
      size = 11, color = '#00a2d2',
      hjust = 0.5, vjust = 6
    ),
    axis.title.y = element_blank(),
    legend.position = c(.35, .975),
    legend.text = element_text(
      size = 7, color = "#25465d"
    ),
    legend.title = element_text(
      size = 11, color = "#25465d"
    ),
    legend.spacing.y = unit(0.25, "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(
      face = "bold", size = 15, color = '#00a2d2',
      hjust = .5
    ),
    plot.caption = element_text(
      size = 10, color = "#25465d", hjust = .5,
      vjust = 5
    ),
    plot.margin = unit(
      c(t = 1, r = -2, b = -1, l = -2), "lines"
    ),
    plot.background = element_blank(),
    panel.background = element_blank(),
    legend.background = element_blank(),
    panel.border = element_blank()
  )

ggsave(
  filename = "PARTECIPAZIONE_EVENTI_2015.png",
  width = 7, height = 8.5, dpi = 600,
  device = "png", bg = "white", p
)

## 2015 MASCHI----
# get NUTS2-level data
get_eurostat_data <- function() {
  eurostat_df <- eurostat::get_eurostat(
    "ilc_scp01",
    time_format = "num"
  ) |>
    dplyr::filter(
      time == 2015 &
        sex == "M" &
        age == "Y_GE16" &
        frequenc == "GE1" &
        isced11 == "TOTAL"
    ) |>
    dplyr::select(geo, time, values)
  names(eurostat_df)[1] <- "CNTR_CODE"
  return(eurostat_df)
}

eurostat_df <- get_eurostat_data()
head(eurostat_df)

# convert to wide format
wide_df <- tidyr::pivot_wider(
  eurostat_df,
  names_from = time, values_from = values
)

head(wide_df)

get_enriched_table <- function() {
  enriched_df <- wide_df |>
    dplyr::mutate(values = `2015`) |>
    dplyr::select(CNTR_CODE, values)
  
  # merge shp and data.frame
  df <- nuts2 |>
    dplyr::left_join(enriched_df, by = "CNTR_CODE")
  return(df)
}

df <- get_enriched_table()
head(df)


# LEGEND BREAKS ----
# let's find a natural interval with Jenks breaks
#df$values <- df$values / 1000

ni <- classInt::classIntervals(df$values,
                               n = 6,
                               style = "jenks"
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
  xmax <- 36.5500
  ymin <- 34.5000
  ymax <- 71.0500
  
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

# MAP ----

cols <- c('#82e0ff', '#71bfed', '#5e9edb', '#4a7fca', '#3060b8', '#0043a6', "#cacaca")

p <- ggplot() +
  geom_sf(
    data = filter(eu, CNTR_ID == "RS"),
    color = "grey20", size = 0.15, fill = "#cacaca"
  ) +
  geom_sf(data = df, aes(fill = cat), color = NA, size = 0) +
  geom_sf(data = eu, color = "grey20", size = 0.125, fill = "transparent") +
  geom_sf(
    data = non_eu, color = "grey20", size = 0.125, fill = "#cacaca"
  ) +
  coord_sf(
    crs = crsLAEA,
    xlim = c(bbox["xmin"], bbox["xmax"]),
    ylim = c(bbox["ymin"], bbox["ymax"])
  ) +
  scale_fill_manual(
    name = "Percentage",
    values = cols,
    drop = F
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
  labs(
    x = "Sport Business Lab Consultancy\nData: Eurostat",
    y = NULL,
    title = "Participation in any cultural or sport activities\nin the last 12 month (MALES) - 2015",
    subtitle = ""
  ) +
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_text(
      size = 11, color = "#f0951e",
      hjust = 0.5, vjust = 6
    ),
    axis.title.y = element_blank(),
    legend.position = c(.35, .975),
    legend.text = element_text(
      size = 7, color = "#25465d"
    ),
    legend.title = element_text(
      size = 11, color = "#25465d"
    ),
    legend.spacing.y = unit(0.25, "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(
      face = "bold", size = 20, color = "#f0951e",
      hjust = .5
    ),
    plot.caption = element_text(
      size = 10, color = "#25465d", hjust = .5,
      vjust = 5
    ),
    plot.margin = unit(
      c(t = 1, r = -2, b = -1, l = -2), "lines"
    ),
    plot.background = element_blank(),
    panel.background = element_blank(),
    legend.background = element_blank(),
    panel.border = element_blank()
  )

ggsave(
  filename = "PARTECIPAZIONE_EVENTI_MASCHI_2015.png",
  width = 7, height = 8.5, dpi = 600,
  device = "png", bg = "white", p
)

## 2015 FEMMINE----
# get NUTS2-level data
get_eurostat_data <- function() {
  eurostat_df <- eurostat::get_eurostat(
    "ilc_scp01",
    time_format = "num"
  ) |>
    dplyr::filter(
      time == 2015 &
        sex == "F" &
        age == "Y_GE16" &
        frequenc == "GE1" &
        isced11 == "TOTAL"
    ) |>
    dplyr::select(geo, time, values)
  names(eurostat_df)[1] <- "CNTR_CODE"
  return(eurostat_df)
}

eurostat_df <- get_eurostat_data()
head(eurostat_df)

# convert to wide format
wide_df <- tidyr::pivot_wider(
  eurostat_df,
  names_from = time, values_from = values
)

head(wide_df)

get_enriched_table <- function() {
  enriched_df <- wide_df |>
    dplyr::mutate(values = `2015`) |>
    dplyr::select(CNTR_CODE, values)
  
  # merge shp and data.frame
  df <- nuts2 |>
    dplyr::left_join(enriched_df, by = "CNTR_CODE")
  return(df)
}

df <- get_enriched_table()
head(df)


# LEGEND BREAKS ----
# let's find a natural interval with Jenks breaks
#df$values <- df$values / 1000

ni <- classInt::classIntervals(df$values,
                               n = 6,
                               style = "jenks"
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
  xmax <- 36.5500
  ymin <- 34.5000
  ymax <- 71.0500
  
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

# MAP ----

cols <- c('#ffc6ec', '#ebb8dc', '#d6aacb', '#c39cbb', '#af8eab', '#9c819c', "#cacaca")

p <- ggplot() +
  geom_sf(
    data = filter(eu, CNTR_ID == "RS"),
    color = "grey20", size = 0.15, fill = "#cacaca"
  ) +
  geom_sf(data = df, aes(fill = cat), color = NA, size = 0) +
  geom_sf(data = eu, color = "grey20", size = 0.125, fill = "transparent") +
  geom_sf(
    data = non_eu, color = "grey20", size = 0.125, fill = "#cacaca"
  ) +
  coord_sf(
    crs = crsLAEA,
    xlim = c(bbox["xmin"], bbox["xmax"]),
    ylim = c(bbox["ymin"], bbox["ymax"])
  ) +
  scale_fill_manual(
    name = "Percentage",
    values = cols,
    drop = F
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
  labs(
    x = "Sport Business Lab Consultancy\nData: Eurostat",
    y = NULL,
    title = "Participation in any cultural or sport activities\nin the last 12 month (FEMALES) - 2015",
    subtitle = ""
  ) +
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_text(
      size = 11, color = "#f0951e",
      hjust = 0.5, vjust = 6
    ),
    axis.title.y = element_blank(),
    legend.position = c(.35, .975),
    legend.text = element_text(
      size = 7, color = "#25465d"
    ),
    legend.title = element_text(
      size = 11, color = "#25465d"
    ),
    legend.spacing.y = unit(0.25, "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(
      face = "bold", size = 20, color = "#f0951e",
      hjust = .5
    ),
    plot.caption = element_text(
      size = 10, color = "#25465d", hjust = .5,
      vjust = 5
    ),
    plot.margin = unit(
      c(t = 1, r = -2, b = -1, l = -2), "lines"
    ),
    plot.background = element_blank(),
    panel.background = element_blank(),
    legend.background = element_blank(),
    panel.border = element_blank()
  )

ggsave(
  filename = "PARTECIPAZIONE_EVENTI_FEMMINE_2015.png",
  width = 7, height = 8.5, dpi = 600,
  device = "png", bg = "white", p
)

## 2015 ETA 16 -29----
# get NUTS2-level data
get_eurostat_data <- function() {
  eurostat_df <- eurostat::get_eurostat(
    "ilc_scp01",
    time_format = "num"
  ) |>
    dplyr::filter(
      time == 2015 &
        sex == "T" &
        age == "Y16-29" &
        frequenc == "GE1" &
        isced11 == "TOTAL"
    ) |>
    dplyr::select(geo, time, values)
  names(eurostat_df)[1] <- "CNTR_CODE"
  return(eurostat_df)
}

eurostat_df <- get_eurostat_data()
head(eurostat_df)

# convert to wide format
wide_df <- tidyr::pivot_wider(
  eurostat_df,
  names_from = time, values_from = values
)

head(wide_df)

get_enriched_table <- function() {
  enriched_df <- wide_df |>
    dplyr::mutate(values = `2015`) |>
    dplyr::select(CNTR_CODE, values)
  
  # merge shp and data.frame
  df <- nuts2 |>
    dplyr::left_join(enriched_df, by = "CNTR_CODE")
  return(df)
}

df <- get_enriched_table()
head(df)


# LEGEND BREAKS ----
# let's find a natural interval with Jenks breaks
#df$values <- df$values / 1000

ni <- classInt::classIntervals(df$values,
                               n = 6,
                               style = "jenks"
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
  xmax <- 36.5500
  ymin <- 34.5000
  ymax <- 71.0500
  
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

# MAP ----

cols <- c('#a0ff87', '#71d45c', '#45aa34', '#038202', '#005800', '#003200', "#cacaca")

p <- ggplot() +
  geom_sf(
    data = filter(eu, CNTR_ID == "RS"),
    color = "grey20", size = 0.15, fill = "#cacaca"
  ) +
  geom_sf(data = df, aes(fill = cat), color = NA, size = 0) +
  geom_sf(data = eu, color = "grey20", size = 0.125, fill = "transparent") +
  geom_sf(
    data = non_eu, color = "grey20", size = 0.125, fill = "#cacaca"
  ) +
  coord_sf(
    crs = crsLAEA,
    xlim = c(bbox["xmin"], bbox["xmax"]),
    ylim = c(bbox["ymin"], bbox["ymax"])
  ) +
  scale_fill_manual(
    name = "Percentage",
    values = cols,
    drop = F
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
  labs(
    x = "Sport Business Lab Consultancy\nData: Eurostat",
    y = NULL,
    title = "Participation in any cultural or sport activities\nin the last 12 month (AGE: 16-29)- 2015",
    subtitle = ""
  ) +
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_text(
      size = 11, color = "#f0951e",
      hjust = 0.5, vjust = 6
    ),
    axis.title.y = element_blank(),
    legend.position = c(.35, .975),
    legend.text = element_text(
      size = 7, color = "#25465d"
    ),
    legend.title = element_text(
      size = 11, color = "#25465d"
    ),
    legend.spacing.y = unit(0.25, "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(
      face = "bold", size = 20, color = "#f0951e",
      hjust = .5
    ),
    plot.caption = element_text(
      size = 10, color = "#25465d", hjust = .5,
      vjust = 5
    ),
    plot.margin = unit(
      c(t = 1, r = -2, b = -1, l = -2), "lines"
    ),
    plot.background = element_blank(),
    panel.background = element_blank(),
    legend.background = element_blank(),
    panel.border = element_blank()
  )

ggsave(
  filename = "PARTECIPAZIONE_EVENTI_2015_16-29.png",
  width = 7, height = 8.5, dpi = 600,
  device = "png", bg = "white", p
)


## 2006 ----
# get NUTS2-level data
get_eurostat_data <- function() {
  eurostat_df <- eurostat::get_eurostat(
    "ilc_scp01",
    time_format = "num"
  ) |>
    dplyr::filter(
      time == 2006 &
        sex == "T" &
        age == "Y_GE16" &
        frequenc == "GE1" &
        isced11 == "TOTAL"
    ) |>
    dplyr::select(geo, time, values)
  names(eurostat_df)[1] <- "CNTR_CODE"
  return(eurostat_df)
}

eurostat_df <- get_eurostat_data()
head(eurostat_df)

# convert to wide format
wide_df <- tidyr::pivot_wider(
  eurostat_df,
  names_from = time, values_from = values
)

head(wide_df)

get_enriched_table <- function() {
  enriched_df <- wide_df |>
    dplyr::mutate(values = `2006`) |>
    dplyr::select(CNTR_CODE, values)
  
  # merge shp and data.frame
  df <- nuts2 |>
    dplyr::left_join(enriched_df, by = "CNTR_CODE")
  return(df)
}

df <- get_enriched_table()
head(df)


# LEGEND BREAKS ----
# let's find a natural interval with Jenks breaks
#df$values <- df$values * 100

ni <- classInt::classIntervals(df$values,
                               n = 6,
                               style = "quantile",
                               unique = TRUE
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
  xmax <- 36.5500
  ymin <- 34.5000
  ymax <- 71.0500
  
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

# MAP ----

cols <- c('#f7e028', '#eeb320', '#f0951e', '#c4630e', '#aa3b06', '#8d0000', "#cacaca")

p <- ggplot() +
  geom_sf(
    data = filter(eu, CNTR_ID == "RS"),
    color = "grey20", size = 0.15, fill = "#cacaca"
  ) +
  geom_sf(data = df, aes(fill = cat), color = NA, size = 0) +
  geom_sf(data = eu, color = "grey20", size = 0.125, fill = "transparent") +
  geom_sf(
    data = non_eu, color = "grey20", size = 0.125, fill = "#cacaca"
  ) +
  coord_sf(
    crs = crsLAEA,
    xlim = c(bbox["xmin"], bbox["xmax"]),
    ylim = c(bbox["ymin"], bbox["ymax"])
  ) +
  scale_fill_manual(
    name = "Percentage",
    values = cols,
    drop = F
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
  labs(
    x = "Sport Business Lab Consultancy\nData: Eurostat",
    y = NULL,
    title = "Participation in any cultural or sport activities\nin the last 12 month - 2006",
    subtitle = ""
  ) +
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_text(
      size = 11, color = '#00a2d2',
      hjust = 0.5, vjust = 6
    ),
    axis.title.y = element_blank(),
    legend.position = c(.35, .975),
    legend.text = element_text(
      size = 7, color = "#25465d"
    ),
    legend.title = element_text(
      size = 11, color = "#25465d"
    ),
    legend.spacing.y = unit(0.25, "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(
      face = "bold", size = 15, color = '#00a2d2',
      hjust = .5
    ),
    plot.caption = element_text(
      size = 10, color = "#25465d", hjust = .5,
      vjust = 5
    ),
    plot.margin = unit(
      c(t = 1, r = -2, b = -1, l = -2), "lines"
    ),
    plot.background = element_blank(),
    panel.background = element_blank(),
    legend.background = element_blank(),
    panel.border = element_blank()
  )

ggsave(
  filename = "PARTECIPAZIONE_EVENTI_2006.png",
  width = 7, height = 8.5, dpi = 600,
  device = "png", bg = "white", p
)


## 2006 MASCHI----
# get NUTS2-level data
get_eurostat_data <- function() {
  eurostat_df <- eurostat::get_eurostat(
    "ilc_scp01",
    time_format = "num"
  ) |>
    dplyr::filter(
      time == 2006 &
        sex == "M" &
        age == "Y_GE16" &
        frequenc == "GE1" &
        isced11 == "TOTAL"
    ) |>
    dplyr::select(geo, time, values)
  names(eurostat_df)[1] <- "CNTR_CODE"
  return(eurostat_df)
}

eurostat_df <- get_eurostat_data()
head(eurostat_df)

# convert to wide format
wide_df <- tidyr::pivot_wider(
  eurostat_df,
  names_from = time, values_from = values
)

head(wide_df)

get_enriched_table <- function() {
  enriched_df <- wide_df |>
    dplyr::mutate(values = `2006`) |>
    dplyr::select(CNTR_CODE, values)
  
  # merge shp and data.frame
  df <- nuts2 |>
    dplyr::left_join(enriched_df, by = "CNTR_CODE")
  return(df)
}

df <- get_enriched_table()
head(df)


# LEGEND BREAKS ----
# let's find a natural interval with Jenks breaks
#df$values <- df$values / 1000

ni <- classInt::classIntervals(df$values,
                               n = 6,
                               style = "quantile"
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
  xmax <- 36.5500
  ymin <- 34.5000
  ymax <- 71.0500
  
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

# MAP ----

cols <- c('#82e0ff', '#71bfed', '#5e9edb', '#4a7fca', '#3060b8', '#0043a6', "#cacaca")

p <- ggplot() +
  geom_sf(
    data = filter(eu, CNTR_ID == "RS"),
    color = "grey20", size = 0.15, fill = "#cacaca"
  ) +
  geom_sf(data = df, aes(fill = cat), color = NA, size = 0) +
  geom_sf(data = eu, color = "grey20", size = 0.125, fill = "transparent") +
  geom_sf(
    data = non_eu, color = "grey20", size = 0.125, fill = "#cacaca"
  ) +
  coord_sf(
    crs = crsLAEA,
    xlim = c(bbox["xmin"], bbox["xmax"]),
    ylim = c(bbox["ymin"], bbox["ymax"])
  ) +
  scale_fill_manual(
    name = "Percentage",
    values = cols,
    drop = F
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
  labs(
    x = "Sport Business Lab Consultancy\nData: Eurostat",
    y = NULL,
    title = "Participation in any cultural or sport activities\nin the last 12 month (MALES) - 2006",
    subtitle = ""
  ) +
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_text(
      size = 11, color = "#f0951e",
      hjust = 0.5, vjust = 6
    ),
    axis.title.y = element_blank(),
    legend.position = c(.35, .975),
    legend.text = element_text(
      size = 7, color = "#25465d"
    ),
    legend.title = element_text(
      size = 11, color = "#25465d"
    ),
    legend.spacing.y = unit(0.25, "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(
      face = "bold", size = 20, color = "#f0951e",
      hjust = .5
    ),
    plot.caption = element_text(
      size = 10, color = "#25465d", hjust = .5,
      vjust = 5
    ),
    plot.margin = unit(
      c(t = 1, r = -2, b = -1, l = -2), "lines"
    ),
    plot.background = element_blank(),
    panel.background = element_blank(),
    legend.background = element_blank(),
    panel.border = element_blank()
  )

ggsave(
  filename = "PARTECIPAZIONE_EVENTI_MASCHI_2006.png",
  width = 7, height = 8.5, dpi = 600,
  device = "png", bg = "white", p
)

## 2006 FEMMINE----
# get NUTS2-level data
get_eurostat_data <- function() {
  eurostat_df <- eurostat::get_eurostat(
    "ilc_scp01",
    time_format = "num"
  ) |>
    dplyr::filter(
      time == 2006 &
        sex == "F" &
        age == "Y_GE16" &
        frequenc == "GE1" &
        isced11 == "TOTAL"
    ) |>
    dplyr::select(geo, time, values)
  names(eurostat_df)[1] <- "CNTR_CODE"
  return(eurostat_df)
}

eurostat_df <- get_eurostat_data()
head(eurostat_df)

# convert to wide format
wide_df <- tidyr::pivot_wider(
  eurostat_df,
  names_from = time, values_from = values
)

head(wide_df)

get_enriched_table <- function() {
  enriched_df <- wide_df |>
    dplyr::mutate(values = `2006`) |>
    dplyr::select(CNTR_CODE, values)
  
  # merge shp and data.frame
  df <- nuts2 |>
    dplyr::left_join(enriched_df, by = "CNTR_CODE")
  return(df)
}

df <- get_enriched_table()
head(df)


# LEGEND BREAKS ----
# let's find a natural interval with Jenks breaks
#df$values <- df$values / 1000

ni <- classInt::classIntervals(df$values,
                               n = 6,
                               style = "quantile"
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
  xmax <- 36.5500
  ymin <- 34.5000
  ymax <- 71.0500
  
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

# MAP ----

cols <- c('#ffc6ec', '#ebb8dc', '#d6aacb', '#c39cbb', '#af8eab', '#9c819c', "#cacaca")

p <- ggplot() +
  geom_sf(
    data = filter(eu, CNTR_ID == "RS"),
    color = "grey20", size = 0.15, fill = "#cacaca"
  ) +
  geom_sf(data = df, aes(fill = cat), color = NA, size = 0) +
  geom_sf(data = eu, color = "grey20", size = 0.125, fill = "transparent") +
  geom_sf(
    data = non_eu, color = "grey20", size = 0.125, fill = "#cacaca"
  ) +
  coord_sf(
    crs = crsLAEA,
    xlim = c(bbox["xmin"], bbox["xmax"]),
    ylim = c(bbox["ymin"], bbox["ymax"])
  ) +
  scale_fill_manual(
    name = "Percentage",
    values = cols,
    drop = F
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
  labs(
    x = "Sport Business Lab Consultancy\nData: Eurostat",
    y = NULL,
    title = "Participation in any cultural or sport activities\nin the last 12 month (FEMALES) - 2006",
    subtitle = ""
  ) +
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_text(
      size = 11, color = "#f0951e",
      hjust = 0.5, vjust = 6
    ),
    axis.title.y = element_blank(),
    legend.position = c(.35, .975),
    legend.text = element_text(
      size = 7, color = "#25465d"
    ),
    legend.title = element_text(
      size = 11, color = "#25465d"
    ),
    legend.spacing.y = unit(0.25, "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(
      face = "bold", size = 20, color = "#f0951e",
      hjust = .5
    ),
    plot.caption = element_text(
      size = 10, color = "#25465d", hjust = .5,
      vjust = 5
    ),
    plot.margin = unit(
      c(t = 1, r = -2, b = -1, l = -2), "lines"
    ),
    plot.background = element_blank(),
    panel.background = element_blank(),
    legend.background = element_blank(),
    panel.border = element_blank()
  )

ggsave(
  filename = "PARTECIPAZIONE_EVENTI_FEMMINE_2006.png",
  width = 7, height = 8.5, dpi = 600,
  device = "png", bg = "white", p
)

## 2015 ETA 16 -29----
# get NUTS2-level data
get_eurostat_data <- function() {
  eurostat_df <- eurostat::get_eurostat(
    "ilc_scp01",
    time_format = "num"
  ) |>
    dplyr::filter(
      time == 2006 &
        sex == "T" &
        age == "Y16-29" &
        frequenc == "GE1" &
        isced11 == "TOTAL"
    ) |>
    dplyr::select(geo, time, values)
  names(eurostat_df)[1] <- "CNTR_CODE"
  return(eurostat_df)
}

eurostat_df <- get_eurostat_data()
head(eurostat_df)

# convert to wide format
wide_df <- tidyr::pivot_wider(
  eurostat_df,
  names_from = time, values_from = values
)

head(wide_df)

get_enriched_table <- function() {
  enriched_df <- wide_df |>
    dplyr::mutate(values = `2006`) |>
    dplyr::select(CNTR_CODE, values)
  
  # merge shp and data.frame
  df <- nuts2 |>
    dplyr::left_join(enriched_df, by = "CNTR_CODE")
  return(df)
}

df <- get_enriched_table()
head(df)


# LEGEND BREAKS ----
# let's find a natural interval with Jenks breaks
#df$values <- df$values / 1000

ni <- classInt::classIntervals(df$values,
                               n = 6,
                               style = "quantile"
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
  xmax <- 36.5500
  ymin <- 34.5000
  ymax <- 71.0500
  
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

# MAP ----

cols <- c('#a0ff87', '#71d45c', '#45aa34', '#038202', '#005800', '#003200', "#cacaca")

p <- ggplot() +
  geom_sf(
    data = filter(eu, CNTR_ID == "RS"),
    color = "grey20", size = 0.15, fill = "#cacaca"
  ) +
  geom_sf(data = df, aes(fill = cat), color = NA, size = 0) +
  geom_sf(data = eu, color = "grey20", size = 0.125, fill = "transparent") +
  geom_sf(
    data = non_eu, color = "grey20", size = 0.125, fill = "#cacaca"
  ) +
  coord_sf(
    crs = crsLAEA,
    xlim = c(bbox["xmin"], bbox["xmax"]),
    ylim = c(bbox["ymin"], bbox["ymax"])
  ) +
  scale_fill_manual(
    name = "Percentage",
    values = cols,
    drop = F
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
  labs(
    x = "Sport Business Lab Consultancy\nData: Eurostat",
    y = NULL,
    title = "Participation in any cultural or sport activities\nin the last 12 month (AGE: 16-29)- 2006",
    subtitle = ""
  ) +
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_text(
      size = 11, color = "#f0951e",
      hjust = 0.5, vjust = 6
    ),
    axis.title.y = element_blank(),
    legend.position = c(.35, .975),
    legend.text = element_text(
      size = 7, color = "#25465d"
    ),
    legend.title = element_text(
      size = 11, color = "#25465d"
    ),
    legend.spacing.y = unit(0.25, "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(
      face = "bold", size = 20, color = "#f0951e",
      hjust = .5
    ),
    plot.caption = element_text(
      size = 10, color = "#25465d", hjust = .5,
      vjust = 5
    ),
    plot.margin = unit(
      c(t = 1, r = -2, b = -1, l = -2), "lines"
    ),
    plot.background = element_blank(),
    panel.background = element_blank(),
    legend.background = element_blank(),
    panel.border = element_blank()
  )

ggsave(
  filename = "PARTECIPAZIONE_EVENTI_2006_16-29.png",
  width = 7, height = 8.5, dpi = 600,
  device = "png", bg = "white", p
)
