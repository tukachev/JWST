library(tidyverse)
library(jsonlite)
library(png)
library(glue)
library(ragg)
library(here)
library(showtext)
library(ggdark)
library(ggtext)

# шрифт для графика
font_add_google("Jura", family = "Jura")
showtext_auto()

# получаем данные
api_url <- "https://api.jwst-hub.com/track"
jwst_data <- fromJSON(api_url)
jwst_data <- as_tibble_row(unlist(jwst_data))
# загружаем состояние телескопа в виде картинки
download.file(
  url = gsub(pattern = "Crop", "", jwst_data$deploymentImgURL),
  # url = jwst_data$deploymentImgURL,
  here("jwst_deployment_status.png"),
  mode = 'wb'
)
img <- readPNG(here("jwst_deployment_status.png"))

# преобразуем данные для графика
current_date_time <- lubridate::ymd_hms(jwst_data$timestamp)
current_date_time <-
  stringi::stri_datetime_format(current_date_time,
                                "d MMMM yyyy kk:mm:ss",
                                locale = "ru",
                                tz = "UTC")
# С момента старта
ElapsedTime <-
  glue(
    "<span style = 'font-size:18pt'>**С момента старта**</span><br>
       <span style = 'font-size:24pt;color:#FFD900'>**L+{jwst_data$launchElapsedTime}**</span>"
  )
# Расстояние от Земли
distanceEarth <-
  glue(
    "<span style = 'font-size:18pt'>**Расстояние от Земли**, км</span><br>
       <span style='font-size:24pt;color:#FFD900'>**{formattable::comma(jwst_data$distanceEarthKm)}**</span>"
  )
# До орбиты L2
distanceL2 <-
  glue(
    "<span style = 'font-size:18pt'>**Расстояние до L2**, км</span><br>
       <span style='font-size:24pt;color:#FFD900'>**{formattable::comma(jwst_data$distanceL2Km)}**</span>"
  )
#Завершено
percentageCompleted <-
  glue(
    "<br><span style = 'font-size:18pt'>**{round(as.numeric(jwst_data$percentageCompleted), 1)}%**</span> <span style='font-size:16pt'>завершено</span>"
  )
#текущая крейсерская скорость
speed <-
  glue(
    "<span style = 'font-size:18pt'>**Cкорость**, км/с</span><br>
      <span style='font-size:24pt;color:#FFD900'>**{jwst_data$speedKmS}**</span>"
  )
#температура
red_color <- "#ED3537" #D2292D
blue_color <- "#1494E9" #1761B0
tempSides <-
  glue(
    "<span style = 'font-size:18pt'>**Температура**</span><br>
    <span style = 'font-size:16pt'>Защитного экрана </span><span style='font-size:18pt;color:{red_color}'>**{jwst_data$tempC.tempWarmSide1C}**</span><span style='font-size:18pt'>°C</span><br>
    <span style = 'font-size:16pt'>Панели оборудования </span><span style='font-size:18pt;color:{red_color}'>**{jwst_data$tempC.tempWarmSide2C}**</span></span><span style='font-size:18pt'>°C</span><br>
    <span style = 'font-size:16pt'>Главного зеркала </span><span style='font-size:18pt;color:{blue_color}'>**{jwst_data$tempC.tempCoolSide1C}**</span></span><span style='font-size:18pt'>°C</span><br>
    <span style = 'font-size:16pt'>Радиатора </span><span style='font-size:18pt;color:{blue_color}'>**{jwst_data$tempC.tempCoolSide2C}**</span></span><span style='font-size:18pt'>°C</span>"
  )

ggplot(mapping = aes(1:100, 1:100)) +
  annotation_raster(
    img,
    xmin = -Inf,
    xmax = Inf,
    ymin = -Inf,
    ymax = Inf
  ) +
  geom_blank() +
  geom_richtext(
    aes(-1, 90),
    label = ElapsedTime,
    hjust = 0,
    vjust = 0,
    family = "Jura",
    lineheight = 1.3,
    color = "white",
    fill = NA,
    label.color = NA
  ) +
  geom_richtext(
    aes(-1, 79),
    label = distanceEarth,
    hjust = 0,
    vjust = 0,
    family = "Jura",
    lineheight = 1.3,
    color = "white",
    fill = NA,
    label.color = NA
  ) +
  geom_richtext(
    aes(-1, 63),
    label = paste(distanceL2, percentageCompleted),
    hjust = 0,
    vjust = 0,
    family = "Jura",
    lineheight = 1.3,
    color = "white",
    fill = NA,
    label.color = NA
  ) +
  geom_richtext(
    aes(100, 90),
    label = speed,
    hjust = 1,
    vjust = 0,
    family = "Jura",
    lineheight = 1.3,
    color = "white",
    fill = NA,
    label.color = NA
  ) +
  geom_richtext(
    aes(100, 90),
    label = tempSides,
    hjust = 1,
    vjust = 1,
    family = "Jura",
    lineheight = 1.3,
    color = "white",
    fill = NA,
    label.color = NA
  ) +
  labs(
    title = "Параметры полёта телескопа «Джеймс Уэбб»",
    subtitle =
      glue(
        "Текущий статус на <span style='color:#FFD900'>**{current_date_time} UTC**</span><br>"
      ),
    caption = glue(
      "**Данные:** Public REST API github.com/avatsaev/webb-tracker-api<br>
                   **Визуализация:** Юрий Тукачев, 2022"
    )
  ) +
  dark_theme_void(base_size = 20, base_family = "Jura") +
  theme(
    text = element_text(
      family = "Jura",
      color = "white",
      size = 20
    ),
    panel.background = element_rect(fill = "black"),
    # plot.title.position = "panel",
    # plot.caption.position = "panel",
    plot.caption = element_markdown(
      color = "gray50",
      size = 13,
      hjust = 0
    ),
    plot.margin = margin(10, 10, 5, 10),
    plot.subtitle = element_markdown(
      hjust = 0,
      size = rel(1.2),
      family = "Jura"
    ),
    plot.title = element_markdown(
      size = rel(1.5),
      family = "Jura",
      face = "bold",
      color = "white"
    )
  )

ggsave(
  here("james_webb_status.png"),
  device = agg_png,
  width = 7,
  height = 6,
  dpi = 150
)
