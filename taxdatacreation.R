
library("dplyr")
library("tidyverse")
library("readxl")
library("ggplot2")
library("ggstatsplot")
library("scales")
pin <- read_excel("/Users/jc/Desktop/three.xlsx")
d <- as.data.frame(pin)
d[is.na(d)] <- 0

d22 <-filter(d, d$year =="2022")


names(d22)
by(d22, d22$neighborhood, summary)



ggplot(d22) +
  aes(x = d22$address
      , y = d22$perSqFt
      , color=neighborhood) +
  geom_point() +
  scale_color_hue()





names(d)
mve <- d[d$neighborhood %in% c("MISSION VALLEY EST"),]


mve<-  as.data.frame(mve)

mve <- mve %>%
  mutate(marketValPerAcre = mve$land_market /  mve$land_acres) %>%
  mutate(valPerSqft = mve$improvement_value / mve$living_area )


d <- d %>%
  mutate(marketValPerAcre = d$land_market /  d$land_acres) %>%
  mutate(valPerSqft = d$improvement_value / d$living_area )




grouped_ggdotplotstats(
  data       = dplyr::filter(d, neighborhood == "MISSION VALLEY EST", year %in% c("2022", "2021", "2020", "2019")),
  y          = address,
  x          = marketValPerAcre,
  grouping.var = year,
  type       = "robust",
  xlab       = "Market Price",
  
) + scale_x_continuous(labels = comma)



grouped_ggdotplotstats(
  data       = dplyr::filter(d, neighborhood == "MISSION VALLEY EST", year %in% c("2022", "2021", "2020", "2019")),
  y          = address,
  x          = valPerSqft,
  grouping.var = year,
  type       = "robust",
  xlab       = "MarketValue"
) + scale_x_continuous(labels = scales::comma)




grouped_ggdotplotstats(
  data       = dplyr::filter(d, neighborhood == "THE PINNACLE", year =="2022"),
  y          = address,
  x          = marketValPerAcre,
  grouping.var = year,
  type       = "robust",
  xlab       = "Market Price"
)+ scale_x_continuous(labels = comma)

