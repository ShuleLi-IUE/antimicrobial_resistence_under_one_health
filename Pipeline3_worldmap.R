library(spData)
library(tmap)
data("World")
# tm_shape(World)
World %>% View()
world %>% View()

data("world", package = "spData")
qtm(world,  ## 使用人均GDP作为填充颜色
    fill = "gdpPercap",fill.palette = "div",fill.n = 5)

anti_df <- read.csv("/Users/lishule/Documents/Papers/list/total_antibiotic_consumption_estimates.csv", check.names = F) %>% 
  group_by(Location) %>%
  summarize(`Antibiotic consumption (DDD/1,000/day)` = mean(`Antibiotic consumption (DDD/1,000/day)`))
risk_score <- read.csv("/Users/lishule/Documents/namespace/ARG_surveillance/nlptask/UN/country_risk_score_3_1.csv", check.names = F)

world_m <- world %>% left_join(anti_df, by = c("name_long" = "Location")) %>% left_join(risk_score, c("name_long" = "Name"))

tmap_style("natural")
tm <- tm_shape(world_m, is.master = TRUE) +
  # tm_grid(x = seq(-180, 180, by=20), y=seq(-90,90,by=10), 
  #         lwd = 0.5,col = "gray80") +
  # tm_borders(col = "black", lwd = 0.7) +
  tm_polygons(col = "Antibiotic consumption (DDD/1,000/day)", n = 4,
          style = "pretty",
          palette = "Blues",
          colorNA = "white", 
          alpha = 1,
          legend.show = TRUE, 
          # legend.hist = F,
          title = "Antibiotic consumption\n(DDD/1,000/day)") +
  tm_bubbles(size = "risk_score_3_1", 
             # col = "risk_score_3_1", 
             col = "firebrick2",
             alpha = 0.85, 
             palette = "YlOrRd", breaks = c(-Inf, 500, 10000, 15000, Inf),
             interval.closure = "right",
             # title = "Co-shared risk"
             ) +
  tm_style("natural", frame = FALSE, earth.boundary = c(-180, -87, 180, 87), bg.color = "gray95") +
  tm_scale_bar(text.size = 0.5, position = c(0.55, 0.04), lwd = 1) +
  tm_compass(position = c(0.02, 0.89), type = "arrow") +
  tm_legend(position = c("left", "bottom"),
            bg.color = "gray95",
            frame = FALSE)
tmap_save(tm, filename = "./UN/world.pdf", dpi = 600)  

world_m_c <- world_m %>% filter(name_long != "United States")
cor.test(world_m$`Antibiotic consumption (DDD/1,000/day)`, world_m$risk_score_3_1, method = "spearman")

plot(world_m$`Antibiotic consumption (DDD/1,000/day)`, world_m$risk_score_3_1)
library(ggplot2)
p <- ggplot(world_m, aes(x = `Antibiotic consumption (DDD/1,000/day)`, y = risk_score_3_1)) +
  geom_point()
ggsave(p, height = 8, width = 8, filename = "./UN/point.pdf")
