# Круговая диаграмма пакетом tidyverse // Circular Barplot 

# ЧАСТЬ-1. готовим датафрейм. 
	# шаг-1. вчитываем таблицу с данными. делаем из нее исходный датафрейм. чистим датафрейм от NA
MDepths <- read.csv("Morphology.csv", header=TRUE, sep = ",")
MDF <- na.omit(MDepths) 
row.has.na <- apply(MDF, 1, function(x){any(is.na(x))}) 
sum(row.has.na) 
head(MDF)

library(tidyverse)
	# шаг-2  Create data / создаем короткий датафрейм всего из 3 значений (value - длина лепестка круга)
data<- data.frame(x = MDF$profile, y = MDF$tg_angle)

# неотсортированный график с разношерстыми леденцами
p_unsort<- ggplot(data, aes(x=x, y=y)) +
   geom_segment( aes(x=x, xend=x, y=0, yend=y), color="skyblue", size=0.5) +
   geom_point( color="slateblue1", size=4) +
   coord_flip() +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank(),
	axis.title.y = element_text(margin = margin(t = 20, r = 3), family = "Times New Roman", face = 1, size = 10),
	axis.title.x = element_text(family = "Times New Roman", face = 1, size = 10, margin = margin(t = .2)),
  ) +
  xlab("Profile Nr.") +
 ylab(expression(tg*degree*(A/H)))
p_unsort

# рисуем скобки дял групп
library(ggsignif)

# Reorder теперь сортируем леденцы
p_sort <- data %>%
  arrange(y) %>%
  mutate(x=factor(x,x)) %>%
  ggplot( aes(x=x, y=y)) +
    geom_segment( aes(x=x, xend=x, y=0, yend=y), color="skyblue", size=0.5) +
	geom_segment( aes(x=x, xend=x, y=0, yend=y ), color=ifelse(data$x %in% c("4", "10", "16", "21"), "orange", "skyblue"), size=ifelse(data$x %in% c("4", "10", "16", "21"), 1.3, 0.5) ) +
	geom_point( color=ifelse(data$x %in% c("4", "10", "16", "21"), "orange", "grey"), size=ifelse(data$x %in% c("4", "10", "16", "21"),  5,  2) ) +
    geom_point( color="slateblue1", size=4, alpha=0.6) +
    geom_signif(comparisons = list(c("21", "20")), annotation="strong \nslope", map_signif_level=TRUE, vjust = 1.3, textsize = 3.0, family = "Times New Roman") +
    geom_signif(comparisons = list(c("20", "2")), annotation="very strong \nslope", map_signif_level=TRUE, vjust = 1.3, textsize = 3.0, family = "Times New Roman") +
    geom_signif(comparisons = list(c("2", "13")), annotation="extreme \nslope", map_signif_level=TRUE, vjust = 1.3, textsize = 3.0, family = "Times New Roman") +
    geom_signif(comparisons = list(c("13", "3")), annotation="steep \nslope", map_signif_level=TRUE, vjust = 1.3, textsize = 3.0, family = "Times New Roman") +
    geom_signif(comparisons = list(c("3", "24")), annotation="very steep \nslope", map_signif_level=TRUE, vjust = 0.5, textsize = 3.0, family = "Times New Roman") +
    theme_light() +
 #   coord_flip() +
    theme(
	panel.grid.major.y = element_blank(),
	panel.border = element_blank(),
	axis.ticks.y = element_blank(),
	axis.title.y = element_text(margin = margin(t = 20, r = 3), family = "Times New Roman", face = 1, size = 10),
	axis.title.x = element_text(family = "Times New Roman", face = 1, size = 10, margin = margin(t = .2)),
    ) +
  xlab("Profile Nr.") +
  ylab(expression(tg*degree*(A/H)))
 p_sort
 

 
 figure <-plot_grid(p_unsort, p_sort, labels = c("1", "2"), ncol = 2, nrow = 1)
 figure
 
	# шаг-11. добавляем к ним общий заголовок, подзаголовок и нижнюю сноску.
Ranking <- figure +						
	labs(title="马里亚纳海沟。剖面1-25。Mariana Trench, Profiles Nr.1-25.", 
	subtitle = "统计图表。地貌聚类分析, 条形图。Geomorphological Analysis: Ranking of Profiles by Angle Steepness (Left: Unsorted; Right: Sorted and Grouped)",
	caption = "Statistics Processing and Graphs: R Programming. Data Source: QGIS") +
	theme(
		plot.margin = margin(5, 10, 20, 5),
		plot.title = element_text(margin = margin(t = 0, r = 20, b = 5, l = 0), family = "Kai", face = "bold", size = 12), # китайский шрифт "Кай"
		plot.subtitle = element_text(margin = margin(t = 0, r = 20, b = 4, l = 0), family = "Hei", face = "bold", size = 10), # китайский шрифт "Хэй"
		plot.caption = element_text(face = 2, size = 8),
		panel.background=ggplot2::element_rect(fill = "white"),
		legend.justification = "bottom", 
		legend.position = "bottom",
		legend.box.just = "right",
		legend.direction = "horizontal",
		legend.box = "horizontal",
		legend.box.background = element_rect(colour = "honeydew4",size=0.2),
		legend.background = element_rect(fill = "white"),
		legend.key.width = unit(1,"cm"),
		legend.key.height = unit(.5,"cm"),
		legend.spacing.x = unit(.2,"cm"),
		legend.spacing.y = unit(.1,"cm"),
		legend.text = element_text(colour="black", size=6, face=1),
		legend.title = element_text(colour="black", size=6, face=1))
Ranking
