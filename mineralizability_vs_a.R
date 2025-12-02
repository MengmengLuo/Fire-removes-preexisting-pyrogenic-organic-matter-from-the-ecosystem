###fit mineralizibility with a 

# load libraries
library(dplyr)
library(ggplot2)
library(multcompView)
library(tidyverse)

minC = read.csv("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/Incubation_Cmin/derived/incubation.all.new.csv")
decay = read.csv("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/Incubation_Cmin/derived/decay.model.all.stats.csv")
minC <- minC %>% filter(week == 12)
minC = minC[,-c(3,4)]

df <- merge(minC, decay, by = "SampleID")

lim = lm(mineralizability~a,data=df)
# Inspect results
summary(lim)

lim.Intercept = as.numeric(lim$coefficients[1])
lim.Slope = as.numeric(lim$coefficients[2])

df$Depth = factor(df$Depth, levels = c('Surface','1 cm', '5 cm'), ordered = TRUE)
df$HF = factor(df$HF, levels = c('High','Low', 'Control'), ordered = TRUE)

df %>%
  ggplot()+
  geom_point(aes(x=a,y=mineralizability,fill=Depth, color = Depth, shape=HF))+
  theme_bw()+
  geom_abline(intercept=lim.Intercept,slope=lim.Slope)+
  scale_shape_manual(values = c(24, 21, 23))+
  scale_fill_manual(values = c('#a62445','#d4ad2c', '#2d8ab3'))+
  scale_color_manual(values = c('#a62445','#d4ad2c', '#2d8ab3'))+
  labs(x = expression("Readily Decomposable C Fraction (Coefficient " * italic(a) * ")"), y = bquote('Mineralizability (mg C'*O[2]*'-C '*mg^-1 ~ C*')'), color = "Depth", shape = "Heat Flux")+
  theme(legend.title=element_text(size=12),
        legend.text=element_text(size=10),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))+
  theme_bw() +
  theme(panel.background = element_rect(colour = "black", size=1),
        legend.key = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 0.045))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.03))

#ggsave("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/Incubation_Cmin/min.vs.a.new.tiff", units="in", width=5, height=5)

