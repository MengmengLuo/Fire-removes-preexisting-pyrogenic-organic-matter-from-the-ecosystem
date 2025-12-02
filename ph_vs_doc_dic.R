ph = read.csv("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/pH/data/derived/pH_saturation.csv")
ph = ph[,-c(3:8)]
ph = na.omit(ph)
doc = read.csv("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/DOC/derived/doc.csv")

df <- cbind(ph, doc)

# Load necessary library
library(ggplot2)
library(patchwork)

# Fit models
m.ph = lm(pH ~ mg_doc_per_sample, data = df)
m.ph.ic = lm(pH ~ mg_ic_per_sample, data = df, na.action = na.exclude)

# Store coefficients
m.ph.Intercept = coef(m.ph)[1]
m.ph.Slope = coef(m.ph)[2]

m.ph.ic.Intercept = coef(m.ph.ic)[1]
m.ph.ic.Slope = coef(m.ph.ic)[2]

# Ensure factors are ordered
df$Depth = factor(df$Depth, levels = c('Surface', '1 cm', '5 cm'), ordered = TRUE)
df$HF = factor(df$HF, levels = c('High', 'Low', 'Control'), ordered = TRUE)

# First plot: pH vs DOC
p1 <- ggplot(df) +
  geom_point(aes(x = mg_doc_per_sample, y = pH, fill = Depth, color = Depth, shape = HF)) +
  geom_abline(intercept = m.ph.Intercept, slope = m.ph.Slope) +
  scale_shape_manual(values = c(24, 21, 23)) +
  scale_fill_manual(values = c('#a62445','#d4ad2c', '#2d8ab3')) +
  scale_color_manual(values = c('#a62445','#d4ad2c', '#2d8ab3')) +
  labs(x = "DOC (mg per sample)", y = "pH", color = "Depth", shape = "Heat Flux") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        panel.background = element_rect(colour = "black", size = 1),
        legend.key = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 8))

# Second plot: pH vs DIC
p2 <- ggplot(df) +
  geom_point(aes(x = mg_ic_per_sample, y = pH, fill = Depth, color = Depth, shape = HF)) +
  geom_abline(intercept = m.ph.ic.Intercept, slope = m.ph.ic.Slope) +
  scale_shape_manual(values = c(24, 21, 23)) +
  scale_fill_manual(values = c('#a62445','#d4ad2c', '#2d8ab3')) +
  scale_color_manual(values = c('#a62445','#d4ad2c', '#2d8ab3')) +
  labs(x = "DIC (mg per sample)", y = "pH", color = "Depth", shape = "Heat Flux") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        panel.background = element_rect(colour = "black", size = 1),
        legend.key = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1.02))


library(ggeasy)
library(ggpubr)
library(grid)
library(gridExtra)

grid.arrange(p1, p2, ncol = 2)

ggarrange(p1, p2,
          common.legend = TRUE, legend = "bottom")

summary(m.ph)
summary(m.ph.ic)

#ggsave("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/DOC/doc_dic_ph_lms_new.tiff",units="in", width=8, height=5)


###combine DOC-CO2-C plot
minC = read.csv("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/Incubation_Cmin/derived/incubation.all.new.csv")
doc = read.csv("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/DOC/derived/doc.csv")
minC <- minC %>% filter(week == 12)
minC = minC[,-c(3,4)]

df <- cbind(minC, doc)
df <- df[df$SampleID != 1, ] #take out the one outlier (5.42 SD from the trt)

any(duplicated(names(df)))
names(df)[duplicated(names(df))]
df <- df[, !duplicated(names(df))]

lim = lm(mg_CO2C_per_sample~mg_doc_per_sample,data=df)
# Inspect results
summary(lim)

lim.Intercept = as.numeric(lim$coefficients[1])
lim.Slope = as.numeric(lim$coefficients[2])

df$Depth = factor(df$Depth, levels = c('Surface','1 cm', '5 cm'), ordered = TRUE)
df$HF = factor(df$HF, levels = c('High','Low', 'Control'), ordered = TRUE)

p3 = ggplot(df) +
  geom_point(aes(x = mg_doc_per_sample, y = mg_CO2C_per_sample, fill = Depth, color = Depth, shape = HF)) +
  geom_abline(intercept = lim.Intercept, slope = lim.Slope) +
  scale_shape_manual(values = c(24, 21, 23)) +
  scale_fill_manual(values = c('#a62445','#d4ad2c', '#2d8ab3')) +
  scale_color_manual(values = c('#a62445','#d4ad2c', '#2d8ab3')) +
  labs(x = "DOC (mg per sample)",
       y = bquote('C'*O[2]*'-C (mg per sample)'),
       color = "Depth",
       shape = "Heat Flux") +
  theme_bw() +  # Put it here first
  theme(
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.background = element_rect(colour = "black", size = 1),
    legend.key = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 8))

ggarrange(p3, p1, p2,
          ncol = 3, nrow = 1,
          common.legend = TRUE,
          legend = "bottom")

#ggsave("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/DOC/doc_dic_ph_co2c_lms_new.tiff",units="in", width=12, height=5)
