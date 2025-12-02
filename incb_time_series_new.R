#content
#create the new graph for the CO2-C for 12 weeks
#create the new graph for mineralizability
#produce the time series dataset for model fitting

# load libraries
library(dplyr)
library(ggplot2)
library(multcompView)
library(tidyverse)

carbon = read.csv("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/C_N/data/derived/for_DOC&CO2C_anal.csv")

#####2wk, cumulative
incb2_2wk = read.csv("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/Incubation_Cmin/derived/Samples_incubation_rep2_2wk.csv")
##calculate blank
blank.2wk = (incb2_2wk$C.CO2_g[16]+incb2_2wk$C.CO2_g[32] +incb2_2wk$C.CO2_g[48])/3
##rm blank
incb2_2wk_noblank = incb2_2wk[ -c(16, 32, 48:51),]
#preparing to merge
df1 = incb2_2wk_noblank[, -c(2,3,6,7,8,10:18,20,21)]
df1$week = paste(2)
colnames(df1)[colnames(df1) == "C.CO2_g"] ="C_loss_as_CO2"
#subtract blank
df1$C_loss_as_CO2 = df1$C_loss_as_CO2 - blank.2wk
df1$mg_C_per_sample = paste(carbon$mg_C_per_sample)
df1$g_of_sample_mass = paste(carbon$final.sample.mass)


######4wk, cumulative
incb2_4wk = read.csv("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/Incubation_Cmin/derived/Samples_incubation_rep2_4wk.csv")
##calculate blank
blank.4wk = (incb2_4wk$C.CO2_g[16]+incb2_4wk$C.CO2_g[32] +incb2_4wk$C.CO2_g[48])/3
##rm blank
incb2_4wk_noblank = incb2_4wk[ -c(16, 32, 48:51),]
#subtract blank
incb2_4wk_noblank$C.CO2_g = incb2_4wk_noblank$C.CO2_g - blank.4wk
#combine 4 weeks
incb2_4wk_noblank$C.CO2_g = incb2_4wk_noblank$C.CO2_g + df1$C_loss_as_CO2
#preparing to merge
df2 = incb2_4wk_noblank[, -c(2,3,6,7,8,10:18,20,21)]
df2$week = paste(4)
colnames(df2)[colnames(df2) == "C.CO2_g"] ="C_loss_as_CO2"
df2$mg_C_per_sample = paste(carbon$mg_C_per_sample)
df2$g_of_sample_mass = paste(carbon$final.sample.mass)


#####8wk, cumulative
incb2_8wk = read.csv("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/Incubation_Cmin/derived/Samples_incubation_rep2_8wk.csv")
##calculate blank
blank.8wk = (incb2_8wk$C.CO2_g[16]+incb2_8wk$C.CO2_g[32] +incb2_8wk$C.CO2_g[48])/3
##rm blank
incb2_8wk_noblank = incb2_8wk[ -c(16, 32, 48:51),]
#subtract blank
incb2_8wk_noblank$C.CO2_g = incb2_8wk_noblank$C.CO2_g - blank.8wk
#combine 8 weeks
incb2_8wk_noblank$C.CO2_g = incb2_8wk_noblank$C.CO2_g + df2$C_loss_as_CO2
#preparing to merge
df3 = incb2_8wk_noblank[, -c(2,3,6,7,8,10:18,20,21)]
df3$week = paste(8)
colnames(df3)[colnames(df3) == "C.CO2_g"] ="C_loss_as_CO2"
df3$mg_C_per_sample = paste(carbon$mg_C_per_sample)
df3$g_of_sample_mass = paste(carbon$final.sample.mass)

#####12wk, cumulative 
incb2_12wk = read.csv("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/Incubation_Cmin/derived/Samples_incubation_rep2_12wk.csv")
##calculate blank
blank.12wk = (incb2_12wk$C.CO2_g[16]+incb2_12wk$C.CO2_g[32] +incb2_12wk$C.CO2_g[48])/3
##rm blank
incb2_12wk_noblank = incb2_12wk[ -c(16, 32, 48:51),]
#subtract blank
incb2_12wk_noblank$C.CO2_g = incb2_12wk_noblank$C.CO2_g - blank.12wk
#combine 12 weeks
incb2_12wk_noblank$C.CO2_g = incb2_12wk_noblank$C.CO2_g + df3$C_loss_as_CO2
#preparing to merge
df4 = incb2_12wk_noblank[, -c(2,3,6,7,8,10:18,20,21)]
df4$week = paste(12)
colnames(df4)[colnames(df4) == "C.CO2_g"] ="C_loss_as_CO2"
df4$mg_C_per_sample = paste(carbon$mg_C_per_sample)
df4$g_of_sample_mass = paste(carbon$final.sample.mass)


#####0wk
# for week 0, the fraction of C remaining is all 1
df0 = df1[, -c(5, 6)] 
df0$C_loss_as_CO2 = 0
df0$week = 0

#write.csv(df1, "~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/Incubation_Cmin/derived/2wk.new.csv")
#write.csv(df2, "~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/Incubation_Cmin/derived/4wk.new.csv")
#write.csv(df3, "~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/Incubation_Cmin/derived/8wk.new.csv")
#write.csv(df4, "~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/Incubation_Cmin/derived/12wk.new.csv")

###merge by row
df = rbind(df0, df1, df2, df3, df4)

#calculate mg of CO2-C in each sample (for the total sample masses)
df$C_loss_as_CO2 <- as.numeric(df$C_loss_as_CO2)
df$sample_mass_g <- as.numeric(df$sample_mass_g)
df$g_of_sample_mass <- as.numeric(df$g_of_sample_mass)

df$mg_CO2C_per_sample = df$C_loss_as_CO2/df$sample_mass_g*df$g_of_sample_mass*1000

#calculate the fraction of C that is mineralized during 12 weeks
df$mg_CO2C_per_sample = as.numeric(df$mg_CO2C_per_sample)
df$mg_C_per_sample = as.numeric(df$mg_C_per_sample)

df$mineralizability = df$mg_CO2C_per_sample/df$mg_C_per_sample

#calculate the faction of C remained

df$mineralizability = as.numeric(df$mineralizability)

df$frac_C_remained = 1-df$mineralizability

#write.csv(df, "~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/Incubation_Cmin/derived/incubation.all.new.csv")



##plot the 12wk CO2-C

CO2_plot <- df %>% filter(week == 12)

CO2_plot$Depth = factor(CO2_plot$Depth, levels = c('Surface','1 cm', '5 cm'), ordered = TRUE)
CO2_plot$HF = factor(CO2_plot$HF, levels = c('High','Low', 'Control'), ordered = TRUE)
legend = "Depth"
pd = position_dodge(width = 0.8)

CO2_plot %>%
  ggplot(aes(x = HF, y = mg_CO2C_per_sample, fill = Depth))+
  stat_boxplot(geom = "errorbar", width = 0.3, position = pd)+
  geom_boxplot(position = pd)+
  scale_fill_manual(legend, values = c('#a62445','#d4ad2c', '#2d8ab3'))+
  theme_bw()+
  theme(legend.title=element_text(size=12),
        legend.text=element_text(size=10),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.position = "bottom",
        #axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1),
        legend.key = element_blank())+
  labs(x = "Heat Flux", y = bquote('C'*O[2]*'-C (mg per sample)'), legend = "Depth")+
  scale_y_continuous(expand = c(0, 0), limits = c(-1, 11))

#ggsave("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/Incubation_Cmin/CO2-C.new.tiff", units="in", width=5, height=5)

#determine that one outlier
z <- (CO2_plot$mg_CO2C_per_sample[1] - mean(CO2_plot$mg_CO2C_per_sample[1:5], na.rm = TRUE)) / sd(CO2_plot$mg_CO2C_per_sample, na.rm = TRUE)
z

#remove the one ourlier
CO2_plot$mg_CO2C_per_sample[1] = NA

#anova with interaction effect
res.aov = aov(mg_CO2C_per_sample ~ HF * Depth, data = CO2_plot)
res.aov = aov(mg_CO2C_per_sample ~ HF + Depth + HF:Depth, data = CO2_plot)
summary(res.aov)

tk = TukeyHSD(res.aov)
cld <- multcompLetters4(res.aov, tk)
tk
cld

qqnorm(CO2_plot$mg_CO2C_per_sample)
qqline(CO2_plot$mg_CO2C_per_sample)
shapiro.test(CO2_plot$mg_CO2C_per_sample)

# plot mineralizability, I don't want to show high surface and 1cm in the graph, so I just assign a random number then mask it in powerpoint
min_plot <- CO2_plot %>%
  mutate(mineralizability = ifelse(HF == "High" & Depth %in% c("Surface", "1 cm"), 0.034, mineralizability)) %>%
  complete(HF, Depth)


min_plot$Depth = factor(min_plot$Depth, levels = c('Surface','1 cm', '5 cm'), ordered = TRUE)
min_plot$HF = factor(min_plot$HF, levels = c('High','Low', 'Control'), ordered = TRUE)
legend = "Depth"
pd = position_dodge(width = 0.8)


min_plot %>%
  ggplot(aes(x = HF, y = mineralizability, fill = Depth))+
  stat_boxplot(geom = "errorbar", width = 0.3, position = pd)+
  geom_boxplot(position = pd)+
  scale_fill_manual(legend, values = c('#a62445','#d4ad2c', '#2d8ab3'))+
  theme_bw()+
  theme(legend.title=element_text(size=12),
        legend.text=element_text(size=10),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.position = "bottom",
        #axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1),
        legend.key = element_blank())+
  labs(x = "Heat Flux", y = bquote('Mineralizability (mg C'*O[2]*'-C '*mg^-1 ~ C*')'), legend = "Depth")+
  scale_y_continuous(expand = c(0, 0), limits = c(-0.005, 0.034))

#ggsave("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/Incubation_Cmin/min.new.tiff", units="in", width=5, height=5)

#remove all trts in HighHF+Surface and 1 cm, and add a new column for stats analysis of mineralizability because trt changes
min_plot <- min_plot[-c(1:10),]
min_plot$trt = paste(min_plot$HF, min_plot$Depth, sep = " ")
min_plot$trt = as.factor(min_plot$trt)

#anova with trt
res.aov = aov(mineralizability ~ trt, data = min_plot)
summary(res.aov)

tk = TukeyHSD(res.aov)
cld <- multcompLetters4(res.aov, tk)
tk
cld

qqnorm(min_plot$mineralizability)
qqline(min_plot$mineralizability)
shapiro.test(min_plot$mineralizability)

#save the dataset df for DOE upload
colnames(df)[2] <- "Heat_Flux"
colnames(df)[4] <- "sample_mass_used_for_incubation_g"
colnames(df)[5] <- "mg_of_total_carbon_per_sample"
colnames(df)[6] <- "sample_mass_after_sample_collection_g"
colnames(df)[7] <- "cumulative_C_loss_as_CO2_in_the_15g_incubated_sample_g"
colnames(df)[9] <- "mg_of_CO2-C_in_each_sample_cumulated_by_week"
colnames(df)[10] <- "mineralizability_(mg_of_CO2-C_per_mg_of_C)"
colnames(df)[11] <- "fraction_of_C_remained"


#write.csv(df, "~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/manuscript_test/data_DOE/CO2-C_all_original.csv", row.names = FALSE)


