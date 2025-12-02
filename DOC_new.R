#This code only produces figures of total DOC and DIC


library(tidyr)
library(dplyr)
library(ggplot2)
library(ggtext)


doc = read.csv("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/DOC/derived/Results_Exported_2023-04-06_123105.csv")
doc = doc[,-c(19)]
doc = na.omit(doc)
doc = doc[-c(1,2,3,13,23,33,43,53),]

doc.redo = read.csv("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/DOC/derived/Results_Exported_2023-05-09_105206.csv")
doc.redo = doc.redo[,-c(20)]
doc.redo = na.omit(doc.redo)

carbon = read.csv("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/C_N/data/derived/for_DOC&CO2C_anal.csv")

doc$HF = paste(carbon$Heat.Flux)
doc$Depth = paste(carbon$Depth)

doc$TOC_ppm = paste(doc$TOC.Average..ppb./1000)
doc$IC_ppm = paste(doc$IC.Average..ppb./1000)
doc$TC_ppm = paste(doc$TC.Average..ppb./1000)

doc$TOC_ppm = as.numeric(doc$TOC_ppm)
doc$IC_ppm = as.numeric(doc$IC_ppm)
doc$TC_ppm = as.numeric(doc$TC_ppm)

doc = doc[,-c(2:18)]
doc

##correct outliers
#set the TOC outliers to updated number
doc$TOC_ppm[24] = 21.0 
doc$TOC_ppm[43] = 21.6

###calculate blank doc, ic, and tc (mg per sample), sample mass is from the average of all
doc.blank = mean(doc.redo$TOC.Average..ppb.[3:5])/1000*(0.021/7)*mean(carbon$final.sample.mass)
ic.blank = mean(doc.redo$IC.Average..ppb.[3:5])/1000*(0.021/7)*mean(carbon$final.sample.mass)
tc.blank = mean(doc.redo$TC.Average..ppb.[3:5])/1000*(0.021/7)*mean(carbon$final.sample.mass)

doc$final.sample.mass <- carbon$final.sample.mass


###specify ppm as mg DOC&IC per sample, with DOC blanks subtracted
doc$mg_doc_per_sample = paste(doc$TOC_ppm*(0.021/7)*doc$final.sample.mass)
doc$mg_ic_per_sample = paste(doc$IC_ppm*(0.021/7)*doc$final.sample.mass)
doc$mg_doc_per_sample = as.numeric(doc$mg_doc_per_sample)
doc$mg_ic_per_sample = as.numeric(doc$mg_ic_per_sample)


#graph
doc$Depth = factor(doc$Depth, levels = c('Surface','1 cm', '5 cm'), ordered = TRUE)
doc$HF = factor(doc$HF, levels = c('High','Low', 'Control'), ordered = TRUE)
legend = "Depth"
pd = position_dodge(width = 0.8)

#DOC
doc %>%
  ggplot(aes(x = HF, y = mg_doc_per_sample, fill = Depth))+
  geom_hline(yintercept = doc.blank, color="lightgrey")+
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
  labs(x = "Heat Flux", y = "DOC (mg per sample)", legend = "Depth")+
  scale_y_continuous(expand = c(0, 0), limits = c(1.5, 8))+
  annotate(geom = "text", x=0.5, y=2, hjust=-3.8, label="Quartz Sand", color = "dark grey", size = 4)


#ggsave("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/DOC/doc.new.new.tiff",units="in", width=5, height=5)


library(multcompView)

anova_incb <- aov(mg_doc_per_sample ~ HF + Depth, data = doc)
summary(anova_incb)

res.aov = aov(mg_doc_per_sample ~ HF * Depth, data = doc)
summary(res.aov)
tk = TukeyHSD(res.aov)
cld <- multcompLetters4(res.aov, tk)
tk
cld


#DIC
doc %>%
  ggplot(aes(x = HF, y = mg_ic_per_sample, fill = Depth))+
  geom_hline(yintercept=ic.blank, color="lightgrey")+
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
  labs(x = "Heat Flux", y = "DIC (mg per sample)", legend = "Depth")+
  scale_y_continuous(limits = c(0.2, 0.9))+
  annotate(geom = "text", x=0.5, y=0.52, hjust=0, label="Quartz Sand", color = "dark grey", size = 4)

#ggsave("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/DOC/ic.new.new.tiff",units="in", width=5, height=5)

anova_incb <- aov(mg_ic_per_sample ~ HF + Depth, data = doc)
summary(anova_incb)

res.aov = aov(mg_ic_per_sample ~ HF * Depth, data = doc)
summary(res.aov)
tk = TukeyHSD(res.aov)
cld <- multcompLetters4(res.aov, tk)
tk
cld

#normality test for DOC

qqnorm(doc$mg_doc_per_sample)
qqline(doc$mg_doc_per_sample, col="red")
shapiro.test(doc$mg_doc_per_sample)
hist(doc$mg_doc_per_sample, breaks=20, main="Histogram", xlab="Value", col="lightblue")

#normality test for DIC

qqnorm(doc$mg_ic_per_sample)
qqline(doc$mg_ic_per_sample, col="red")
shapiro.test(doc$mg_ic_per_sample)
hist(doc$mg_ic_per_sample, breaks=20, main="Histogram", xlab="Value", col="lightblue")

#write.csv(doc, "~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/DOC/derived/doc.csv")

#save dataset to upload to DOE

colnames(doc)[1] <- "SampleID"
colnames(doc)[2] <- "Heat_Flux"
colnames(doc)[4] <- "dissolved_organic_carbon_ppm"
colnames(doc)[5] <- "dissolved_inorganic_carbon_ppm"
colnames(doc)[6] <- "dissolved_total_carbon_ppm"
colnames(doc)[7] <- "sample_mass_after_sample_collection_g"

#write.csv(doc, "~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/manuscript_test/data_DOE/doc_dic.csv", row.names = FALSE)

