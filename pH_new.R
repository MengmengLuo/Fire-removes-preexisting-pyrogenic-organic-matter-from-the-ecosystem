ph = read.csv("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/pH/data/derived/pH_saturation.csv")
ph = ph[,-c(5:8)]
ph = na.omit(ph)
#colors = c('Surface' = '#a62445', '1cm' = '#d4ad2c', '5cm' = '#2d8ab3')
library(ggplot2)
library(dplyr)
ph$depth = factor(ph$depth, levels = c('Surface','1 cm', '5 cm'), ordered = TRUE)
ph$HF = factor(ph$HF, levels = c('High','Low', 'Control'), ordered = TRUE)
legend = "Depth"
pd = position_dodge(width = 0.8)


ph %>%
  ggplot(aes(x = HF, y = pH, fill = depth))+
  geom_hline(yintercept=6.675, color="lightgrey")+
  stat_boxplot(geom = "errorbar", width = 0.3, position = pd)+
  geom_boxplot(position = pd)+
  scale_fill_manual(legend, values = c('#a62445','#d4ad2c', '#2d8ab3'))+
  theme_bw()+
  theme(legend.title=element_text(size=14),
        legend.text=element_text(size=12),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = "bottom",
        panel.background = element_rect(colour = "black", size=1),
        legend.key = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs(x = "Heat Flux", y = "pH", legend = "Depth")+
  scale_y_continuous(limits = c(6.3, 7.8))+
  annotate(geom = "text", x=0.5, y=6.73, hjust=0, label="Quartz Sand", color = "dark grey", size = 4)

#ggsave("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/pH/ph_new_new.tiff", units="in", width=5, height=5)




#stat test
anova2 <- aov(pH ~ HF + depth, data = ph)
summary(anova2)
#anova with interaction effect
res.aov = aov(pH ~ HF * depth, data = ph)
res.aov = aov(pH ~ HF + depth + HF:depth, data = ph)
summary(res.aov)
tk = TukeyHSD(res.aov)
cld <- multcompLetters4(res.aov, tk)
tk
cld

?TukeyHSD

cld2 = data.frame(letters = cld$`HF:depth`$Letters)

################
#how pH related to peak T or degree hours
summaryT = read.csv("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/HF_treatments/degree_hours/data/Summary_T_New.csv")
bottom_only = summaryT[-c(1:5, 11:15, 21:25, 31:35, 41:45, 51:55),]
Sample.ID = c(1:30)
bottom_only = cbind(bottom_only, Sample.ID)
ph = ph[-c(31:45),]
ph = cbind(ph, Sample.ID)

merge.T.ph = merge(ph, bottom_only,by="Sample.ID")

M.dh.ph = lm(pH~Degree.hours,data=merge.T.ph)
# Inspect results
summary(M.dh.ph)

#Store coefficients
Model.Intercept = as.numeric(M.dh.ph$coefficients[1])
Model.Slope = as.numeric(M.dh.ph$coefficients[2])

colors = c('Surface' = '#a62445', '1 cm' = '#d4ad2c', '5 cm' = '#2d8ab3' )

p.dh = ggplot(merge.T.ph)+
  geom_point(aes(x=Degree.hours,y=pH,fill=Depth, color = Depth, shape=Heat.Flux))+
  geom_abline(intercept=Model.Intercept,slope=Model.Slope)+
  scale_fill_manual(values = colors)+
  scale_color_manual(values = colors)+
  scale_shape_manual(values = c(24,21))+
  labs(x = "Degree Hours (°C⋅hrs)", y = "pH", color = "Depth", shape = "Heat Flux")+
  theme(legend.title=element_text(size=12),
        legend.text=element_text(size=10),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))+
  theme_bw() +
  theme(#panel.background = element_rect(colour = "black", size=1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())
#scale_x_continuous(expand = c(0, 0), limits = c(0, 2200))+
#scale_y_continuous(expand = c(0, 0), limits = c(0, 1.02))

p.dh

# how ph related to peak T
M.T.ph = lm(pH~Peak_T,data=merge.T.ph)
# Inspect results
summary(M.T.ph)

#Store coefficients
Model.Intercept.1 = as.numeric(M.T.ph$coefficients[1])
Model.Slope.1 = as.numeric(M.T.ph$coefficients[2])

colors = c('Surface' = '#a62445', '1 cm' = '#d4ad2c', '5 cm' = '#2d8ab3' )

p.T = ggplot(merge.T.ph)+
  geom_point(aes(x=Peak_T,y=pH,fill=Depth, color = Depth, shape=Heat.Flux))+
  geom_abline(intercept=Model.Intercept,slope=Model.Slope)+
  scale_fill_manual(values = colors)+
  scale_color_manual(values = colors)+
  scale_shape_manual(values = c(24,21))+
  labs(x = "Peak Temperature (°C)", y = "pH", color = "Depth", shape = "Heat Flux")+
  theme(legend.title=element_text(size=12),
        legend.text=element_text(size=10),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))+
  theme_bw() +
  theme(#panel.background = element_rect(colour = "black", size=1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())
#scale_x_continuous(expand = c(0, 0), limits = c(0, 2200))+
#scale_y_continuous(expand = c(0, 0), limits = c(0, 1.02))

p.T


#try another model for peak T-quadratic
library(minpack.lm)

###### high hf, 5cm
m <- nlsLM(pH ~ a*Peak_T^2+b*Peak_T+c, data=merge.T.ph, start=list(a=1, b=1, c=1),  
           control = nls.lm.control(maxiter = 660))
summary(m)


p1 = ggplot(merge.T.ph, aes(x=Peak_T,y=pH))+
  geom_point(aes(fill=Depth,color = Depth, shape=Heat.Flux))+
  stat_smooth(method = 'nlsLM', formula = y ~ a*x^2+b*x+c, se = FALSE,
              method.args = list(start=coef(m)), color = "black")+
  scale_fill_manual(values = colors)+
  scale_color_manual(values = colors)+
  scale_shape_manual(values = c(24,21))+
  labs(x = "Peak Temperature (°C)", y = "pH", color = "Depth", shape = "Heat Flux")+
  theme(legend.title=element_text(size=12),
        legend.text=element_text(size=10),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))+
  theme_bw() +
  theme(#panel.background = element_rect(colour = "black", size=1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())+
  scale_x_continuous(expand = c(0, 0), limits = c(100, 660))+
  scale_y_continuous(expand = c(0, 0), limits = c(6.2, 7.7))
p1

#ggsave("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/pH/pH.vs.peakT.tiff", units="in", width=5, height=5)
#get the R-square for ph vs peakT
# define a null function
nullfunct = function(x, w){w}

# Get the mean ph
meanpH    = mean(merge.T.ph$pH)

# Generate the null model
null = nls(pH ~ nullfunct(Peak_T, w),
           data = merge.T.ph,
           start = list(w   = meanpH),
           trace = FALSE,
           nls.control(maxiter = 660))

# Compare model to null

PseudoR = rcompanion::nagelkerke(m,null)
PseudoR$Pseudo.R.squared.for.model.vs.null

#try another model for peak T - segmented regression

library(segmented)

my.seg <- segmented(M.T.ph, 
                    seg.Z = ~ Peak_T, 
                    psi = list(Peak_T = 290)) #290 is a guess


summary(my.seg)
my.seg$psi
confint(my.seg) #check if the 95% CI include zero -- no, so it is statistically significant


my.fitted <- fitted(my.seg)
my.model <- data.frame(Peak_T = merge.T.ph$Peak_T, pH = my.fitted)
ggplot(my.model, aes(x = Peak_T, y = pH)) + geom_line()

p2 = ggplot(merge.T.ph, aes(x=Peak_T,y=pH))+
  geom_point(aes(fill=Depth,color = Depth, shape=Heat.Flux))+
  scale_fill_manual(values = colors)+
  scale_color_manual(values = colors)+
  scale_shape_manual(values = c(24,21))+
  labs(x = "Peak Temperature (°C)", y = "pH", color = "Depth", shape = "Heat Flux")+
  theme(legend.title=element_text(size=12),
        legend.text=element_text(size=10),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))+
  theme_bw() +
  theme(panel.background = element_rect(colour = "black", size=1),
        legend.key = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())+
  scale_x_continuous(expand = c(0, 0), limits = c(100, 660))+
  scale_y_continuous(expand = c(0, 0), limits = c(6.2, 7.7))
p2

p2 = p2 + geom_line(data=my.model, aes(x=Peak_T, y=pH), color="black")
p2
#ggsave("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/pH/pH.vs.peakT-seg.tiff", units="in", width=4, height=4)

