## This code produces the graph for coeff a and b



full = read.csv("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/Incubation_Cmin/derived/incubation.all.new.csv")


library(minpack.lm)

##create a variable to store each of your sample fitted output
decay.list <- list()

for (i in 11:45) {
  m <- summary(nlsLM(frac_C_remained ~ a*exp(-b*week)+(1-a), data=subset(full,SampleID == i), start=list(a=1, b=1),  
                     control = nls.lm.control(maxiter = 1000))) 
  decay.list[i-10] <- as.data.frame(c(coef(m)[,1:2]), col.names= c("a","b", "a_StdErr","b_StdErr" )) #store the coefficients of each of the sample fitted output into decay.list
  # decay.list[i-10]$sampleID <- i
}

##the variables that need to be output are : $convInfo$finIter, $finTol
#coef(m)
# df.residual(m)
# residuals(m)
# print(m)

##transform a list into a data table
decay.df <- do.call(rbind.data.frame, decay.list)
colnames(decay.df) <- c('a','b',"a_StdErr","b_StdErr")
decay.df$SampleID <- 11:45

decay.full = merge(full, decay.df, by = "SampleID")
decay.full$treatments = paste(decay.full$HF,decay.full$Depth)
decay.full$treatments = as.factor(decay.full$treatments)


#write_csv(decay.list, file = ...) #need to modify
#we basically treat a and b as observation, and there's 5 replicates for a and b, we need to use unique argument to take out the duplicates
decay.full.unique = unique(decay.full[,c("SampleID", 'a','b',"a_StdErr","b_StdErr","treatments", "HF", "Depth")])
####for a, do the log of the response (log of the degradable C, because a is a more interpretable one because it is a percentage), showing the ration of the percent is the same
#plot a on log scale, b on original scale, it's also okay to plot a on original scale
#name the t.test

#normality test for a 
qqnorm(decay.full.unique$a)
qqline(decay.full.unique$a, col = "red")
qqnorm(decay.full.unique$a[decay.full.unique$HF=="Low" & decay.full.unique$Depth=="5 cm"])
shapiro.test(decay.full$a)
qqnorm(log(decay.full$a))  # adding 1 to avoid log(0)
qqline(log(decay.full$a), col = "red")
shapiro.test(log(decay.full$a))

#normality test for b
qqnorm(decay.full$b)
qqline(decay.full$b, col = "red")
shapiro.test(decay.full$b) 
qqnorm(1 / decay.full$b)
qqline(1 / decay.full$b, col = "red")
shapiro.test(1 / decay.full$b)
## so for both a and b, it's not normally distributed

#Kruskal-Wallis test


#test the outlier for a and b, I wonder if i should exlude sample 16 in the mean

# for a, if not exclude
mean_a <- mean(decay.full.unique$a[decay.full.unique$SampleID %in% 16:20], na.rm = TRUE)
sd_a <- sd(decay.full.unique$a[decay.full.unique$SampleID %in% 16:20], na.rm = TRUE)
a_16 <- decay.full.unique$a[decay.full.unique$SampleID == 16]
z_16 <- (a_16 - mean_a) / sd_a
z_16 #z = -1.604416

# for a, if exclude
mean_a <- mean(decay.full.unique$a[decay.full.unique$SampleID %in% 17:20], na.rm = TRUE)
sd_a <- sd(decay.full.unique$a[decay.full.unique$SampleID %in% 17:20], na.rm = TRUE)
a_16 <- decay.full.unique$a[decay.full.unique$SampleID == 16]
z_16 <- (a_16 - mean_a) / sd_a
z_16 # z= -3.927334

# for b, if not exclude
mean_b <- mean(decay.full.unique$b[decay.full.unique$SampleID %in% 16:20], na.rm = TRUE)
sd_b <- sd(decay.full.unique$b[decay.full.unique$SampleID %in% 16:20], na.rm = TRUE)
b_16 <- decay.full.unique$b[decay.full.unique$SampleID == 16]
z_16 <- (b_16 - mean_b) / sd_b
z_16 # 1.75808

# for b, if exclude
mean_b <- mean(decay.full.unique$b[decay.full.unique$SampleID %in% 17:20], na.rm = TRUE)
sd_b <- sd(decay.full.unique$b[decay.full.unique$SampleID %in% 17:20], na.rm = TRUE)
b_16 <- decay.full.unique$b[decay.full.unique$SampleID == 16]
z_16 <- (b_16 - mean_b) / sd_b
z_16 #10.30473

#remove outlier sample 16
decay.full.unique = subset(decay.full.unique, SampleID != 16)

#stats for a
res.aov = aov(a ~  treatments, data = decay.full.unique)
summary(res.aov)
tk = TukeyHSD(res.aov)
cld <- multcompLetters4(res.aov, tk)
tk
cld

library(tidyr)
library(dplyr)
library(ggplot2)
library(multcompView)
citation("dplyr")
citation("multcompView")


#preparation of graphing for decay.full
#I need to assign a random value to the non-donfident treatment (high heat flux surface & 1 cm), the purpose of doing that is because I want to leave their space open in the boxplots
#it's basically like photoshop in R
df = read.csv("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/Incubation_Cmin/derived/12wk.new.csv")
df.non.confi.dat = df[c(1:10),] 
df.non.confi.dat$a = 0.05
df.non.confi.dat$b = 0.45
df.non.confi.dat$a_StdErr = 0
df.non.confi.dat$b_StdErr = 0
df.non.confi.dat = df.non.confi.dat[,-c(1,5,6,7,8,9)]
decay.full.for.graph = decay.full.unique[, -c(6)]

decay.full.for.graph = rbind(decay.full.for.graph, df.non.confi.dat)


#graphing for a 
decay.full.for.graph$Depth = factor(decay.full.for.graph$Depth, levels = c('Surface','1 cm', '5 cm'), ordered = TRUE)
decay.full.for.graph$HF = factor(decay.full.for.graph$HF, levels = c('High','Low', 'Control'), ordered = TRUE)
legend = "Depth"
pd = position_dodge(width = 0.8)


p1 <- decay.full.for.graph %>%
  ggplot(aes(x = HF, y = a, fill = Depth)) +
  stat_boxplot(geom = "errorbar", width = 0.3, position = pd) +
  geom_boxplot(position = pd) +
  scale_fill_manual(legend, values = c('#a62445','#d4ad2c', '#2d8ab3')) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(colour = "black", size = 1),
        legend.key = element_blank()) +
  labs(x = NULL,
       y = expression("Readily Decomposable C Fraction (Coefficient " * italic(a) * ")"),
       legend = "Depth") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.05))
p1
#ggsave("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/Incubation_Cmin/degradableC.no.outlier.new1.tiff", units="in", width=5, height=5)


#stats for b

res.aov = aov(b ~  treatments, data = decay.full.unique)
summary(res.aov)
tk = TukeyHSD(res.aov)
cld <- multcompLetters4(res.aov, tk)
tk
cld

#graohing for b
p2 <- decay.full.for.graph %>%
  ggplot(aes(x = HF, y = b, fill = Depth)) +
  stat_boxplot(geom = "errorbar", width = 0.3, position = pd) +
  geom_boxplot(position = pd) +
  scale_fill_manual(legend, values = c('#a62445','#d4ad2c', '#2d8ab3')) +
  theme_bw() +
  theme(legend.position = "bottom",  # suppress legend here, keep only in p1
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(colour = "black", size = 1),
        legend.key = element_blank()) +
  labs(x = "Heat Flux",
       y = bquote("Decomposition Rate (Coefficient " * italic(b) * ", Wee"*k^-1*")")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.45))
p2
#ggsave("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/Incubation_Cmin/degradation_speed.new.3.tiff", units="in", width=5, height=5)
library(patchwork)
combined_plot <- (p1 / p2) + plot_layout(heights = c(1, 1))
combined_plot

#ggsave("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/Incubation_Cmin/a&b.new.tiff", units="in", width=5, height=9)

