# Content
# 1. import CN data, match EA data with sample info --done
# 2. combine the redo data to create the full data set, including mass and C% --done
# 3. clean it up, and do an initial analysis and visualization of the data --done
# 4. calculate the proportion of C got consumed in relative to the unburned control --done
# Created Feb 26, 2025
# Mengmeng Luo and Thea Whitman

# Set working directory to the location of this script

# Load relevant libraries
library(tidyr)
library(dplyr)
library(ggplot2)

# Import data

# Import EA data from Jackson lab
CN_EA = read.csv("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/C_N/data/derived/Whitman Luo CN 111822_raw_columnnames.csv")
# Remove rows with no masses
CN_EA = CN_EA[!is.na(CN_EA$Mass_g),]

# Import sample information
CN_samdat_high = read.csv("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/C_N/data/derived/CN_high.csv")
CN_samdat_low = read.csv("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/C_N/data/derived/CN_low.csv")
# Look at data table to make sure it's ok
head(CN_samdat_low)
head(CN_samdat_high)

dim(CN_samdat_low)
dim(CN_samdat_high)

# Goal: Create table that has sample information, along with C and N data
# This will be done by matching sample masses because there wasn't a separate ID
# Make sure same colnames before rbind
colnames(CN_samdat_high) == colnames(CN_samdat_low)

# Join the high and low sample data
CN_samdat_full = rbind(CN_samdat_high,CN_samdat_low)
dim(CN_samdat_full)

# check - are masses unique?
CN_samdat_full$sample.mass
# Calculate the number of unique masses, and compare it to the number of rows in the sample data
# Since it is the same, we are good to go
length(unique(CN_samdat_full$sample.mass)) == dim(CN_samdat_full)[1]

# Now we will merge by masses

#Check - is each mass present in the EA data?
CN_samdat_full$sample.mass %in% CN_EA$Mass_g

# We see three samples masses are not found in the EA results.
# Which were they?
CN_samdat_full[!(CN_samdat_full$sample.mass %in% CN_EA$Mass_g),]

# The 398.983 missing sample was entered incorrectly in the EA machine as 399.983
# The 399.898 sample was not analyzed
# A sample got analyzed twice in EA
# The 60.515 sample was entered incorrectly in the EA machine as 65.15

# Correct first error
CN_EA[CN_EA$Mass_g==399.983,]$Mass_g = 398.983

# Correct second error
CN_EA[CN_EA$Mass_g==65.15,]$Mass_g = 60.515


#Check again - is each mass present in the EA data?
CN_samdat_full$sample.mass %in% CN_EA$Mass_g

# Now we will merge the datasets
colnames(CN_EA)[7] = "sample.mass" #unify the colname for both datasets
CN_full = merge(CN_samdat_full,CN_EA,by="sample.mass")
dim(CN_full)

# We can see one mass is duplicated
Errors = levels(as.factor(CN_full$Notes))
Errors = Errors[3:4]
Errors

CN_full$Notes %in% Errors

# Filter out error rows
CN_full = CN_full %>%
  dplyr::filter(!(Notes %in% Errors))

colnames(CN_full)

# Clean up data table
CN_full_clean = CN_full %>%
  dplyr::select(Sample.ID,Heat.Flux,Depth,rep,sample.mass,N_pct,C_pct,Notes)

CN_full_clean = arrange(CN_full_clean, Sample.ID)

###############################################################################

# At this point, the test results from the first round of test got cleaned
# we'll need to clean up the data from the redo test, and combine the redo with the cleaned original dataset


# Import EA data from Jackson lab
CN_EA_redo = read.csv("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/C_N/data/derived/Whitman Luo CN 032123_columnnames.csv")
# Remove rows with no masses
CN_EA_redo = CN_EA_redo[!is.na(CN_EA_redo$X),]


# Remove blank columns
CN_EA_redo = CN_EA_redo[, -c(10:251)]

# Change colname of mass to the same
colnames(CN_EA_redo)[6]
colnames(CN_EA_redo)[6] = "sample.mass"



# Import sample information
CN_samdat_redo = read.csv("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/C_N/data/derived/CN_redo.csv")

# Look at data table to make sure it's ok
head(CN_samdat_redo)

dim(CN_samdat_redo)

#remove unrelated rows and columns
CN_samdat_redo = CN_samdat_redo[ -c(22:25, 30:36), -c(1, 13, 14)]


# Goal: Create table that has sample information, along with C and N data
# This will be done by matching sample masses because there wasn't a separate ID
# lucky me but stupid me, I hope to find a way to make things easier next time :(

# check - are masses unique?
CN_samdat_redo$sample.mass
# Calculate the number of unique masses, and compare it to the number of rows in the sample data
# Since it is the same, we are good to go
length(unique(CN_samdat_redo$sample.mass)) == dim(CN_samdat_redo)[1]

# Now we will merge by masses

#Check - is each mass present in the EA data? 
CN_samdat_redo$sample.mass %in% CN_EA_redo$sample.mass # Y, so no need to correct the error

# Now we will merge the datasets
CN_redo = merge(CN_samdat_redo,CN_EA_redo,by="sample.mass")
dim(CN_redo)

# Clean up data table
CN_redo_clean = CN_redo %>%
  dplyr::select(Sample.ID,Heat.Flux,Depth,rep,sample.mass,X0.1,X0.2, X.1) 
colnames(CN_redo_clean) = c('Sample.ID','Heat.Flux','Depth','rep','sample.mass','N_pct','C_pct', 'Notes')

CN_redo_clean = arrange(CN_redo_clean, Sample.ID)

#combine CN_full_clean and CN_redo_clean
# Make sure same colnames before rbind
colnames(CN_full_clean) == colnames(CN_redo_clean)

# Join the CN_full_clean and CN_redo_clean (original test and redo test)
CN_full_final = rbind(CN_full_clean,CN_redo_clean)
dim(CN_full_final)

CN_full_final = arrange(CN_full_final, strtoi(Sample.ID,10))

#cut out repeated ones from the last measurement
CN_full_final = CN_full_final[-c(31, 32, 35, 36, 39, 40, 43, 44, 47, 48, 52, 98),]
CN_full_final = arrange(CN_full_final, strtoi(Sample.ID,10))

CN_full_final$Sample.ID = as.factor(CN_full_final$Sample.ID)
CN_full_final$Sample.ID = ordered(CN_full_final$Sample.ID,levels=sort(as.numeric(levels(CN_full_final$Sample.ID))))
# this two lines of code only works when you want to get the average in order


# get average C% out of replicates
C_pct_avg = aggregate(C_pct ~ Sample.ID, data=CN_full_final, FUN=mean)
as.numeric(C_pct_avg$Sample.ID)

# get average mass out of replicates
mass_avg = aggregate(sample.mass ~ Sample.ID, data=CN_full_final, FUN=mean)
as.numeric(mass_avg$Sample.ID)

# get average N% out of replicates
N_pct_avg = aggregate(N_pct ~ Sample.ID, data=CN_full_final, FUN=mean)
as.numeric(N_pct_avg$Sample.ID)


######################################################################
# make a new dataset for the averaged data

CN_final_avg = merge(mass_avg, C_pct_avg, by="Sample.ID")
CN_final_avg = merge(CN_final_avg, N_pct_avg, by="Sample.ID")

CN_final_avg = arrange(CN_final_avg, strtoi(Sample.ID,10))

cm_below_surface = c(0,0,0,0,0,1,1,1,1,1,5,5,5,5,5,0,0,0,0,0,1,1,1,1,1,5,5,5,5,5,0,0,0,0,0,1,1,1,1,1,5,5,5,5,5)
class(cm_below_surface)
CN_final_avg = cbind(CN_final_avg, cm_below_surface)

Heat.Flux = c("High", "High", "High", "High", "High", "High", "High", "High", "High", "High", "High", "High", "High", "High", "High", 
              "Low", "Low", "Low", "Low", "Low", "Low", "Low", "Low", "Low", "Low", "Low", "Low", "Low", "Low", "Low",
              "Control", "Control", "Control", "Control", "Control", "Control", "Control", "Control", "Control", "Control", "Control", "Control", "Control", "Control", "Control")
CN_final_avg = cbind(CN_final_avg, Heat.Flux)

Depth = c("Surface", "Surface", "Surface", "Surface", "Surface", "1 cm", "1 cm", "1 cm", "1 cm", "1 cm", "5 cm", "5 cm", "5 cm", "5 cm", "5 cm", 
          "Surface", "Surface", "Surface", "Surface", "Surface", "1 cm", "1 cm", "1 cm", "1 cm", "1 cm", "5 cm", "5 cm", "5 cm", "5 cm", "5 cm",
          "Surface", "Surface", "Surface", "Surface", "Surface", "1 cm", "1 cm", "1 cm", "1 cm", "1 cm", "5 cm", "5 cm", "5 cm", "5 cm", "5 cm")
CN_final_avg = cbind(CN_final_avg, Depth)

###anova
res.aov = aov(C_pct ~ Heat.Flux * Depth, data = CN_final_avg)
summary(res.aov)

res.aov = aov(C_pct ~ Heat.Flux , data = CN_final_avg)
summary(res.aov)


###########################################
#plot with error bar

data_summary <- function(CN_final_avg, Heat.Flux, cm_below_surface){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(CN_final_avg, cm_below_surface, .fun=summary_func,
                  Heat.Flux)
  data_sum <- rename(data_sum, c("mean" = Heat.Flux))
  return(data_sum)
}
data_summary

df <- data_summary(CN_final_avg, Heat.Flux="C_pct", 
                   cm_below_surface=c("cm_below_surface", "Heat.Flux"))
CN_final_avg$cm_below_surface <- as.factor(CN_final_avg$cm_below_surface)
head(df)

df

df$Heat.Flux = factor(df$Heat.Flux, levels = c('High','Low', 'Control'), ordered = TRUE)
#colors = c('Surface' = '#a62445', '1cm' = '#d4ad2c', '5cm' = '#2d8ab3')


df %>%
  ggplot(aes(x = C_pct,y = cm_below_surface))+
  geom_vline(xintercept=0,color="lightgrey")+geom_hline(yintercept=0, color="lightgrey")+
  geom_path(aes(linetype=Heat.Flux), size = 1)+
  geom_point(aes(fill=Heat.Flux, shape=Heat.Flux), size = 5)+
  geom_errorbar(aes(xmin=C_pct-sd, xmax=C_pct+sd), width=.3) +
  scale_fill_manual(values = c('#debb6a', '#58822a', '#D3D3D3'))+
  scale_shape_manual(values=c(24,21,23))+
  guides(fill=guide_legend(title="Heat Flux"),shape=guide_legend(title="Heat Flux"),linetype=guide_legend(title="Heat Flux"))+
  theme_bw()+
  theme(legend.title=element_text(size=12),
        legend.text=element_text(size=10),
        legend.key.width = unit(1.5, "cm"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.position = "bottom",
        #axis.line = element_line(color='black'),
        panel.background = element_rect(colour = "black", size=1),
        legend.key = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs(x = "%C in PyOM-Sand Samples (mean ± SD)", y = "Depth (cm)", legend = "Depth")+
  scale_y_reverse(limits=c(5.5,-0.5), breaks = seq(0, 6, 1)) +
  scale_x_continuous(position="top",expand = c(0, 0), limits = c(-.1, 1.1), breaks = seq(0, 1.1, 0.25))

# Save the graph
#ggsave("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/C_N/total_C.tiff", units="in", width=5, height=3.5)
#ggsave("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/C_N/total_C_v2.tiff", units="in", width=5, height=3.5)
#ggsave("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/C_N/total_C_v3.tiff", units="in", width=5, height=3.5)


#############################################
# normalize the Carbon concentration results by the control values to show the fraction of initial Carbon that was consumed

# heat flux and depth in the data frame are characters, so needs to change to factors
CN_final_avg$Heat.Flux = as.factor(CN_final_avg$Heat.Flux)
CN_final_avg$Depth = as.factor(CN_final_avg$Depth)

# so here we need to put out the dataset for the total sample mass

sample_mass_dat = read.csv("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/mass/mass_summary+estimation.csv")

sample_mass_dat = sample_mass_dat[,-c(2,3)]

CN_merge = merge(CN_final_avg, sample_mass_dat, by = 'Sample.ID')

# so here we calculate how much C is estimated to be in one whole sample

CN_merge$mg_C_per_sample = CN_merge$C_pct/100*CN_merge$final.sample.mass*1000

control_dat = CN_merge %>% filter(Heat.Flux=="Control")

control_mean <- aggregate(mg_C_per_sample ~ Depth, data = control_dat, FUN = mean, na.rm = TRUE)

# Convert the result to a data frame (optional, as aggregate already returns a data frame)
control_mean <- as.data.frame(control_mean)

#save this dataset for analyzing DOC and minC
CN_merge <- CN_merge %>% arrange(Sample.ID)

#write.csv(CN_merge, "~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/C_N/data/derived/for_DOC&CO2C_anal.csv", row.names = FALSE)

CN_merge = merge(CN_merge, control_mean, by = "Depth")

CN_merge$c_loss_frac = (CN_merge$mg_C_per_sample.y-CN_merge$mg_C_per_sample.x)/(CN_merge$mg_C_per_sample.y)

C_frac_dat = CN_merge[CN_merge$Heat.Flux != "Control", ]

library(vegan)
Model.1 = lm(c_loss_frac~average.mass.loss.for.treatments,data=C_frac_dat,na.action = na.exclude)
# Inspect results
summary(Model.1)

#Store coefficients
Model.Intercept.1 = as.numeric(Model.1$coefficients[1])
Model.Slope.1 = as.numeric(Model.1$coefficients[2])

colors = c('Surface' = '#a62445', '1 cm' = '#d4ad2c', '5 cm' = '#2d8ab3' )

p1 = ggplot(C_frac_dat)+
  geom_point(aes(x=average.mass.loss.for.treatments,y=c_loss_frac,fill=Depth,color = Depth, shape=Heat.Flux))+
  geom_abline(intercept=Model.Intercept.1,slope=Model.Slope.1)+
  scale_fill_manual(values = colors)+
  scale_color_manual(values = colors)+
  scale_shape_manual(values = c(24,21))+
  labs(x = "Mass Loss Fraction", y = "C Loss Fraction", color = "Depth", shape = "Heat Flux")+
  theme_bw() +
  theme(legend.title=element_text(size=12),
        legend.text=element_text(size=10),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.position = "right",
        #panel.background = element_rect(colour = "black", size=1
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1.02))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.02))
#geom_label(aes(x = 0.6, y = 0.2), hjust = 0, 
#       label = paste( "y = 0.97x + 0.04",
#                       "\nRsq = 0.98",
#                      "\np < 0.05"),
#       label.size = NA)

p1

#ggsave("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/C_N/massloss.vs.c.loss.new.tiff", units="in", width=5, height=5)

###########################################
#plot with error bar for estimated C stock (not percentage)

data_summary <- function(CN_merge, Heat.Flux, cm_below_surface){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(CN_merge, cm_below_surface, .fun=summary_func,
                  Heat.Flux)
  data_sum <- rename(data_sum, c("mean" = Heat.Flux))
  return(data_sum)
}
data_summary

df <- data_summary(CN_merge, Heat.Flux="mg_C_per_sample.x", 
                   cm_below_surface=c("cm_below_surface", "Heat.Flux"))
CN_merge$cm_below_surface <- as.factor(CN_merge$cm_below_surface)
head(df)

df

df$cm_below_surface <- as.numeric(as.character(df$cm_below_surface))

df$Heat.Flux = factor(df$Heat.Flux, levels = c('High','Low', 'Control'), ordered = TRUE)
#colors = c('Surface' = '#a62445', '1cm' = '#d4ad2c', '5cm' = '#2d8ab3')


df %>%
  ggplot(aes(x = mg_C_per_sample.x,y = cm_below_surface))+
  geom_vline(xintercept=0,color="lightgrey")+geom_hline(yintercept=0, color="lightgrey")+
  geom_path(aes(linetype=Heat.Flux), size = 1)+
  geom_point(aes(fill=Heat.Flux, shape=Heat.Flux), size = 5)+
  geom_errorbar(aes(xmin=mg_C_per_sample.x-sd, 
                    xmax=mg_C_per_sample.x+sd,
                    group = Heat.Flux
                    ), 
                width=0.3) +
  scale_fill_manual(values = c('#debb6a', '#58822a', '#D3D3D3'))+
  scale_shape_manual(values=c(24,21,23))+
  guides(fill=guide_legend(title="Heat Flux"),shape=guide_legend(title="Heat Flux"),linetype=guide_legend(title="Heat Flux"))+
  theme_bw()+
  theme(legend.title=element_text(size=12),
        legend.text=element_text(size=10),
        legend.key.width = unit(1.5, "cm"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.position = "bottom",
        #axis.line = element_line(color='black'),
        panel.background = element_rect(colour = "black", size=1),
        legend.key = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs(x = "C in PyOM-Sand Samples (mean ± SD, mg C per sample)", y = "Depth (cm)", legend = "Depth")+
  scale_y_reverse(limits=c(5.15, -0.15), breaks = seq(0, 6, 1)) +
  scale_x_continuous(position="top",expand = c(0, 0), limits = c(-25, 825), breaks = seq(0, 800, 100))

# Save the graph
#ggsave("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/C_N/total_C_stock.tiff", units="in", width=5, height=3.5)

###anova
res.aov = aov(mg_C_per_sample.x ~ Heat.Flux * Depth, data = CN_merge)
summary(res.aov)

res.aov = aov(mg_C_per_sample.x ~ Heat.Flux , data = CN_merge)
summary(res.aov)


#save dataset to for DOE upload
colnames(CN_merge)[3] <- "sample_mass_for_CN_analyses_prep_g"
colnames(CN_merge)[6] <- "Depth_cm_below_surface"
colnames(CN_merge)[8] <- "original_PyOM_mass_in_each_sample_g"
colnames(CN_merge)[9] <- "average_mass_loss_in_each_heatflux-depth_treatment_g"
colnames(CN_merge)[10] <- "mass_of_PyOM_left_in_each_sample_after_heatflux-depth_treatment_g"
colnames(CN_merge)[11] <- "final_PyOM-sand_sample_mass_after_sample_collection_g"
colnames(CN_merge)[12] <- "C_in_PyOM-sand_samples_after_heatflux-depth_treatment_(mg_C_per_sample)"
colnames(CN_merge)[13] <- "average_C_in_PyOM-sand_samples_in_controls_(mg_C_per_sample)_for_calculating_C_loss_fraction"
colnames(CN_merge)[14] <- "calculated_C_loss_fraction"

#write.csv(CN_merge, "~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/manuscript_test/data_DOE/CN_pct_C_stock_C_loss_in_samples.csv", row.names = FALSE)
