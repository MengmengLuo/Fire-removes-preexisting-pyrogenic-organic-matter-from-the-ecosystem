#this code produce the decay model graph, and decay model fitting statistics

full = read.csv("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/Incubation_Cmin/derived/incubation.all.new.csv")

library(minpack.lm)

## Ensure SampleID is correctly formatted
full$SampleID <- as.integer(full$SampleID)

## Create a list to store each fitted output
decay.list <- list()

for (i in 11:45) {
  m <- nlsLM(frac_C_remained ~ a*exp(-b*week) + (1-a), 
             data=subset(full, SampleID == i), 
             start=list(a=1, b=1), 
             control = nls.lm.control(maxiter = 1000))
  
  decay.list[[i-10]] <- as.data.frame(t(coef(m)))  # Store coefficients correctly
}

## Transform list into a data frame
decay.df <- do.call(rbind.data.frame, decay.list)
colnames(decay.df) <- c('a', 'b')
decay.df$SampleID <- 11:45  # Ensure correct SampleID assignment

## Merge with full dataset
decay.full = merge(full, decay.df, by = "SampleID")

## Define treatments
decay.full$treatments = as.factor(paste(decay.full$HF, decay.full$Depth))

## Ensure factor levels are ordered correctly
decay.full$Depth = factor(decay.full$Depth, levels = c('Surface','1 cm', '5 cm'), ordered = TRUE)
decay.full$HF = factor(decay.full$HF, levels = c('High','Low', 'Control'), ordered = TRUE)

## Colors
colors = c('Surface' = '#a62445', '1 cm' = '#d4ad2c', '5 cm' = '#2d8ab3')

#exclude sample 16, because both a and b for sample 16 are outliers
decay.full <- decay.full %>% filter(SampleID != 16)

## Plot
decay.full %>%
  ggplot(aes(x=week, y=frac_C_remained, group = SampleID, col = treatments)) +
  geom_point() +
  facet_grid(Depth ~ HF) +
  stat_smooth(method = 'nlsLM', 
              formula = y ~ a*exp(-b*x) + (1-a), 
              se = FALSE,
              method.args = list(start = list(a=1, b=1)), 
              size = 0.5) +  ## Ensure proper starting values
  scale_x_continuous(limits = c(0, 12), breaks = c(0, 2, 4, 8, 12)) +
  theme_bw() +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1)) +
  labs(x = "Week", y = "Fraction of C Remaining")
#ggsave("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/Incubation_Cmin/C_CO2_indiv_new.tiff", units="in", width=6, height=4)

##test the fitness of each model

# Load the necessary library
library(minpack.lm)

# Create a list to store the model results for each sample
model_results <- list()

# Loop through each unique SampleID in the dataset
for (sample in unique(decay.full$SampleID)) {
  
  # Fit the model for the current sample
  model <- try(nlsLM(frac_C_remained ~ a * exp(-b * week) + (1 - a),
                     data = subset(decay.full, SampleID == sample),
                     start = list(a = 1, b = 1),
                     control = nls.lm.control(maxiter = 1000)))
  
  # Check if the model fitting was successful
  if (inherits(model, "try-error")) {
    next  # Skip to the next sample if the model didn't fit
  }
  
  # Extract coefficients and standard errors
  coef_values <- coef(model)
  coef_se <- sqrt(diag(vcov(model)))
  
  # Get the summary of the model to extract p-values
  model_summary <- summary(model)
  
  # Calculate R-squared
  observed <- subset(decay.full, SampleID == sample)$frac_C_remained
  predicted <- predict(model)
  RSS <- sum((observed - predicted)^2)
  TSS <- sum((observed - mean(observed))^2)
  R_squared <- 1 - (RSS / TSS)
  
  # Create a data frame with the results for this sample
  model_results[[as.character(sample)]] <- data.frame(
    SampleID = sample,
    a = coef_values[1],
    b = coef_values[2],
    a_StdErr = coef_se[1],
    b_StdErr = coef_se[2],
    p_a = model_summary$coefficients[1, 4],
    p_b = model_summary$coefficients[2, 4],
    R_squared = R_squared
  )
}

# Combine all the results into a single data frame
model_results_df <- do.call(rbind, model_results)
model_results_df = model_results_df[, -c(2,3)]

# Merge with your original dataset to add the model results
decay.full_with_results <- merge(decay.full, model_results_df, by = "SampleID", all.x = TRUE)

# View the results
head(decay.full_with_results)

decay.full.unique.results = unique(decay.full_with_results[,c("SampleID", 'a','b',"p_a", "p_b", "R_squared","a_StdErr","b_StdErr","treatments", "HF", "Depth")])

# Filter for SampleID where p_a > 0.05
p_a_greater_0_05 <- decay.full.unique.results[decay.full.unique.results$p_a > 0.05, "SampleID"]
p_a_greater_0_05
#. SampleIDs with p_a > 0.05: 17 20 

# Filter for SampleID where p_a > 0.1
p_a_greater_0_1 <- decay.full.unique.results[decay.full.unique.results$p_a > 0.1, "SampleID"]
p_a_greater_0_1
#. SampleIDs with p_a > 0.05: 20 

# Filter for SampleID where p_b > 0.05
p_b_greater_0_05 <- decay.full.unique.results[decay.full.unique.results$p_b > 0.05, "SampleID"]
p_b_greater_0_05
#. SampleIDs with p_b > 0.05: 17 18 19 20 28 31 

# Filter for SampleID where p_b > 0.1
p_b_greater_0_1 <- decay.full.unique.results[decay.full.unique.results$p_b > 0.1, "SampleID"]
p_b_greater_0_1
#. SampleIDs with p_b > 0.1: 17 19 20

# Filter for SampleID where R_squared < 0.95
R_squared_less_0_95 <- decay.full.unique.results[decay.full.unique.results$R_squared < 0.95, "SampleID"]
R_squared_less_0_95 
#. SampleIDs with R_squared < 0.95: 17 19 20 

min(decay.full.unique.results$R_squared)
max(decay.full.unique.results$p_a)
max(decay.full.unique.results$p_b)

#write.csv(decay.full.unique.results, "~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/Incubation_Cmin/derived/decay.model.all.stats.csv")

