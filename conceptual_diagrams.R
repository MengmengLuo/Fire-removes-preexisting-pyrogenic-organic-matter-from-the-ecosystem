library(ggplot2)
library(dplyr)

# ------------------------------
# Example data
# ------------------------------
# Two regimes: high-intensity/low-frequency vs. low-intensity/high-frequency
df <- data.frame(
  time = c(2, 28,    # <-- changed 30 to 28 so it doesn't overlap
           5, 10, 15, 20, 25, 30),
  addition = c(19, 11, 
               5, 5, 5, 5, 5, 5),
  Fire_Regime = c(rep("High-intensity, low-frequency", 2),
                  rep("Low-intensity, high-frequency", 6)),
  stringsAsFactors = FALSE
)

# ------------------------------
# Figure 1a: Bars (Additions per event)
# ------------------------------
p1a <- ggplot(df, aes(x = time, y = addition, fill = Fire_Regime)) +
  geom_col(width = 1, position = "dodge") +
  labs(x = "Time", y = "PyC Addition") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 14)

# ------------------------------
# Figure 1b: Stepwise cumulative inputs
# ------------------------------
df_cumulative <- df %>%
  group_by(Fire_Regime) %>%
  arrange(time) %>%
  mutate(cumulative = cumsum(addition))

p1b <- ggplot(df_cumulative, aes(x = time, y = cumulative, color = Fire_Regime)) +
  geom_step(linewidth = 1.2) +
  labs(x = "Time", y = "Cumulative PyC Input") +
  scale_color_brewer(palette = "Set2") +
  theme_minimal(base_size = 14)

# ------------------------------
# Combined version: Bars + cumulative line
# ------------------------------
p_combined <- ggplot(df_cumulative, aes(x = time)) +
  geom_step(aes(y = cumulative, color = Fire_Regime),
            linewidth = 1.2) +
  geom_col(aes(y = addition, fill = Fire_Regime),
           width = 1, position = "dodge", alpha = 0.6) +
  labs(x = "Time", y = "PyC Addition & Cumulative Input",
       fill = "Fire Regime",
       color = "Fire Regime") +
  scale_color_manual(values = c("High-intensity, low-frequency" = "darkgreen",
                                "Low-intensity, high-frequency" = "lightgreen")) +
  scale_fill_manual(values = c("High-intensity, low-frequency" = "darkgreen",
                               "Low-intensity, high-frequency" = "lightgreen")) +
  theme_minimal(base_size = 14) +
  scale_y_continuous(expand = c(0, 0))+
  theme(
    legend.position = c(0.4, 0.85),
    panel.grid = element_blank(),      # remove all grid lines
    axis.text = element_blank(),       # remove axis numbers
    axis.ticks = element_blank(),      # remove tick marks
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8) # add outside border
  )


# Print plots
p1a
p1b
p_combined
#ggsave("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/manuscript_test/figure_drafts/p_combined.tiff", plot = p_combined, width = 4, height = 5)



##overlap decay 1 and decay 2
library(ggplot2)
library(dplyr)

# Time sequence
time <- seq(0, 100, by = 1)

# Parameters
fire1 <- 5
fire2 <- 50
add1 <- 50
add2 <- 20
loss_fraction <- 0.3
k1 <- 0.02   # Decay rate before fire 2
k2 <- 0.02  # Slower decay after fire 2 (more resistant)

# First-fire PyC stock over time (switch decay rate at fire2)
decay1 <- numeric(length(time))

for (i in seq_along(time)) {
  t <- time[i]
  if (t < fire1) {
    decay1[i] <- 0
  } else if (t >= fire1 && t < fire2) {
    decay1[i] <- add1 * exp(-k1 * (t - fire1))
  } else {
    remaining_at_fire2 <- add1 * exp(-k1 * (fire2 - fire1))
    remaining_after_loss <- remaining_at_fire2 * (1 - loss_fraction)
    decay1[i] <- remaining_after_loss * exp(-k2 * (t - fire2))
  }
}

# Second-fire PyC stock
decay2 <- ifelse(time >= fire2, add2 * exp(-k1 * (time - fire2)), 0)

# Net stock
net_stock <- decay1 + decay2

df <- data.frame(time, decay1, decay2, net_stock)

ggplot(df, aes(x = time)) +
  # First-fire PyC decay (continuous area)
  geom_area(aes(y = -decay1), fill = "darkgreen", alpha = 0.6) +
  # Second-fire PyC decay
  geom_area(aes(y = -decay2), fill = "lightgreen", alpha = 0.5) +
  # Instant loss at fire2 (negative bar)
  geom_col(aes(y = ifelse(time == fire2, -loss_fraction * add1 * exp(-k1 * (fire2 - fire1)), 0)),
           fill = "darkgreen", width = 3) +
  # Additions as bars
  geom_col(aes(y = ifelse(time == fire1, add1, 0)), fill = "darkgreen", width = 3) +
  geom_col(aes(y = ifelse(time == fire2, add2, 0)), fill = "lightgreen", width = 3) +
  # Net stock line
  geom_line(aes(y = net_stock), color = "red", size = 1) +
  geom_hline(yintercept = 0, color = "darkgreen") +
  theme_minimal(base_size = 14) +
  labs(x = "", y = "") +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
  ) 
#ggsave("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/manuscript_test/figure_drafts/netstock.tiff", width = 5, height = 4)


# stack decay 1 and decay 2

library(ggplot2)
library(dplyr)
library(tidyr)

# Time sequence
time <- seq(0, 100, by = 1)

# Parameters (you can tweak)
fire1 <- 5
fire2 <- 50
add1 <- 50
add2 <- 20
loss_fraction <- 0.3
k1 <- 0.02   # decay rate for newly produced PyC
k2 <- 0.02   # decay rate for older/remaining PyC after fire2

# Compute remaining amount of first-fire PyC at fire2 (before instant loss)
remaining_at_fire2 <- add1 * exp(-k1 * (fire2 - fire1))
remaining_after_loss <- remaining_at_fire2 * (1 - loss_fraction)

# First-fire stock over time: k1 until fire2, then remaining_after_loss decays with k2
decay1 <- ifelse(
  time < fire1, 
  0,
  ifelse(
    time < fire2,
    add1 * exp(-k1 * (time - fire1)),
    remaining_after_loss * exp(-k2 * (time - fire2))
  )
)

# Second-fire stock (new PyC), decays with k1 after fire2
decay2 <- ifelse(time >= fire2, add2 * exp(-k1 * (time - fire2)), 0)

# Net stock (what you plot as the red line)
net_stock <- decay1 + decay2

# Instant combustion loss at fire2 (negative bar)
instant_loss_vec <- ifelse(time == fire2, - (loss_fraction * remaining_at_fire2), 0)

# Dataframe
df <- data.frame(time, decay1, decay2, net_stock, instant_loss = instant_loss_vec)

# Long form for stacking: IMPORTANT -> put decay2 first so it's plotted below decay1
df_long <- df %>%
  pivot_longer(cols = c(decay2, decay1), names_to = "source", values_to = "value") %>%
  mutate(source = factor(source, levels = c("decay2", "decay1")),  # decay2 below decay1
         value = -value)  # negative so areas plot below the zero line

# Plot
ggplot() +
  # stacked loss areas (light green BELOW, dark green ABOVE)
  geom_area(data = df_long, aes(x = time, y = value, fill = source), position = "stack", alpha = 0.7) +
  scale_fill_manual(values = c("decay2" = "lightgreen", "decay1" = "darkgreen")) +
  # instant loss at fire2 (negative bar)
  geom_col(data = df, aes(x = time, y = instant_loss),
           inherit.aes = FALSE, fill = "darkgreen", width = 3) +
  # additions as vertical bars above zero
  geom_col(data = df, aes(x = time, y = ifelse(time == fire1, add1, 0)),
           inherit.aes = FALSE, fill = "darkgreen", width = 3) +
  geom_col(data = df, aes(x = time, y = ifelse(time == fire2, add2, 0)),
           inherit.aes = FALSE, fill = "lightgreen", width = 3) +
  # net stock line (red)
  geom_line(data = df, aes(x = time, y = net_stock), color = "red", size = 1.1) +
  geom_hline(yintercept = 0, color = "darkgreen") +
  theme_minimal(base_size = 14) +
  labs(x = "", y = "") +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.9),
    legend.position = "none"
  )
#ggsave("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/manuscript_test/figure_drafts/netstock.tiff", width = 5, height = 4)

# Thea's update

library(ggplot2)
library(dplyr)

# Time sequence
time <- seq(0, 100, by = 1)

# Parameters
fire1 <- 5
fire2 <- 50
add1 <- 50
add2 <- 20
loss_fraction <- 0.3
k1 <- 0.015
k2 <- 0.015

# Compute remaining amount of first-fire PyC at fire2 (before instant loss)
remaining_at_fire2 <- add1 * exp(-k1 * (fire2 - fire1))
remaining_after_loss <- remaining_at_fire2 * (1 - loss_fraction)

# First-fire stock over time
decay1 <- ifelse(
  time < fire1, 0,
  ifelse(time < fire2,
         add1 * exp(-k1 * (time - fire1)),
         remaining_after_loss * exp(-k2 * (time - fire2)))
)

# Second-fire stock
decay2 <- ifelse(time >= fire2, add2 * exp(-k1 * (time - fire2)), 0)

# Net stock
net_stock <- decay1 + decay2

# Dataframe for plotting
df <- data.frame(time, decay1, decay2, net_stock) %>%
  mutate(ymin_dark = 0,
         ymax_dark = decay1,
         ymin_light = decay1,
         ymax_light = decay1 + decay2)

# Plot
ggplot(df, aes(x = time)) +
  # dark green (first fire)
  geom_ribbon(aes(ymin = ymin_dark, ymax = ymax_dark),
              fill = "darkgreen", alpha = 0.6) +
  # light green stacked on top (second fire)
  geom_ribbon(aes(ymin = ymin_light, ymax = ymax_light),
              fill = "lightgreen", alpha = 0.6) +
  # red net stock line
  geom_line(aes(y = net_stock), color = "red", linewidth = 1.1) +
  theme_minimal(base_size = 14) +
  labs(x = "Time", y = "Remaining PyC Stock") +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.9),
    legend.position = "none"
  )

#ggsave("~/Library/CloudStorage/Box-Box/WhitmanLab/Projects/Repeated_Fire/manuscript_test/figure_drafts/netstock_edited.tiff", width = 5, height = 4)


