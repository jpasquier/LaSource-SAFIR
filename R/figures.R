library(dplyr)
library(ggplot2)
library(here)
library(tidyr)
library(wesanderson)

# Working directory
i_am("R/figures.R")

# Source preprocessing script
source(here("R/preprocessing.R"))

# Output directory
outdir <- here("results", paste0("figures_", format(Sys.Date(), "%Y%m%d")))
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

# Figure
vars <- c(
  approach_coping = "Approach Coping",
  avoidant_coping = "Avoidant Coping",
  ff_total = "Family Functioning",
  ps_total = "Perceived Support"
)
phases <- c(ph1 = "Phase 1", ph3 = "Phase 3", fu = "Follow-up")
d <- dta_fm %>%
  select(record_id, phase, approach_coping, avoidant_coping, ff_total,
         ps_total) %>%
  filter(phase %in% c("ph1", "ph3", "fu")) %>%
  pivot_longer(!c(record_id, phase)) %>%
  mutate(
    phase = factor(droplevels(phase), names(phases), phases),
    name = factor(name, names(vars), vars)
  ) %>%
  drop_na(value)
fig1 <- ggplot(d, aes(x = name, y = value, fill = phase)) +
  geom_boxplot(position = position_dodge(width = .5), width = .4) +
  labs(x = NULL, y = "Score", fill = NULL) +
  scale_fill_manual(values = wes_palette(name = "Darjeeling1", n = 3)) +
  theme_bw() +
  theme(legend.position = "bottom")
fig2 <- d %>%
  group_by(name, phase) %>%
  summarise(Mean = mean(value), SD = sd(value), .groups = "drop") %>%
  ggplot(aes(x = name, y = Mean, color = phase)) +
  geom_point(position = position_dodge(width = .4)) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                position = position_dodge(width = .4), width = .2) +
  labs(x = NULL, y = "Mean Score ± SD", color = NULL) +
  scale_color_manual(values = wes_palette(name = "Darjeeling1", n = 3)) +
  theme_bw() +
  theme(legend.position = "bottom")

# Export figures
tiff(file.path(outdir, "boxplots.tiff"), width = 2400, height = 1800,
     res = 480, compression = "zip")
print(fig1)
dev.off()
tiff(file.path(outdir, "means.tiff"), width = 2400, height = 1800,
     res = 480, compression = "zip")
print(fig2)
dev.off()
