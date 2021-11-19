

library(tidyverse)
library(lubridate)
library(data.table)
library(mgcv)

data_root <- "data/"
figures_root <- "figures/"
dir.create(figures_root)

# --------------------------------------------------------------------
# read in species observations on checklists complete or incomplete

d_loc <- str_glue("{data_root}/species_complete_incomplete_NY_2010_2020.txt")
d <- read.table(d_loc, header = TRUE)

d2 <- d %>%
	filter(no_complete >= 10, no_incomplete >= 10) %>%
	mutate(interest = prop_incomplete / prop_complete)

d3 <- d2 %>%
	mutate(log10_prop_complete = log10(prop_complete)) %>%
	mutate(log10_interest = log10(interest))
g1 <- gam(log10_interest ~ s(log10_prop_complete, k = 6), data = d3)

nd <- data.frame(log10_prop_complete = seq(min(d3$log10_prop_complete), max(d3$log10_prop_complete), length.out = 100))
p1 <- predict(g1, nd, se.fit = TRUE)


# --------------------------------------------------------------------
# plot species 'interest' (incomplete:complete ratio) against species prevalence


date <- Sys.Date()

plot_loc <- str_glue("{figures_root}/complete_vs_interest_{date}.png")
png(plot_loc, width = 10, height = 10, units = "cm", pointsize = 9, res = 600)
	par(mar = c(5, 5, 1, 1))
	plot(log10(d2$prop_complete), log10(d2$interest), xaxt = "n", yaxt = "n", ylim = c(-2, 0),
		xlab = "Species prevalence on complete checklists", ylab = "Incomplete:complete checklist ratio",
		pch = 16, col = "white")
	axis(side = 1, at = c(-5:-1), labels = c(0.00001, 0.0001, 0.001, 0.01, 0.1))
	axis(side = 2, at = c(0, -1, -2), labels = 10^c(0, -1, -2))
	polygon(x = c(nd$log10_prop_complete, rev(nd$log10_prop_complete), nd$log10_prop_complete[1]),
			y = c(p1$fit - 1.96*p1$se.fit, rev(p1$fit + 1.96*p1$se.fit), p1$fit[1] - 1.96*p1$se.fit[1]),
			col = "grey90", border = alpha("white", 0))
	lines(nd$log10_prop_complete, p1$fit)
	points(log10(d2$prop_complete), log10(d2$interest), 
		pch = 16, col = alpha("black", 0.3))
dev.off()


# identify(log10(d2$prop_complete), log10(d2$interest), d2$species, cex=0.5)
