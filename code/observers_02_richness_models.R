
library(tidyverse)
library(lubridate)
library(mgcv)
library(merTools)
library(lme4)

data_root <- "data/"
figures_root <- "figures/"


# --------------------------------------------------------------------
# read in richness and checklist info

data_loc <- str_glue("{data_root}/richness_ny_may.csv")
rimay <- read_csv(data_loc)


# --------------------------------------------------------------------
# randomly subset to fewer observers for computational efficiency

set.seed(46)
all_obs <- names(table(rimay$observer_id))
sub_obs <- sample(all_obs, size = 2000, replace = FALSE)

rimay_sub <- rimay %>% 
		filter(observer_id %in% sub_obs) %>%
		mutate(observer_id = factor(observer_id)) %>%
		as.data.frame()

nrow(rimay_sub)
# 43012

# --------------------------------------------------------------------
# model for the whole population

pop1 <- glm(number_of_species ~ effort_hrs + effort_hrs_sqrt + year_seq, data = rimay_sub, family = poisson)

nd <- expand.grid(effort_hrs = 1, effort_hrs_sqrt = 1, year_seq = 1:19)
p_pop1 <- predict(pop1, nd, type = "link", se.fit = TRUE)
est_pop1 <- exp(p_pop1$fit)
lcl_pop1 <- exp(p_pop1$fit - 1.96*p_pop1$se.fit)
ucl_pop1 <- exp(p_pop1$fit + 1.96*p_pop1$se.fit)


# --------------------------------------------------------------------
# model including individual effects

ind1 <- glmer(number_of_species ~ effort_hrs + effort_hrs_sqrt + year_seq + (1|observer_id),
			data = rimay_sub, family = poisson)

pred_obs_levels <- ranef(ind1)$observer_id %>%
		rownames_to_column(var = "observer_id") %>%
		rename("re_intercept" = "(Intercept)")

nd_ind1 <- data.frame(Intercept = 1, effort_hrs = 1, effort_hrs_sqrt = 1, year_seq = 1:19) %>% as.matrix() 
p_ind1_sub <- nd_ind1 %*% fixef(ind1)
p_ind1_var <- diag(nd_ind1 %*% tcrossprod(vcov(ind1),nd_ind1))
p_ind1_sd <- sqrt(p_ind1_var)

p_ind1 <- data.frame(log_fit = p_ind1_sub, log_lwr = p_ind1_sub - 1.96*p_ind1_sd, log_upr = p_ind1_sub + 1.96*p_ind1_sd) %>%
		mutate(fit = exp(log_fit), lwr = exp(log_lwr), upr = exp(log_upr))

# find an observer with average numbers (on the real scale)
aim <- log(mean(est_pop1))
nd_ind2 <- data.frame(Intercept = 1, effort_hrs = 1, effort_hrs_sqrt = 1, year_seq = 10) %>% as.matrix()
p_ind2_sub <- nd_ind2 %*% fixef(ind1)
obs_aim <- aim - p_ind2_sub
w_obs <- which(abs(pred_obs_levels$re_intercept - c(obs_aim)) == min(abs(pred_obs_levels$re_intercept - c(obs_aim))))

# add the observer-specific random effect to the fixed effects predictions. 
re_obs <- pred_obs_levels$re_intercept[w_obs]
p_ind2 <- p_ind1 %>% 
			dplyr::select(log_fit, log_lwr, log_upr) %>%
			mutate(log_fit = log_fit + re_obs, log_lwr = log_lwr + re_obs, log_upr = log_upr + re_obs)  %>%
			mutate(fit = exp(log_fit), lwr = exp(log_lwr), upr = exp(log_upr))



# --------------------------------------------------------------------
# when do observers first appear in the NY may data? 

first_may_ny <- rimay_sub %>% 
				dplyr::select(observer_id, year_seq) %>%
				group_by(observer_id) %>%
				summarise(first_year = min(year_seq), .groups = "drop")

# combine with observer random effects from ind1 

obs_summary <- pred_obs_levels %>%
				left_join(first_may_ny)

bp <- boxplot(re_intercept ~ first_year, data = obs_summary, plot = FALSE, range = 0)



# --------------------------------------------------------------------
# plot for paper

date <- Sys.Date()
plot_loc <- str_glue("{figures_root}/fig_observer_years_{date}.png")
png(plot_loc, width = 15, height = 9, units = "cm", pointsize = 9, res = 600)

	par(mfrow=c(1,2))

	## panel 1

	xlimits = c(-1, 19)
	ylimits = c(18, 26)
	cols <- c("steelblue2", "firebrick2")

	plot(0, 0, col="white", xlim = xlimits, ylim = ylimits, 
		xlab = "Year", ylab = "Expected species per hour in May NY", 
		xaxt= "n", yaxt="n")
	axis(side = 1, at = c(-1, 9, 19), labels = c(2000, 2010, 2020))
	axis(side = 2, at = c(20, 25))

	# population
	polygon(x = c(1:19, rev(1:19), 1), y = c(lcl_pop1, rev(ucl_pop1), lcl_pop1[1]), 
		col = alpha(cols[1], 0.2), border = alpha("white", 0))
	lines(x = 1:19, y = est_pop1, col = cols[1])

	# individual
	polygon(x = c(1:19, rev(1:19), 1), y = c(p_ind2$lwr, rev(p_ind2$upr), p_ind2$lwr[1]), 
		col = alpha(cols[2], 0.2), border = alpha("white", 0))
	lines(x = 1:19, y = p_ind2$fit, col = cols[2])


	# legend

	legend(x = 1, y = 19.5, 
			fill = alpha(cols, 0.2), col = cols, border = alpha("white", 0), 
			bty = "n",
			legend = c("Population level", "Individual level"))
	segments( x0 = 2.3, x1 = 3.1, y0 = c(18.9, 18.3), y1 = c(18.9, 18.3),
		col = cols)

	text(x = -1, y = ylimits[1] + (ylimits[2]-ylimits[1])*0.95, pos = 4, labels = "a", font = 2)


	## panel 2

	plot(0, 0, col="white", ylim=c(-2, 2), xlim=c(-1, 19), xaxt = "n", xlab = "First year of May NY submissions", ylab = "Observer random effect")
	a <- 0.4
	for(i in 1:ncol(bp$stats)){
		polygon(x = c(i - a, i + a, i + a, i - a, i - a),
				y = c(bp$stats[2,i], bp$stats[2,i], bp$stats[4,i], bp$stats[4,i], bp$stats[2,i]),
				col = "grey80", border = "white")	
		segments(x0 = i -a*0.6, x1 = i + a*0.6, y0 = bp$stats[3,i], y1 = bp$stats[3,i], lwd =2, col = "grey30")
		segments(x0 = i, x1 = i, y0 = bp$stats[4, i], y1 = bp$stats[5,i], col = "grey30")
		segments(x0 = i, x1 = i, y0 = bp$stats[2, i], y1 = bp$stats[1,i], col = "grey30")
	}
	axis(side = 1, at = c(-1, 9, 19), labels = c("2000", "2010", "2020"))
	text(x = -1, y = -2 + 4*0.95, pos = 4, labels = "b", font = 2)

dev.off()



