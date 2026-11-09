suppressMessages({library(glmmTMB); library(dplyr); library(tidyr); library(ggplot2); library(splines); library(MASS)})
set.seed(42)

d <- read.csv("C:/Users/ebamg/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan/kano_ibadan_ento/wet_hourly_biting_data.csv")

hl <- c("6-7pm","7-8pm","8-9pm","9-10pm","10-11pm","11-12am","12-1am","1-2am","2-3am","3-4am","4-5am","5-6am")

d <- d %>% mutate(ward=factor(ward), location=factor(location, levels=c("Indoor","Outdoor")),
  hour=factor(hour, levels=hl), hour_num=as.numeric(hour)-1, hh_id=factor(hh_id), night_id=factor(night_id),
  hhnight=interaction(hh_id, night_id, drop=TRUE), trapnight=interaction(hh_id, night_id, location, drop=TRUE))
m <- glmmTMB(n_mosq ~ ward*location + ns(hour_num,2)*ward*location + (1|hh_id) + (1|trapnight),
             family=nbinom2(), data=d)
print(summary(m))

## ---- Diagnostics via simulation (DHARMa not installable here) ----
nsim <- 1000
S <- simulate(m, nsim=nsim)
obs <- d$n_mosq
chk <- function(f) { o <- f(obs); s <- apply(S,2,f); c(observed=o, sim_mean=mean(s), p_two_sided=min(1,2*min(mean(s>=o),mean(s<=o)))) }
cat("\nPosterior-predictive style checks (observed vs 1000 simulated datasets)\n")
print(rbind(zeros=chk(function(y) sum(y==0)), total=chk(sum), max=chk(max),
            var_to_mean=chk(function(y) var(y)/mean(y)),
            nonzero_trapnights=chk(function(y) sum(tapply(y,d$trapnight,sum)>0)),
            trapnight_max=chk(function(y) max(tapply(y,d$trapnight,sum)))))
for(w in levels(d$ward)) for(l in levels(d$location)) { i <- d$ward==w & d$location==l
  cat(sprintf("%s %s total: obs=%d, sim mean=%.1f [95%% %s]\n", w, l, sum(obs[i]), mean(colSums(S[i,])),
      paste(quantile(colSums(S[i,]),c(.025,.975)),collapse="-"))) }
# randomized quantile residuals (DHARMa-style) 
rq <- sapply(seq_len(nrow(d)), function(i){ lo <- mean(S[i,]<obs[i]); hi <- mean(S[i,]<=obs[i]); runif(1,lo,hi)})
cat("\nKS test of residual uniformity: p =", signif(ks.test(rq,"punif")$p.value,3), "\n")
# residual by trap-night (hour-clustering) 
tn <- tapply(rq, d$trapnight, mean); cat("trap-night mean residual sd (expect ~0.083):", round(sd(tn),3), "\n")
# hour-to-hour autocorrelation in residuals within trap-night
ac <- d %>% mutate(r=qnorm(pmin(pmax(rq,1e-3),1-1e-3))) %>% arrange(trapnight,hour_num) %>% group_by(trapnight) %>%
  mutate(rl=lag(r)) %>% ungroup() %>% filter(!is.na(rl)); cat("lag-1 residual correlation:", round(cor(ac$r,ac$rl),3), "\n")

## ---- Predictions ----
grid <- expand.grid(ward=levels(d$ward), location=levels(d$location), hour=hl, stringsAsFactors=FALSE) %>%
  mutate(ward=factor(ward,levels=levels(d$ward)), location=factor(location,levels=levels(d$location)),
         hour=factor(hour,levels=hl), hour_num=as.numeric(hour)-1)
# design matrix for fixed effects at grid points
X <- model.matrix(delete.response(terms(m, component="cond", fixed.only=TRUE)), grid) 
beta <- fixef(m)$cond
V <- vcov(m, full=TRUE)           # joint covariance: betas, dispersion, RE log-SDs
nb <- length(beta); th_hat <- m$sdr$par.fixed[(nb+2):(nb+3)]   # log-SDs: hh_id, trapnight
stopifnot(all(grepl("theta", names(m$sdr$par.fixed)[(nb+2):(nb+3)])))
mu_full <- c(beta, th_hat); Vk <- V[c(1:nb,(nb+2):(nb+3)), c(1:nb,(nb+2):(nb+3))]
mm0 <- NULL
B <- 4000
draw <- MASS::mvrnorm(B, mu_full, Vk)
Bd <- draw[, seq_along(beta)]; Td <- draw[, length(beta)+seq_along(th_hat), drop=FALSE]
sig2 <- rowSums(exp(2*Td))        # total RE variance (log scale) per draw (all intercept-only RE)
eta <- Bd %*% t(X)                 # B x n_grid
marg <- exp(eta + sig2/2)          # marginal mean (lognormal correction)
cond0 <- exp(eta)                  # typical household/trap-night (RE=0)
q <- function(M) t(apply(M,2,quantile,c(.5,.025,.975)))
mm <- q(marg); grid$rate <- exp(X %*% beta + sum(exp(2*th_hat))/2)[,1]
grid$lcl <- mm[,2]; grid$ucl <- mm[,3]
grid$rate_typical <- exp(X%*%beta)[,1]
cat("\nTotal RE variance (log scale):", round(sum(exp(2*th_hat)),3), " -> marginal/typical ratio:", round(exp(sum(exp(2*th_hat))/2),2), "\n")
write.csv(grid %>% dplyr::select(ward,location,hour,rate,lcl,ucl,rate_typical), "hourly_rates_by_ward_location.csv", row.names=FALSE)

## ---- Summaries: mean hourly rate and per-night total (12 h), by ward x location, plus ward rate ratios ----
sumdraw <- function(idx) rowMeans(marg[, idx, drop=FALSE])
res <- list(); RR <- list()
for(l in levels(d$location)) {
  dr <- sapply(levels(d$ward), function(w) sumdraw(which(grid$ward==w & grid$location==l)))
  for(w in levels(d$ward)) res[[paste(w,l)]] <- data.frame(ward=w, location=l, mean_hourly=median(dr[,w]), lcl=quantile(dr[,w],.025), ucl=quantile(dr[,w],.975),
      per_night_12h=12*median(dr[,w]), n_lcl=12*quantile(dr[,w],.025), n_ucl=12*quantile(dr[,w],.975))
  r <- dr[,"Challenge"]/dr[,"Agugu"]; RR[[l]] <- data.frame(location=l, contrast="Challenge / Agugu", RR=median(r), lcl=quantile(r,.025), ucl=quantile(r,.975), p_RR_gt1=mean(r>1))
}
res <- bind_rows(res); RR <- bind_rows(RR)
cat("\nMean hourly rate (marginal), averaged across the 12 night hours\n"); print(res, row.names=FALSE, digits=3)
cat("\nWard rate ratios within location (averaged across hours)\n"); print(RR, row.names=FALSE, digits=3)
write.csv(res, "summary_rates.csv", row.names=FALSE); write.csv(RR, "ward_rate_ratios.csv", row.names=FALSE)
# also indoor vs outdoor within ward
IO <- lapply(levels(d$ward), function(w){ a <- sumdraw(which(grid$ward==w&grid$location=="Outdoor")); b <- sumdraw(which(grid$ward==w&grid$location=="Indoor")); r<-a/b
  data.frame(ward=w, contrast="Outdoor / Indoor", RR=median(r), lcl=quantile(r,.025), ucl=quantile(r,.975))}) %>% bind_rows()
cat("\nOutdoor/Indoor within ward\n"); print(IO, row.names=FALSE, digits=3); write.csv(IO,"location_rate_ratios.csv",row.names=FALSE)

## ---- Plot ----
obs_h <- d %>% group_by(ward,location,hour) %>% summarise(obs=mean(n_mosq),.groups="drop")
Fig2B <- ggplot(grid, aes(hour, rate, group=1)) + geom_ribbon(aes(ymin=lcl,ymax=ucl), fill="grey75", alpha=.6) + geom_line(linewidth=.8) +
  geom_point(data=obs_h, aes(y=obs), colour="firebrick", size=1.6) + facet_grid(location~ward) +
  labs(x="Hour of night", y="Anopheles per trap-hour (marginal mean, 95% CI)", caption="Red points = raw observed mean per trap-hour") +
  theme_bw() + theme(axis.text.x=element_text(angle=60,hjust=1))
ggsave("hourly_rates_plot.png", p, width=9, height=6, dpi=200)
saveRDS(m,"final_model.rds")
print(p)

ggsave("hourly_rates_plot.pdf", Fig2B, width = 10, height = 8)
