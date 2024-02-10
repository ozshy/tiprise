# tiprise_2024_x_y.R 

# packages used
library(ggplot2); theme_set(theme_bw())
library(latex2exp)# LaTeX in ggplot
library(xtable)# export data frames to LaTeX tables

#Figure 1 (calibrations) begins####

(phi=0.553)# frac of high tippers
(muh = 0.2)# high-tippers' markup
(mul = 0.2)# high-tippers' markdown (negative)

# population growth https://www.census.gov/data/tables/time-series/dec/popchange-data-text.html (note: includes additions and subractions, e.g., birth and dealth, immigration, etc.)
pop_1920 = 106021568
pop_1950 = 151325798
pop_2020 = 331449281
(cagr_year = (pop_2020/pop_1950)^(1/70)-1)
(cagr_decade = (pop_2020/pop_1950)^(1/7)-1)

# US death: https://www.macrotrends.net/countries/USA/united-states/death-rate 1950-2020
(death_rate_1950 = 9.649/1000)# \lambda in paper
(death_rate_2020 = 8.880/1000)# \lambda in paper
(death_year_rate_avg = (death_rate_1950+death_rate_2020)/2) # \lambda in paper
(death_decade_rate_avg = 10*(death_rate_1950+death_rate_2020)/2) # \lambda in paper


# inferring entering consumer rates by adding death rate to population growth
(enter_year_rate= cagr_year + death_year_rate_avg)# \eta in paper
(enter_decade_rate= cagr_decade + death_decade_rate_avg)# \eta in paper

# for the simulation below define
# yearly basis
(lambda_year_rate =  death_year_rate_avg )
(eta_year_rate = enter_year_rate)
# decade basis
(lambda_decade_rate =  death_decade_rate_avg )
(eta_decade_rate = enter_decade_rate)

# build the nht, nlt, and nt sequences in general time intervals
(t.vec=seq(0,70,1))
length(t.vec)

## build the nht, nlt, and nt sequences in general time intervals
# build the nht, nlt, and nt sequences in general time intervals
(t.vec=seq(1,70,1))
length(t.vec)

# define population sequences (time zero left out, to be added later before drawing)
(n.vec = as.numeric(rep(NA, length(t.vec))))
length(n.vec)
(n0 = 100)# n_0 in paper
for (t in t.vec) {
  n.vec[t] = n0*(1+eta_year_rate-lambda_year_rate)^t
}
n.vec #total pop

# high tipper pop
(nh.vec = as.numeric(rep(NA, length(t.vec))))
length(nh.vec)
(nh0 = n0/2)# nh_0 in paper
(nh.vec[1] = (1-lambda_year_rate)*nh0 + n0*eta_year_rate*phi)
for (t in 2:length(t.vec)) {
  nh.vec[t] = (1-lambda_year_rate)*nh.vec[t-1] + n0*eta_year_rate*phi*(1+eta_year_rate-lambda_year_rate)^(t-1)
}
nh.vec

# low tipper pop
(nl.vec = as.numeric(rep(NA, length(t.vec))))
length(nl.vec)
(nl0 = n0/2)# nl_0 in paper
(nl.vec[1] = (1-lambda_year_rate)*nl0 + n0*eta_year_rate*(1-phi))
for (t in 2:length(t.vec)) {
  nl.vec[t] = (1-lambda_year_rate)*nl.vec[t-1] + n0*eta_year_rate*(1-phi)*(1+eta_year_rate-lambda_year_rate)^(t-1)
}
nl.vec

# testing
n.vec

# Simulating Result 1 in paper
nh.vec+nl.vec
nh.vec/nl.vec
nl.vec/nh.vec
muh/mul

# avg tipping rate over time
(r0 = 0.1)
r.vec = as.numeric(rep(NA, length(t.vec)))
rh.vec = as.numeric(rep(NA, length(t.vec)))
rl.vec = as.numeric(rep(NA, length(t.vec)))
#
(rh.vec[1] = (1+muh)*r0)# eq (1) in paper
(rl.vec[1] = (1-mul)*r0)
(r.vec[1] = r0)
#
for (t in 2:length(t.vec)) {
  rh.vec[t] = (1+muh)*r.vec[t-1];
  rl.vec[t] = (1-muh)*r.vec[t-1];
  r.vec[t] = (nh.vec[t]*rh.vec[t] + nl.vec[t]*rl.vec[t])/(n.vec[t])
}

(year.vec = seq(1951, 2020, 1))
data.frame(year.vec, r.vec, rh.vec, rl.vec)

# preparing a data frame
(r.df = data.frame(t.vec, year.vec, r.vec, rh.vec, rl.vec))
dim(r.df)

# Plot Figure 1 in the paper
ggplot(r.df, aes(x=year.vec)) + geom_line(aes(y=100*r.vec), linetype="solid", size=1.2, color="black") + geom_line(aes(y=100*rh.vec), linetype="longdash", size=1.2, color="red") + geom_line(aes(y=100*rl.vec), linetype="dotdash", size=1.2, color="blue") + scale_x_continuous(breaks = seq(1950,2020,5)) + scale_y_continuous(breaks = seq(0,24,1)) +labs(x="Year", y="Tipping rates (%)")  +theme(axis.text.x = element_text(size = 14, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) +annotate("text", x = 1990, y = 14.0, label =TeX("$\\hat{r}_t$"), size = 8, color="black") +annotate("text", x = 1990, y = 16.5, label =TeX("$\\hat{r}_t^H$"), size = 8, color="red") +annotate("text", x = 1990, y = 11.4, label =TeX("$\\hat{r}_t^L$"), size = 8, color="blue") +geom_segment(aes(x=2020, y=8, xend=2020, yend=20), linetype="dotted", size=1.2) +geom_segment(aes(x=1950, y=20, xend=2020, yend=20), linetype="dotted", size=1.2) +geom_segment(aes(x=2013, y=8, xend=2013, yend=18), linetype="dotted", size=1.2) +geom_segment(aes(x=1950, y=18, xend=2013, yend=18), linetype="dotted", size=1.2) +geom_segment(aes(x=2001, y=8, xend=2001, yend=15), linetype="dotted", size=1.2) +geom_segment(aes(x=1950, y=15, xend=2001, yend=15), linetype="dotted", size=1.2)

#Figure 1 (calibrations) ends####

#Table (calibrations) 1 begins####

### Constructing table muh, mul, versus phi yielding r=20% in 2020 [note: Figure 1 used muh=mul=0.2]

## muh=mul = 0.1
(phi=0.607)# frac of high tippers
(muh = 0.1)# high-tippers' markup
(mul = 0.1)# high-tippers' markdown (negative)

# define population sequences (time zero left out, to be added later before drawing)
(n.vec = as.numeric(rep(NA, length(t.vec))))
length(n.vec)
(n0 = 100)# n_0 in paper
for (t in t.vec) {
  n.vec[t] = n0*(1+eta_year_rate-lambda_year_rate)^t
}
n.vec #total pop

# high tipper pop
(nh.vec = as.numeric(rep(NA, length(t.vec))))
length(nh.vec)
(nh0 = n0/2)# nh_0 in paper
(nh.vec[1] = (1-lambda_year_rate)*nh0 + n0*eta_year_rate*phi)
for (t in 2:length(t.vec)) {
  nh.vec[t] = (1-lambda_year_rate)*nh.vec[t-1] + n0*eta_year_rate*phi*(1+eta_year_rate-lambda_year_rate)^(t-1)
}
nh.vec

# low tipper pop
(nl.vec = as.numeric(rep(NA, length(t.vec))))
length(nl.vec)
(nl0 = n0/2)# nl_0 in paper
(nl.vec[1] = (1-lambda_year_rate)*nl0 + n0*eta_year_rate*(1-phi))
for (t in 2:length(t.vec)) {
  nl.vec[t] = (1-lambda_year_rate)*nl.vec[t-1] + n0*eta_year_rate*(1-phi)*(1+eta_year_rate-lambda_year_rate)^(t-1)
}
nl.vec

# avg tipping rate over time
(r0 = 0.1)
r.vec = as.numeric(rep(NA, length(t.vec)))
rh.vec = as.numeric(rep(NA, length(t.vec)))
rl.vec = as.numeric(rep(NA, length(t.vec)))
#
(rh.vec[1] = (1+muh)*r0)# eq (1) in paper
(rl.vec[1] = (1-mul)*r0)
(r.vec[1] = r0)
#
for (t in 2:length(t.vec)) {
  rh.vec[t] = (1+muh)*r.vec[t-1];
  rl.vec[t] = (1-muh)*r.vec[t-1];
  r.vec[t] = (nh.vec[t]*rh.vec[t] + nl.vec[t]*rl.vec[t])/(n.vec[t])
}
data.frame(year.vec, r.vec, rh.vec, rl.vec)

## muh=mul = 0.2
(phi=0.553)# frac of high tippers
(muh = 0.2)# high-tippers' markup
(mul = 0.2)# high-tippers' markdown (negative)

# define population sequences (time zero left out, to be added later before drawing)
(n.vec = as.numeric(rep(NA, length(t.vec))))
length(n.vec)
(n0 = 100)# n_0 in paper
for (t in t.vec) {
  n.vec[t] = n0*(1+eta_year_rate-lambda_year_rate)^t
}
n.vec #total pop

# high tipper pop
(nh.vec = as.numeric(rep(NA, length(t.vec))))
length(nh.vec)
(nh0 = n0/2)# nh_0 in paper
(nh.vec[1] = (1-lambda_year_rate)*nh0 + n0*eta_year_rate*phi)
for (t in 2:length(t.vec)) {
  nh.vec[t] = (1-lambda_year_rate)*nh.vec[t-1] + n0*eta_year_rate*phi*(1+eta_year_rate-lambda_year_rate)^(t-1)
}
nh.vec

# low tipper pop
(nl.vec = as.numeric(rep(NA, length(t.vec))))
length(nl.vec)
(nl0 = n0/2)# nl_0 in paper
(nl.vec[1] = (1-lambda_year_rate)*nl0 + n0*eta_year_rate*(1-phi))
for (t in 2:length(t.vec)) {
  nl.vec[t] = (1-lambda_year_rate)*nl.vec[t-1] + n0*eta_year_rate*(1-phi)*(1+eta_year_rate-lambda_year_rate)^(t-1)
}
nl.vec

# avg tipping rate over time
(r0 = 0.1)
r.vec = as.numeric(rep(NA, length(t.vec)))
rh.vec = as.numeric(rep(NA, length(t.vec)))
rl.vec = as.numeric(rep(NA, length(t.vec)))
#
(rh.vec[1] = (1+muh)*r0)# eq (1) in paper
(rl.vec[1] = (1-mul)*r0)
(r.vec[1] = r0)
#
for (t in 2:length(t.vec)) {
  rh.vec[t] = (1+muh)*r.vec[t-1];
  rl.vec[t] = (1-muh)*r.vec[t-1];
  r.vec[t] = (nh.vec[t]*rh.vec[t] + nl.vec[t]*rl.vec[t])/(n.vec[t])
}
data.frame(year.vec, r.vec, rh.vec, rl.vec)

## muh=mul = 0.3
(phi=0.535)# frac of high tippers
(muh = 0.3)# high-tippers' markup
(mul = 0.3)# high-tippers' markdown (negative)

# define population sequences (time zero left out, to be added later before drawing)
(n.vec = as.numeric(rep(NA, length(t.vec))))
length(n.vec)
(n0 = 100)# n_0 in paper
for (t in t.vec) {
  n.vec[t] = n0*(1+eta_year_rate-lambda_year_rate)^t
}
n.vec #total pop

# high tipper pop
(nh.vec = as.numeric(rep(NA, length(t.vec))))
length(nh.vec)
(nh0 = n0/2)# nh_0 in paper
(nh.vec[1] = (1-lambda_year_rate)*nh0 + n0*eta_year_rate*phi)
for (t in 2:length(t.vec)) {
  nh.vec[t] = (1-lambda_year_rate)*nh.vec[t-1] + n0*eta_year_rate*phi*(1+eta_year_rate-lambda_year_rate)^(t-1)
}
nh.vec

# low tipper pop
(nl.vec = as.numeric(rep(NA, length(t.vec))))
length(nl.vec)
(nl0 = n0/2)# nl_0 in paper
(nl.vec[1] = (1-lambda_year_rate)*nl0 + n0*eta_year_rate*(1-phi))
for (t in 2:length(t.vec)) {
  nl.vec[t] = (1-lambda_year_rate)*nl.vec[t-1] + n0*eta_year_rate*(1-phi)*(1+eta_year_rate-lambda_year_rate)^(t-1)
}
nl.vec

# avg tipping rate over time
(r0 = 0.1)
r.vec = as.numeric(rep(NA, length(t.vec)))
rh.vec = as.numeric(rep(NA, length(t.vec)))
rl.vec = as.numeric(rep(NA, length(t.vec)))
#
(rh.vec[1] = (1+muh)*r0)# eq (1) in paper
(rl.vec[1] = (1-mul)*r0)
(r.vec[1] = r0)
#
for (t in 2:length(t.vec)) {
  rh.vec[t] = (1+muh)*r.vec[t-1];
  rl.vec[t] = (1-muh)*r.vec[t-1];
  r.vec[t] = (nh.vec[t]*rh.vec[t] + nl.vec[t]*rl.vec[t])/(n.vec[t])
}
data.frame(year.vec, r.vec, rh.vec, rl.vec)

## muh=0.1 < mul=0.2
(phi=0.606)# frac of high tippers
(muh = 0.1)# high-tippers' markup
(mul = 0.2)# high-tippers' markdown (negative)

# define population sequences (time zero left out, to be added later before drawing)
(n.vec = as.numeric(rep(NA, length(t.vec))))
length(n.vec)
(n0 = 100)# n_0 in paper
for (t in t.vec) {
  n.vec[t] = n0*(1+eta_year_rate-lambda_year_rate)^t
}
n.vec #total pop

# high tipper pop
(nh.vec = as.numeric(rep(NA, length(t.vec))))
length(nh.vec)
(nh0 = n0/2)# nh_0 in paper
(nh.vec[1] = (1-lambda_year_rate)*nh0 + n0*eta_year_rate*phi)
for (t in 2:length(t.vec)) {
  nh.vec[t] = (1-lambda_year_rate)*nh.vec[t-1] + n0*eta_year_rate*phi*(1+eta_year_rate-lambda_year_rate)^(t-1)
}
nh.vec

# low tipper pop
(nl.vec = as.numeric(rep(NA, length(t.vec))))
length(nl.vec)
(nl0 = n0/2)# nl_0 in paper
(nl.vec[1] = (1-lambda_year_rate)*nl0 + n0*eta_year_rate*(1-phi))
for (t in 2:length(t.vec)) {
  nl.vec[t] = (1-lambda_year_rate)*nl.vec[t-1] + n0*eta_year_rate*(1-phi)*(1+eta_year_rate-lambda_year_rate)^(t-1)
}
nl.vec

# avg tipping rate over time
(r0 = 0.1)
r.vec = as.numeric(rep(NA, length(t.vec)))
rh.vec = as.numeric(rep(NA, length(t.vec)))
rl.vec = as.numeric(rep(NA, length(t.vec)))
#
(rh.vec[1] = (1+muh)*r0)# eq (1) in paper
(rl.vec[1] = (1-mul)*r0)
(r.vec[1] = r0)
#
for (t in 2:length(t.vec)) {
  rh.vec[t] = (1+muh)*r.vec[t-1];
  rl.vec[t] = (1-muh)*r.vec[t-1];
  r.vec[t] = (nh.vec[t]*rh.vec[t] + nl.vec[t]*rl.vec[t])/(n.vec[t])
}
data.frame(year.vec, r.vec, rh.vec, rl.vec)

## muh=0.2 > mul=0.1
(phi=0.553)# frac of high tippers
(muh = 0.2)# high-tippers' markup
(mul = 0.1)# high-tippers' markdown (negative)

# define population sequences (time zero left out, to be added later before drawing)
(n.vec = as.numeric(rep(NA, length(t.vec))))
length(n.vec)
(n0 = 100)# n_0 in paper
for (t in t.vec) {
  n.vec[t] = n0*(1+eta_year_rate-lambda_year_rate)^t
}
n.vec #total pop

# high tipper pop
(nh.vec = as.numeric(rep(NA, length(t.vec))))
length(nh.vec)
(nh0 = n0/2)# nh_0 in paper
(nh.vec[1] = (1-lambda_year_rate)*nh0 + n0*eta_year_rate*phi)
for (t in 2:length(t.vec)) {
  nh.vec[t] = (1-lambda_year_rate)*nh.vec[t-1] + n0*eta_year_rate*phi*(1+eta_year_rate-lambda_year_rate)^(t-1)
}
nh.vec

# low tipper pop
(nl.vec = as.numeric(rep(NA, length(t.vec))))
length(nl.vec)
(nl0 = n0/2)# nl_0 in paper
(nl.vec[1] = (1-lambda_year_rate)*nl0 + n0*eta_year_rate*(1-phi))
for (t in 2:length(t.vec)) {
  nl.vec[t] = (1-lambda_year_rate)*nl.vec[t-1] + n0*eta_year_rate*(1-phi)*(1+eta_year_rate-lambda_year_rate)^(t-1)
}
nl.vec

# avg tipping rate over time
(r0 = 0.1)
r.vec = as.numeric(rep(NA, length(t.vec)))
rh.vec = as.numeric(rep(NA, length(t.vec)))
rl.vec = as.numeric(rep(NA, length(t.vec)))
#
(rh.vec[1] = (1+muh)*r0)# eq (1) in paper
(rl.vec[1] = (1-mul)*r0)
(r.vec[1] = r0)
#
for (t in 2:length(t.vec)) {
  rh.vec[t] = (1+muh)*r.vec[t-1];
  rl.vec[t] = (1-muh)*r.vec[t-1];
  r.vec[t] = (nh.vec[t]*rh.vec[t] + nl.vec[t]*rl.vec[t])/(n.vec[t])
}
data.frame(year.vec, r.vec, rh.vec, rl.vec)

## muh=0.1 < mul=0.3
(phi=0.606)# frac of high tippers
(muh = 0.1)# high-tippers' markup
(mul = 0.3)# high-tippers' markdown (negative)

# define population sequences (time zero left out, to be added later before drawing)
(n.vec = as.numeric(rep(NA, length(t.vec))))
length(n.vec)
(n0 = 100)# n_0 in paper
for (t in t.vec) {
  n.vec[t] = n0*(1+eta_year_rate-lambda_year_rate)^t
}
n.vec #total pop

# high tipper pop
(nh.vec = as.numeric(rep(NA, length(t.vec))))
length(nh.vec)
(nh0 = n0/2)# nh_0 in paper
(nh.vec[1] = (1-lambda_year_rate)*nh0 + n0*eta_year_rate*phi)
for (t in 2:length(t.vec)) {
  nh.vec[t] = (1-lambda_year_rate)*nh.vec[t-1] + n0*eta_year_rate*phi*(1+eta_year_rate-lambda_year_rate)^(t-1)
}
nh.vec

# low tipper pop
(nl.vec = as.numeric(rep(NA, length(t.vec))))
length(nl.vec)
(nl0 = n0/2)# nl_0 in paper
(nl.vec[1] = (1-lambda_year_rate)*nl0 + n0*eta_year_rate*(1-phi))
for (t in 2:length(t.vec)) {
  nl.vec[t] = (1-lambda_year_rate)*nl.vec[t-1] + n0*eta_year_rate*(1-phi)*(1+eta_year_rate-lambda_year_rate)^(t-1)
}
nl.vec

# avg tipping rate over time
(r0 = 0.1)
r.vec = as.numeric(rep(NA, length(t.vec)))
rh.vec = as.numeric(rep(NA, length(t.vec)))
rl.vec = as.numeric(rep(NA, length(t.vec)))
#
(rh.vec[1] = (1+muh)*r0)# eq (1) in paper
(rl.vec[1] = (1-mul)*r0)
(r.vec[1] = r0)
#
for (t in 2:length(t.vec)) {
  rh.vec[t] = (1+muh)*r.vec[t-1];
  rl.vec[t] = (1-muh)*r.vec[t-1];
  r.vec[t] = (nh.vec[t]*rh.vec[t] + nl.vec[t]*rl.vec[t])/(n.vec[t])
}
data.frame(year.vec, r.vec, rh.vec, rl.vec)

## muh=0.3 > mul=0.1
(phi=0.5355)# frac of high tippers
(muh = 0.3)# high-tippers' markup
(mul = 0.1)# high-tippers' markdown (negative)

# define population sequences (time zero left out, to be added later before drawing)
(n.vec = as.numeric(rep(NA, length(t.vec))))
length(n.vec)
(n0 = 100)# n_0 in paper
for (t in t.vec) {
  n.vec[t] = n0*(1+eta_year_rate-lambda_year_rate)^t
}
n.vec #total pop

# high tipper pop
(nh.vec = as.numeric(rep(NA, length(t.vec))))
length(nh.vec)
(nh0 = n0/2)# nh_0 in paper
(nh.vec[1] = (1-lambda_year_rate)*nh0 + n0*eta_year_rate*phi)
for (t in 2:length(t.vec)) {
  nh.vec[t] = (1-lambda_year_rate)*nh.vec[t-1] + n0*eta_year_rate*phi*(1+eta_year_rate-lambda_year_rate)^(t-1)
}
nh.vec

# low tipper pop
(nl.vec = as.numeric(rep(NA, length(t.vec))))
length(nl.vec)
(nl0 = n0/2)# nl_0 in paper
(nl.vec[1] = (1-lambda_year_rate)*nl0 + n0*eta_year_rate*(1-phi))
for (t in 2:length(t.vec)) {
  nl.vec[t] = (1-lambda_year_rate)*nl.vec[t-1] + n0*eta_year_rate*(1-phi)*(1+eta_year_rate-lambda_year_rate)^(t-1)
}
nl.vec

# avg tipping rate over time
(r0 = 0.1)
r.vec = as.numeric(rep(NA, length(t.vec)))
rh.vec = as.numeric(rep(NA, length(t.vec)))
rl.vec = as.numeric(rep(NA, length(t.vec)))
#
(rh.vec[1] = (1+muh)*r0)# eq (1) in paper
(rl.vec[1] = (1-mul)*r0)
(r.vec[1] = r0)
#
for (t in 2:length(t.vec)) {
  rh.vec[t] = (1+muh)*r.vec[t-1];
  rl.vec[t] = (1-muh)*r.vec[t-1];
  r.vec[t] = (nh.vec[t]*rh.vec[t] + nl.vec[t]*rl.vec[t])/(n.vec[t])
}
data.frame(year.vec, r.vec, rh.vec, rl.vec)

#Table 1 (calibrations) ends####

#Figure 2 (simulations) begins####
(t.vec = 1:100)
(simul.df = data.frame(t.vec))# preparing data frame for 2 cases

# Fig2 case 1: falling then rising tips
(n0=100)
(nh0=40)
(nl0=60)
#
(phi=0.6)# frac of high tippers
(muh = 0.2)# high-tippers' markup
(mul = 0.2)# high-tippers' markdown (negative)

# define population sequences (time zero left out, to be added later before drawing)
(n.vec = as.numeric(rep(NA, length(t.vec))))
length(n.vec)
for (t in t.vec) {
  n.vec[t] = n0*(1+eta_year_rate-lambda_year_rate)^t
}
n.vec #total pop

# high tipper pop
(nh.vec = as.numeric(rep(NA, length(t.vec))))
length(nh.vec)
(nh.vec[1] = (1-lambda_year_rate)*nh0 + n0*eta_year_rate*phi)
for (t in 2:length(t.vec)) {
  nh.vec[t] = (1-lambda_year_rate)*nh.vec[t-1] + n0*eta_year_rate*phi*(1+eta_year_rate-lambda_year_rate)^(t-1)
}
nh.vec

# low tipper pop
(nl.vec = as.numeric(rep(NA, length(t.vec))))
length(nl.vec)
(nl.vec[1] = (1-lambda_year_rate)*nl0 + n0*eta_year_rate*(1-phi))
for (t in 2:length(t.vec)) {
  nl.vec[t] = (1-lambda_year_rate)*nl.vec[t-1] + n0*eta_year_rate*(1-phi)*(1+eta_year_rate-lambda_year_rate)^(t-1)
}
nl.vec

# avg tipping rate over time
(r0 = 0.1)
r.vec = as.numeric(rep(NA, length(t.vec)))
rh.vec = as.numeric(rep(NA, length(t.vec)))
rl.vec = as.numeric(rep(NA, length(t.vec)))
#
(rh.vec[1] = (1+muh)*r0)# eq (1) in paper
(rl.vec[1] = (1-mul)*r0)
(r.vec[1] = r0)
#
for (t in 2:length(t.vec)) {
  rh.vec[t] = (1+muh)*r.vec[t-1];
  rl.vec[t] = (1-muh)*r.vec[t-1];
  r.vec[t] = (nh.vec[t]*rh.vec[t] + nl.vec[t]*rl.vec[t])/(n.vec[t])
}
data.frame(t.vec, r.vec, rh.vec, rl.vec)
# insert r.vec into the data frame
simul.df$case1 = r.vec
simul.df

# Fig2 case 2: rising then falling tips
(t.vec = 1:100)
#
(n0=100)
(nh0=60)
(nl0=40)
#
(phi=0.4)# frac of high tippers
(muh = 0.2)# high-tippers' markup
(mul = 0.2)# high-tippers' markdown (negative)

# define population sequences (time zero left out, to be added later before drawing)
(n.vec = as.numeric(rep(NA, length(t.vec))))
length(n.vec)
for (t in t.vec) {
  n.vec[t] = n0*(1+eta_year_rate-lambda_year_rate)^t
}
n.vec #total pop

# high tipper pop
(nh.vec = as.numeric(rep(NA, length(t.vec))))
length(nh.vec)
(nh.vec[1] = (1-lambda_year_rate)*nh0 + n0*eta_year_rate*phi)
for (t in 2:length(t.vec)) {
  nh.vec[t] = (1-lambda_year_rate)*nh.vec[t-1] + n0*eta_year_rate*phi*(1+eta_year_rate-lambda_year_rate)^(t-1)
}
nh.vec

# low tipper pop
(nl.vec = as.numeric(rep(NA, length(t.vec))))
length(nl.vec)
(nl.vec[1] = (1-lambda_year_rate)*nl0 + n0*eta_year_rate*(1-phi))
for (t in 2:length(t.vec)) {
  nl.vec[t] = (1-lambda_year_rate)*nl.vec[t-1] + n0*eta_year_rate*(1-phi)*(1+eta_year_rate-lambda_year_rate)^(t-1)
}
nl.vec

# avg tipping rate over time
(r0 = 0.1)
r.vec = as.numeric(rep(NA, length(t.vec)))
rh.vec = as.numeric(rep(NA, length(t.vec)))
rl.vec = as.numeric(rep(NA, length(t.vec)))
#
(rh.vec[1] = (1+muh)*r0)# eq (1) in paper
(rl.vec[1] = (1-mul)*r0)
(r.vec[1] = r0)
#
for (t in 2:length(t.vec)) {
  rh.vec[t] = (1+muh)*r.vec[t-1];
  rl.vec[t] = (1-muh)*r.vec[t-1];
  r.vec[t] = (nh.vec[t]*rh.vec[t] + nl.vec[t]*rl.vec[t])/(n.vec[t])
}
data.frame(t.vec, r.vec, rh.vec, rl.vec)

simul.df$case2 = r.vec
simul.df

#plotting Figure 2
ggplot(simul.df, aes(x=t.vec)) + geom_line(aes(y=100*case1), linetype="solid", size=1.2, color="black") + geom_line(aes(y=100*case2), linetype="longdash", size=1.2, color="red") + scale_x_continuous(breaks = seq(0,100,5)) + scale_y_continuous(breaks = seq(0,20,1))  +labs(x="Time period", y="Tipping rates (%)")  +theme(axis.text.x = element_text(size = 14, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) +geom_segment(aes(x=33, y=5, xend=33, yend=17), linetype="dotted", size=1.2) +annotate("text", x = 93, y = 18.0, label =TeX("$\\hat{r}_t^I$"), size = 8, color="black") +annotate("text", x = 93, y = 7.5, label =TeX("$\\hat{r}_t^{II}$"), size = 8, color="red") 

#+annotate("text", x = 1990, y = 11.4, label =TeX("$\\hat{r}_t^L$"), size = 8, color="blue") +geom_segment(aes(x=2020, y=8, xend=2020, yend=20), linetype="dotted", size=1.2) +geom_segment(aes(x=1950, y=20, xend=2020, yend=20), linetype="dotted", size=1.2) +geom_segment(aes(x=2013, y=8, xend=2013, yend=18), linetype="dotted", size=1.2) +geom_segment(aes(x=1950, y=18, xend=2013, yend=18), linetype="dotted", size=1.2) +geom_segment(aes(x=2001, y=8, xend=2001, yend=15), linetype="dotted", size=1.2) +geom_segment(aes(x=1950, y=15, xend=2001, yend=15), linetype="dotted", size=1.2)

#Figure 2 (simulations) ends####
