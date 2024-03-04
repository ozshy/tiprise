# tiprise_2024_x_y.R 

# packages used
library(ggplot2); theme_set(theme_bw())
library(latex2exp)# LaTeX in ggplot
library(xtable)# export data frames to LaTeX tables

#Figure 1 (calibrations) begins####

(phi=0.553)# frac of high tippers
(muh = 0.2)# high-tippers' markup
(mul = 0.2)# high-tippers' markdown (negative)

# population growth https://www.census.gov/data/tables/time-series/dec/popchange-data-text.html (note: includes additions and subtractions, e.g., birth and dealth, immigration, etc.)
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

# Fig 2 case 1: falling then rising tips
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

# Fig 2 case 2: rising then falling tips
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

## Fig 2 case 3: random pop growth with nht upward drift
(t.vec = 1:100)
#
(n0=100)
(nh0=50)
(nl0=50)
#
(muh = 0.2)# high-tippers' markup
(mul = 0.2)# high-tippers' markdown (negative)
# Generate a random sequence with an upward drift of prob of high tippers
set.seed(1955)
(phi.vec = sample(c(0.2,0.6), size=100, prob = c(0.2,0.8), replace = T))
table(phi.vec)

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
(nh.vec[1] = (1-lambda_year_rate)*nh0 + n0*eta_year_rate*phi.vec[1])
for (t in 2:length(t.vec)) {
  nh.vec[t] = (1-lambda_year_rate)*nh.vec[t-1] + n0*eta_year_rate*phi.vec[t]*(1+eta_year_rate-lambda_year_rate)^(t-1)
}
nh.vec

# low tipper pop
(nl.vec = as.numeric(rep(NA, length(t.vec))))
length(nl.vec)
(nl.vec[1] = (1-lambda_year_rate)*nl0 + n0*eta_year_rate*(1-phi.vec[1]))
for (t in 2:length(t.vec)) {
  nl.vec[t] = (1-lambda_year_rate)*nl.vec[t-1] + n0*eta_year_rate*(1-phi.vec[t])*(1+eta_year_rate-lambda_year_rate)^(t-1)
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
data.frame(t.vec, r.vec, rh.vec, rl.vec, "frac nh" = nh.vec/n.vec, "frac hl" = nl.vec/n.vec)

simul.df$case3 = r.vec
simul.df

## plotting Figure 2
ggplot(simul.df, aes(x=t.vec)) + geom_line(aes(y=100*case1), linetype="dotdash", size=1.2, color="black") + geom_line(aes(y=100*case2), linetype="longdash", size=1.2, color="red") + geom_line(aes(y=100*case3), linetype="solid", size=1.2, color="blue") + scale_x_continuous(breaks = seq(0,100,5)) + scale_y_continuous(breaks = seq(0,20,1))  +labs(x="Time period", y="Tipping rates (%)")  +theme(axis.text.x = element_text(size = 14, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) +geom_segment(aes(x=33, y=5, xend=33, yend=17), linetype="dotted", size=1.2) +annotate("text", x = 93, y = 18.0, label =TeX("$\\hat{r}_t^I$"), size = 8, color="black") +annotate("text", x = 93, y = 7.5, label =TeX("$\\hat{r}_t^{II}$"), size = 8, color="red") +annotate("text", x = 93, y = 12, label =TeX("$\\hat{r}_t^{III}$"), size = 8, color="blue") 

#Figure 2 (simulations) ends####

#Figure 3 (utility) begins####
# equation (13) subsection 4.1 in the paper
(alph=0.5)
(bet=0.5)
(py=1)
(tau=0.18)
(dtau=0)#will turn into a vector for the simulation
# gam (gamma) will be a vector in the simulations
(gam = (py^(alph/(alph-1)) *(tau+dtau)^(1-bet) *((1+tau+dtau)/alph)^(1/(alph-1)))/bet)
#
# y is needed to compute the Hessian => will need also to be a vector in the simulations
(y=(bet*gam*(tau+dtau)^(bet-1))/py)# from FOC
# Hessian (will be a vector in the simulations, should be >0)
(h = alph*bet*gam*y^(alph-2) *(tau+dtau)^(bet-2) *(alph-1) *(bet-1) -py^2)

# making the above vectors of dtau
(dtau.vec = seq(-0.05, 0.05, 0.001))
length(dtau.vec)
tau+dtau.vec # range 

# Case of tau = 0.15 => 15% social norm tip
tau_15 = 0.15
#
(gam_15.vec = (py^(alph/(alph-1)) *(tau_15+dtau.vec)^(1-bet) *((1+tau_15+dtau.vec)/alph)^(1/(alph-1)))/bet)
#
(y_15.vec=(bet*gam_15.vec*(tau_15+dtau.vec)^(bet-1))/py)# from FOC
#
(h_15.vec = alph*bet*gam_15.vec*y_15.vec^(alph-2) *(tau_15+dtau.vec)^(bet-2) *(alph-1) *(bet-1) -py^2)

# Case of tau = 0.18 => 18% social norm tip
tau_18 = 0.18
#
(gam_18.vec = (py^(alph/(alph-1)) *(tau_18+dtau.vec)^(1-bet) *((1+tau_18+dtau.vec)/alph)^(1/(alph-1)))/bet)
#
(y_18.vec=(bet*gam_18.vec*(tau_18+dtau.vec)^(bet-1))/py)# from FOC
#
(h_18.vec = alph*bet*gam_18.vec*y_18.vec^(alph-2) *(tau_18+dtau.vec)^(bet-2) *(alph-1) *(bet-1) -py^2)

# Case of tau = 0.15 => 15% social norm tip
tau_15 = 0.15
#
(gam_15.vec = (py^(alph/(alph-1)) *(tau_15+dtau.vec)^(1-bet) *((1+tau_15+dtau.vec)/alph)^(1/(alph-1)))/bet)
#
(y_15.vec=(bet*gam_15.vec*(tau_15+dtau.vec)^(bet-1))/py)# from FOC
#
(h_15.vec = alph*bet*gam_15.vec*y_15.vec^(alph-2) *(tau_15+dtau.vec)^(bet-2) *(alph-1) *(bet-1) -py^2)

# Case of tau = 0.20 => 20% social norm tip
tau_20 = 0.20
#
(gam_20.vec = (py^(alph/(alph-1)) *(tau_20+dtau.vec)^(1-bet) *((1+tau_20+dtau.vec)/alph)^(1/(alph-1)))/bet)
#
(y_20.vec=(bet*gam_20.vec*(tau_20+dtau.vec)^(bet-1))/py)# from FOC
#
(h_20.vec = alph*bet*gam_20.vec*y_20.vec^(alph-2) *(tau_20+dtau.vec)^(bet-2) *(alph-1) *(bet-1) -py^2)

# preparing a data frame for drawing
(utility.df = data.frame(dtau.vec, gam_15.vec, gam_18.vec, gam_20.vec))
#
# plotting Figure 3 (with flipping axes)
ggplot(utility.df, aes(x=100*dtau.vec)) +geom_line(aes(y=gam_15.vec), linetype="solid", size=1.2, color="black") +geom_line(aes(y=gam_18.vec), linetype="longdash", size=1.2, color="red") +geom_line(aes(y=gam_20.vec), linetype="dotdash", size=1.2, color="blue") +labs(x=TeX("$\\Delta\\tau =$ deviation from the social norm tipping rate (%)"), y=TeX("$\\gamma =$ tipping utility parameter")) + scale_x_continuous(breaks = seq(-5,5,0.5)) + scale_y_continuous(breaks = seq(0.13,0.16,0.0025)) +coord_flip() +theme(axis.text.x = element_text(size = 14, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) +annotate("text", x = 2.7, y = 0.15, label =TeX("$\\hat{\\tau}=15\\%$"), size = 6, color="black") +annotate("text", x = -0.3, y = 0.15, label =TeX("$\\hat{\\tau}=18\\%$"), size = 6, color="red") +annotate("text", x = -4.3, y = 0.15, label =TeX("$\\hat{\\tau}=20\\%$"), size = 6, color="blue") +annotate("text", x = -5, y = 0.1573, label =TeX("High tippers (high $\\gamma$)"), size = 6, color="black") +annotate("text", x = -5, y = 0.1350, label =TeX("Low tippers (low $\\gamma$)"), size = 6, color="black") +geom_segment(aes(x=-2, y=0.1300, xend=-2, yend=0.14118), linetype="dotted", size=1.2) +geom_segment(aes(x=-2, y=0.14118, xend=-5, yend=0.14118), linetype="dotted", size=1.2) +geom_segment(aes(x=3, y=0.1300, xend=3, yend=0.15234), linetype="dotted", size=1.2) +geom_segment(aes(x=3, y=0.15234, xend=-5, yend=0.15234), linetype="dotted", size=1.2)
                                                            
#Figure 3 (utility) ends####

#Figure 4 (scope simulations) begins####
# simulation of expanding set of tipped services
(t.vec = 1:50)
(simul.df = data.frame(t.vec))# preparing data frame for 2 cases

# Fig 4 (scope simulations) case 1:
(n0=100)
(nh0=30)
(nl0=70)
#
(phi=0.65)# frac of high tippers

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
#
#verify sum:  # should be equal to n.vec
nh.vec+nl.vec
n.vec

# social norm num of tipped services
(s0 = 0)
(sig = 0.51)# sigma in paper: "majority" min frac of high tippers to trigger change in social norm (adding 1 more service)
#
(delta_h=1)# additional services tipped by type H
(delta_l=1)# fewer services tipped by type L
#
s.vec = as.numeric(rep(NA, length(t.vec)))
#
(s.vec[1] = ifelse(nh.vec[1]/n.vec[1] > sig, s0+delta_h, ifelse(nh.vec[1]/n.vec[1] < 1-sig, ifelse(s0-delta_l >=1, s0-delta_l, 0), s0)))
#
for (t in 2:length(t.vec)) {
(s.vec[t] = ifelse(nh.vec[t]/n.vec[t] > sig, s.vec[t-1]+delta_h, ifelse(nh.vec[t]/n.vec[t] < 1-sig, ifelse(s.vec[t-1]-delta_l >=1, s.vec[t-1]-delta_l, 0), s.vec[t-1])))
}
s_case1.vec = s.vec
data.frame(t.vec, s_case1.vec, nh.vec/n.vec)

# Fig 4 (scope simulations) case 2 (random):
(n0=100)
(nh0=50)
(nl0=50)

# Generate a random sequence with an upward drift of prob of high tippers
set.seed(1955)
(phi.vec = sample(c(0.2,0.6), size=100, prob = c(0.2,0.8), replace = T))
table(phi.vec)

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
(nh.vec[1] = (1-lambda_year_rate)*nh0 + n0*eta_year_rate*phi.vec[1])
for (t in 2:length(t.vec)) {
  nh.vec[t] = (1-lambda_year_rate)*nh.vec[t-1] + n0*eta_year_rate*phi.vec[t]*(1+eta_year_rate-lambda_year_rate)^(t-1)
}
nh.vec

# low tipper pop
(nl.vec = as.numeric(rep(NA, length(t.vec))))
length(nl.vec)
(nl.vec[1] = (1-lambda_year_rate)*nl0 + n0*eta_year_rate*(1-phi.vec[1]))
for (t in 2:length(t.vec)) {
  nl.vec[t] = (1-lambda_year_rate)*nl.vec[t-1] + n0*eta_year_rate*(1-phi.vec[t])*(1+eta_year_rate-lambda_year_rate)^(t-1)
}
nl.vec

# verify sum to n
nh.vec+nl.vec
n.vec

# social norm num of tipped services
(s0 = 0)
(sig = 0.51)# sigma in paper: "majority" min frac of high tippers to trigger change in social norm (adding 1 more service)
#
(delta_h=1)# additional services tipped by type H
(delta_l=1)# fewer services tipped by type L
#
s.vec = as.numeric(rep(NA, length(t.vec)))
#sh.vec = as.numeric(rep(NA, length(t.vec)))
#sl.vec = as.numeric(rep(NA, length(t.vec)))
#
#(sh.vec[1] = s0+)# eq (1) in paper
#(sl.vec[1] = s0)
(s.vec[1] = ifelse(nh.vec[1]/n.vec[1] > sig, s0+delta_h, ifelse(nh.vec[1]/n.vec[1] < 1-sig, ifelse(s0-delta_l >=1, s0-delta_l, 0), s0)))
#
for (t in 2:length(t.vec)) {
  (s.vec[t] = ifelse(nh.vec[t]/n.vec[t] > sig, s.vec[t-1]+delta_h, ifelse(nh.vec[t]/n.vec[t] < 1-sig, ifelse(s.vec[t-1]-delta_l >=1, s.vec[t-1]-delta_l, 0), s.vec[t-1])))
}
#
s_case2.vec = s.vec
(scope.df=data.frame(t.vec, s_case1.vec, s_case2.vec, nh.vec/n.vec))

## plotting Figure 4
ggplot(scope.df, aes(x=t.vec)) +geom_point(aes(y=s_case1.vec),  size=2, color="blue") +geom_line(aes(y=s_case1.vec),  size=0.4, color="blue") +geom_point(aes(y=s_case2.vec),  size=2, color="red") +geom_line(aes(y=s_case2.vec),  size=0.4, color="red") +scale_x_continuous(breaks = seq(0,50,5)) + scale_y_continuous(breaks = seq(0,15,1))  +labs(x="Time period (t)", y=TeX("Period $t$ social norm number of tipped services $(\\hat{s}_t)$"))  +theme(axis.text.x = element_text(size = 14, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20))  +annotate("text", x = 18, y = 3, label =TeX("$\\hat{s}_t^{II}$"), size = 8, color="red") +annotate("text", x = 45, y = 3, label =TeX("$\\hat{s}_t^{I}$"), size = 8, color="blue") 


# Below are vertical dotted lines to mark the kinks on the 2 graphs. Not used in the paper!
#+geom_segment(aes(x=18, y=0, xend=18, yend=1), linetype="dotted", size=1) +geom_segment(aes(x=23, y=0, xend=23, yend=6), linetype="dotted", size=1) +geom_segment(aes(x=43, y=0, xend=43, yend=7), linetype="dotted", size=1) 

#Figure 4 (scope simulations) ends####

#Figure 6 (utility scope simulations) begins####
# Note: I do it differently from Figure 3 (Delta_tau). Here, I solve for \Delta_s directly so there is no need to flip the axes. Recall that I was unable to solve (13) explicitly for Delta_tau and that's why I solved for gamma as a function of give a sequence of Delta_tau. In (22), Delta_s is solved directly. 

# equation (22) subsection 5.2 in the paper
(alph=0.5)
(bet=0.5)
(py=1)
(tau=0.15)
#
# yst and ysu from eq (B.4) in paper, not needed here
(yst=(py*(1+tau))^(1/(alph-1))) # from FOC
(ysu=py^(1/(alph-1)))# from FOC
yst < ysu # must hold because t are tipped services
#
(s=3)# initial number of tipped services
(gam=0.4)# gamma: will turn into a vector for the simulation
# gam (gamma) will be a vector in the simulations (cancelled, I will solve for Delta_s instead of gamma)
#(gam = ((1-alph)/alph) * (py^(alph/(alph-1)) -(py*(1+tau))^(alph/(alph-1))) *(s+ds)^(1-bet)  )
# eq (22) Delta s as constant, later will turn into a vector. Below is typed from the paper eq (22)
(ds_temp1 = gam^(1/(1-bet)) *(py)^(alph/((alph-1)*(bet-1))) *(1-(1+tau)^(alph/(alph-1)))^(1/(bet-1)) -s)
# typing from Derive eq #82, retyping the above for verification
# build (22) via its component terms
(term1 = (py)^(alph/((alph-1)*(bet-1))))
(term2 = (alph-1)*((1+tau)^(alph/(alph-1))-1)/(alph*gam) )
(ds_temp3 = term1 * term2^(1/(bet-1)) -s)
#
ds_temp3 - ds_temp1 #verify almost equal
# now set min -s (to avoid negative num of tipped services)
(ds_temp4 = max(ds_temp3, -s))
# now round ds into an integer
(ds = round(ds_temp4,digits =0))

# turn ds into a vector for Figure 6
(gam.vec = seq(0.2,0.4,0.001))
length(gam.vec)

# case of s=3 and tau = 15%
(s=3)
(tau=0.15)
(term1 = (py)^(alph/((alph-1)*(bet-1))))
(term2.vec = (alph-1)*((1+tau)^(alph/(alph-1))-1)/(alph*gam.vec) )
(ds_temp3.vec = term1 * term2.vec^(1/(bet-1)) -s)
# now set min to -s to avoid negative num of tipped services
(ds_temp4.vec = ds_temp3.vec) #initialize before loop
for (i in 1:length(gam.vec)) {
 ds_temp4.vec[i] = ifelse(ds_temp3.vec[i] < -s, -s, ds_temp3.vec[i])
}
ds_temp4.vec
# round ds into an integer
(ds_3_15.vec = round((ds_temp4.vec)))

# case of s=3 and tau = 18%
(s=3)
(tau=0.18)
(term1 = (py)^(alph/((alph-1)*(bet-1))))
(term2.vec = (alph-1)*((1+tau)^(alph/(alph-1))-1)/(alph*gam.vec) )
(ds_temp3.vec = term1 * term2.vec^(1/(bet-1)) -s)
# restrict ds not less than -s
(ds_temp4.vec = ds_temp3.vec) #initialize before loop
for (i in 1:length(gam.vec)) {
  ds_temp4.vec[i] = ifelse(ds_temp3.vec[i] < -s, -s, ds_temp3.vec[i])
}
ds_temp4.vec
# round turn it into an integer
(ds_3_18.vec = round((ds_temp4.vec)))

# case of s=3 and tau = 20%
(s=3)
(tau=0.2)
(term1 = (py)^(alph/((alph-1)*(bet-1))))
(term2.vec = (alph-1)*((1+tau)^(alph/(alph-1))-1)/(alph*gam.vec) )
(ds_temp3.vec = term1 * term2.vec^(1/(bet-1)) -s)
# restrict ds not less than -s
(ds_temp4.vec = ds_temp3.vec) #initialize before loop
for (i in 1:length(gam.vec)) {
  ds_temp4.vec[i] = ifelse(ds_temp3.vec[i] < -s, -s, ds_temp3.vec[i])
}
ds_temp4.vec
# round turn it into an integer
(ds_3_20.vec = round((ds_temp4.vec)))

# case of s=4 and tau = 15%
(s=4)
(tau=0.15)
(term1 = (py)^(alph/((alph-1)*(bet-1))))
(term2.vec = (alph-1)*((1+tau)^(alph/(alph-1))-1)/(alph*gam.vec) )
(ds_temp3.vec = term1 * term2.vec^(1/(bet-1)) -s)
# ds cannot be lower than -s
(ds_temp4.vec = ds_temp3.vec) #initialize before loop
for (i in 1:length(gam.vec)) {
  ds_temp4.vec[i] = ifelse(ds_temp3.vec[i] < -s, -s, ds_temp3.vec[i])
}
ds_temp4.vec
# round it into an integer
(ds_4_15.vec = round((ds_temp4.vec)))

# case of s=4 and tau = 18%
(s=4)
(tau=0.18)
(term1 = (py)^(alph/((alph-1)*(bet-1))))
(term2.vec = (alph-1)*((1+tau)^(alph/(alph-1))-1)/(alph*gam.vec) )
(ds_temp3.vec = term1 * term2.vec^(1/(bet-1)) -s)
# ds cannot be lower than -s
(ds_temp4.vec = ds_temp3.vec) #initialize before loop
for (i in 1:length(gam.vec)) {
  ds_temp4.vec[i] = ifelse(ds_temp3.vec[i] < -s, -s, ds_temp3.vec[i])
}
ds_temp4.vec
# round it into an integer
(ds_4_18.vec = round((ds_temp4.vec)))

# case of s=4 and tau = 20%
(s=4)
(tau=0.2)
(term1 = (py)^(alph/((alph-1)*(bet-1))))
(term2.vec = (alph-1)*((1+tau)^(alph/(alph-1))-1)/(alph*gam.vec) )
(ds_temp3.vec = term1 * term2.vec^(1/(bet-1)) -s)
# ds cannot be lower than -s
(ds_temp4.vec = ds_temp3.vec) #initialize before loop
for (i in 1:length(gam.vec)) {
  ds_temp4.vec[i] = ifelse(ds_temp3.vec[i] < -s, -s, ds_temp3.vec[i])
}
ds_temp4.vec
# round it into an integer
(ds_4_20.vec = round((ds_temp4.vec)))


# Inserting results into a data frame (not all variables will in Figure 6)
(ds.df = data.frame(gam.vec, ds_3_15.vec, ds_4_15.vec, ds_3_18.vec, ds_4_18.vec, ds_3_20.vec, ds_4_20.vec))

#
# plotting Figure 6
ggplot(ds.df, aes(x=gam.vec)) +geom_line(aes(y=ds_3_15.vec), linetype="solid", size=1.2, color="black") +geom_line(aes(y=ds_4_15.vec), linetype="longdash", size=1.2, color="red") +geom_line(aes(y=ds_3_20.vec), linetype="dotdash", size=1.2, color="blue") +labs(y=TeX("$\\Delta s =$ deviation from the social norm"), x=TeX("$\\gamma =$ tipping utility parameter")) + scale_x_continuous(breaks = seq(0.2,0.4,0.01)) + scale_y_continuous(breaks = seq(-2,6,1)) +theme(axis.text.x = element_text(size = 14, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) +annotate("text", x = 0.311, y = 3.3, label =TeX("I. ($\\tau=15\\%$, $\\hat{s}=3)"), size = 6, color="black") +annotate("text", x = 0.356, y = 2.7, label =TeX("II. ($\\tau=15\\%$, $\\hat{s}=4)"), size = 6, color="red") +annotate("text", x = 0.378, y = 1.7, label =TeX("III. ($\\tau=20\\%$, $\\hat{s}=3)"), size = 6, color="blue") +geom_segment(aes(x=0.39, y=5.9, xend=0.39, yend=5.1), arrow = arrow(length=unit(0.30,"cm"), ends="last", type = "closed"), color="red") +geom_segment(aes(x=0.395, y=5.9, xend=0.395, yend=3.1), arrow = arrow(length=unit(0.30,"cm"), ends="last", type = "closed"), color="blue") +annotate("text", x = 0.38, y = -2, label =TeX("High tippers (high $\\gamma$)"), size = 6, color="black") +annotate("text", x = 0.23, y = -2, label =TeX("Low tippers (low $\\gamma$)"), size = 6, color="black")  

#Figure 6 (utility scope simulations) ends####