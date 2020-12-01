source("stats.R")

# in case you don't have the Tikz package installed, run:
# install.packages('tikzDevice')

library(tikzDevice) # load the tikzDevice package

write.table(correlation.hypothesis, "tabelas/correlacoes.csv", sep=";")

# UK Scatter Plot
plot(g.uk$growth[ante_consensus] ~ g.usa$growth[ante_consensus],
     xlab = "Crescimento dos EUA (1961-1988) (%)",
     ylab = "Crescimento do Reino Unido (1961-1988) (%)")
abline(lm(g.uk$growth[ante_consensus] ~ g.usa$growth[ante_consensus]))
plot(g.uk$growth[post_consensus] ~ g.usa$growth[post_consensus],
     xlab = "Crescimento dos EUA (1989-2019) (%)",
     ylab = "Crescimento do Reino Unido (1989-2019) (%)")
abline(lm(g.uk$growth[post_consensus] ~ g.usa$growth[post_consensus]))

# p.value = 0.4 Scatter Plots
plot(g.hong_kong$growth[ante_consensus] ~ g.usa$growth[ante_consensus],
     xlab = "Crescimento dos EUA (1962-1988) (%)",
     ylab = "Crescimento de Hong Kong (1962-1988) (%)")
abline(lm(g.hong_kong$growth[ante_consensus] ~ g.usa$growth[ante_consensus]))

plot(g.singapore$growth[post_consensus] ~ g.usa$growth[post_consensus],
     xlab = "Crescimento dos EUA (1989-2019) (%)",
     ylab = "Crescimento de Singapura (1989-2019) (%)")
abline(lm(g.singapore$growth[post_consensus] ~ g.usa$growth[post_consensus]))

plot(g.chile$growth[post_consensus] ~ g.usa$growth[post_consensus],
     xlab = "Crescimento dos EUA (1989-2019) (%)",
     ylab = "Crescimento do Chile (1989-2019) (%)")
abline(lm(g.chile$growth[post_consensus] ~ g.usa$growth[post_consensus]))

# Vietnam Scatter Plot
plot(g.vietnam$growth[ante_consensus] ~ g.usa$growth[ante_consensus],
     xlab = "Crescimento dos EUA (1985-1988) (%)",
     ylab = "Crescimento do Vietnã (1985-1988) (%)")
abline(lm(g.vietnam$growth[ante_consensus] ~ g.usa$growth[ante_consensus]))

plot(g.vietnam$growth[post_consensus] ~ g.usa$growth[post_consensus],
     xlab = "Crescimento dos EUA (1988-2019) (%)",
     ylab = "Crescimento do Vietnã (1988-2019) (%)")
abline(lm(g.vietnam$growth[post_consensus] ~ g.usa$growth[post_consensus]))

