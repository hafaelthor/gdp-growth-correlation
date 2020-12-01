# in case you don't have the World Bank API installed, run:
# install.packages('WDI')

# load the World Bank API, WDI
library(WDI)

INDCODE = 'NY.GDP.MKTP.KD.ZG' # GDP growth (annual %)

g.usa = WDI(indicator = c(growth = INDCODE), country = "USA")

consensus.year = 1989 # Year of the Washington Consensus presentation

# alpha value of significance
significance = 0.05

# Select rows from years after the Washington Consensus
ante_consensus = which(g.usa$year < consensus.year)
post_consensus = which(g.usa$year >= consensus.year)

# Western Europe (Nominal GDP)
g.germany     = WDI(indicator = c(growth = INDCODE), country = "DE")
g.uk          = WDI(indicator = c(growth = INDCODE), country = "GB")
g.france      = WDI(indicator = c(growth = INDCODE), country = "FR")
g.italy       = WDI(indicator = c(growth = INDCODE), country = "IT")
g.spain       = WDI(indicator = c(growth = INDCODE), country = "ES")
g.netherlands = WDI(indicator = c(growth = INDCODE), country = "NL")
western_europe.group = list(DE=g.germany, GB=g.uk,
  FR=g.france, IT=g.italy, ES=g.spain, NL=g.netherlands)

# Asian Tigers and Japan (Nominal GDP)
g.japan       = WDI(indicator = c(growth = INDCODE), country = "JP")
g.south_korea = WDI(indicator = c(growth = INDCODE), country = "KR")
g.hong_kong   = WDI(indicator = c(growth = INDCODE), country = "HK")
g.singapore   = WDI(indicator = c(growth = INDCODE), country = "SG")
asian_belt.group = list(JP=g.japan, KR=g.south_korea,
  HK=g.hong_kong, SG=g.singapore)

# Latin America (Nominal GDP)
g.brazil    = WDI(indicator = c(growth = INDCODE), country = "BR")
g.mexico    = WDI(indicator = c(growth = INDCODE), country = "MX")
g.argentina = WDI(indicator = c(growth = INDCODE), country = "AR")
g.colombia  = WDI(indicator = c(growth = INDCODE), country = "CO")
g.chile     = WDI(indicator = c(growth = INDCODE), country = "CL")
g.peru      = WDI(indicator = c(growth = INDCODE), country = "PE")
g.venezuela = WDI(indicator = c(growth = INDCODE), country = "VE")
latin_america.group = list(BR=g.brazil, MX=g.mexico,
  AR=g.argentina, CO=g.colombia, CL=g.chile, 
  PE=g.peru, VE=g.venezuela)

# Arab World (Nominal GDP)
g.saudi_arabia  = WDI(indicator = c(growth = INDCODE), country = "SA")
g.un_arab_emir  = WDI(indicator = c(growth = INDCODE), country = "AE")
g.egypt         = WDI(indicator = c(growth = INDCODE), country = "EG")
g.iraq          = WDI(indicator = c(growth = INDCODE), country = "IQ")
g.qatar         = WDI(indicator = c(growth = INDCODE), country = "QA")
g.algeria       = WDI(indicator = c(growth = INDCODE), country = "DZ")
arab.group = list(SA=g.saudi_arabia, AE=g.un_arab_emir,
  EG=g.egypt, IQ=g.iraq, QA=g.qatar, DZ=g.algeria)

# Sub-saharan Africa (Nominal GDP)
g.nigeria       = WDI(indicator = c(growth = INDCODE), country = "NG")
g.south_africa  = WDI(indicator = c(growth = INDCODE), country = "ZA")
g.kenya         = WDI(indicator = c(growth = INDCODE), country = "KE")
g.ethiopia      = WDI(indicator = c(growth = INDCODE), country = "ET")
g.angola        = WDI(indicator = c(growth = INDCODE), country = "AO")
subsaharan.group = list(NG=g.nigeria, ZA=g.south_africa,
  KE=g.kenya, ET=g.ethiopia, AO=g.angola)

# Socialist Block (Nominal GDP)
g.vietnam   = WDI(indicator = c(growth = INDCODE), country = "VN")
g.cuba      = WDI(indicator = c(growth = INDCODE), country = "CU")
g.lao       = WDI(indicator = c(growth = INDCODE), country = "LA")
socialist.group = list(CU=g.cuba, VN=g.vietnam, LA=g.lao)

# Concatenating groups into one named list
groups=c("Western Europe" = western_europe.group,
         "Asian Tigers and Japan" = asian_belt.group,
         "Latin America" = latin_america.group,
         "Arab World" = arab.group,
         "Subsaharan Africa" = subsaharan.group,
         "Socialist Block" = socialist.group)

# Getting the countries and groups names into a list
grouped_countries.names = strsplit(names(groups), ".", fixed = TRUE)
country.names   = sapply(grouped_countries.names, function(x) x[[2]])
groups.row      = sapply(grouped_countries.names, function(x) x[[1]])

########################
# CORRELATION ANALYSIS #
########################

# The linear correlation (r) is estimated by
# r = cov(X, Y) / (sd(X) * sd(Y))
# an is computed by the command cor(X, Y)

# linear correlation of the "ante" Washington Consensus sample
us_correlation.ante_consensus <- function (g) 
  cor(g.usa$growth[ante_consensus], g$growth[ante_consensus], use = "na.or.complete")

# linear correlation of post Washington Consensus sample
us_correlation.post_consensus <- function (g) 
  cor(g.usa$growth[post_consensus], g$growth[post_consensus], use = "na.or.complete")

# CORRELATION TEST
#
# To conclude which countries have their
# growth correlated to the US' growth we
# do a correlation test (in R, cor.test)
#
# t = r * sqrt((n - 2)/(1 - r ^ 2)) ~ T(n - 2) 
# where t > 0, do |t| otherwise

# correlation test of the "ante" Washington Consensus sample
us.cor_test.ante_consensus <- function (g) {
  if (sum(complete.cases(g$growth[ante_consensus])) == 0) return(NA)
  cor.test(g.usa$growth[ante_consensus], g$growth[ante_consensus])$p.value
}

# correlation test of the post Washington Consensus sample
us.cor_test.post_consensus <- function (g) {
  if (sum(complete.cases(g$growth[post_consensus])) == 0) return(NA)
  cor.test(g.usa$growth[post_consensus], g$growth[post_consensus])$p.value
}

# Implementing Data Frame
correlations = data.frame(
  row.names = country.names,
  group     = groups.row,
  
  # linear correlations (r) of both samples
  r1 = sapply(groups, us_correlation.ante_consensus),
  r2 = sapply(groups, us_correlation.post_consensus),
  
  # p-values of both samples
  p1 = round(sapply(groups, us.cor_test.ante_consensus), 2),
  p2 = round(sapply(groups, us.cor_test.post_consensus), 2))

correlation.hypothesis = transform(correlations,
  h0.1 = correlations$p1 > significance,
  h0.2 = correlations$p2 > significance)

group.names = unique(groups.row)

# CHI-SQUARED TEST
#
# To conclude that there's a difference
# in growth correlation with the US based
# on globe region (e.g Western Europe)
# or in the different timespans ("ante" and
# post Washington Consensus), we do a
# Chi-Squared Test of independence

# Table for Chi-Squared Tests
correlation.hypothesis.table = data.frame(
  row.names = group.names,
  
  # Sum correlation.hypothesis outcomes by groups from both samples
  h0.1 = sapply(group.names, function (group) sum(correlation.hypothesis$h0.1[which(correlation.hypothesis$group == group)], na.rm = TRUE)),
  h1.1 = sapply(group.names, function (group) sum(!correlation.hypothesis$h0.1[which(correlation.hypothesis$group == group)], na.rm = TRUE)),
  h0.2 = sapply(group.names, function (group) sum(correlation.hypothesis$h0.2[which(correlation.hypothesis$group == group)], na.rm = TRUE)),
  h1.2 = sapply(group.names, function (group) sum(!correlation.hypothesis$h0.2[which(correlation.hypothesis$group == group)], na.rm = TRUE)))

tests.size = sum(correlation.hypothesis.table)

# Chi-Squared Test Matrix by Country Group
ocurrences.by_group = data.frame(
  row.names = group.names,
  h0 = correlation.hypothesis.table$h0.1 + correlation.hypothesis.table$h0.2,
  h1 = correlation.hypothesis.table$h1.1 + correlation.hypothesis.table$h1.2)

# Chi-Squared Test Matrix by Sample
aux = colSums(correlation.hypothesis.table)
ocurrences.by_sample = data.frame(
  row.names = c("before Washington", "after Washington"),
  h0 = c(aux[["h0.1"]], aux[["h0.2"]]),
  h1 = c(aux[["h1.1"]], aux[["h1.2"]])
)

independence.test.by_group = chisq.test(ocurrences.by_group)
independence.test.by_sample = chisq.test(ocurrences.by_sample, correct = FALSE) # without Yates' Correction
# the Yates' Correction isn't done because the number of ocurrences is
# way above 20, which tends to give overly conservative tests.

#############################
# REGRESSION SLOPE ANALYSIS #
#############################

# Test for Slope difference based on NCSS's
# "Tests for the Difference Between Two Linear Regression Slopes"
slope.test <- function (x1, y1, x2, y2) {
  lm1 <- lm(y1 ~ x1)
  lm2 <- lm(y2 ~ x2)
  
  n1 = sum(complete.cases(x1, y1))
  n2 = sum(complete.cases(x2, y2))
  
  v = n1 + n2 - 4
  m = n1 / n2
  
  var.x1  = sum((x1 - mean(x1, na.rm = TRUE)) ^ 2, na.rm = TRUE) / n1
  var.x2  = sum((x2 - mean(x2, na.rm = TRUE)) ^ 2, na.rm = TRUE) / n2
  sy2     = (sum(lm1$residuals ^ 2) + sum(lm2$residuals ^ 2)) / v
  sr2     = sy2 * (1/(m*var.x1) + 1/var.x2)
  
  # t ~ T-Student(v); t > 0
  t <- abs((coef(lm1)[[2]] - coef(lm2)[[2]]) * sqrt(n2 / sr2))
  
  # p-value
  2 * (1 - pt(t, v))
}

# Only select samples of countries with correlation presented in at least one time span
significant.countries = which(!correlation.hypothesis$h0.1 | !correlation.hypothesis$h0.2)

slopes = data.frame(
  row.names = country.names[significant.countries],
  group     = groups.row[significant.countries],
  
  # slopes of simple linear models in both samples
  b1 = sapply(groups[significant.countries], function (g) 
    coef(lm(g$growth[ante_consensus] ~ g.usa$growth[ante_consensus]))[[2]]),
  b2 = sapply(groups[significant.countries], function (g) 
    coef(lm(g$growth[post_consensus] ~ g.usa$growth[post_consensus]))[[2]]),
  
  # p-value of Slope Test
  p = sapply(groups[significant.countries], function (g)
    slope.test(g.usa$growth[ante_consensus], g$growth[ante_consensus], 
               g.usa$growth[post_consensus], g$growth[post_consensus])))

slope.hypothesis = transform(slopes,
  h0 = slopes$p > significance,
  change = slopes$b2 - slopes$b1)