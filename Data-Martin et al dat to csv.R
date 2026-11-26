# %% CLEAR SCREEN AND WORKSPACE
rm(list = ls()); cat("\014"); gc();
# SET DEFAULTS: display options
options(digits = 8); options(scipen = 999);  options(max.print=1e4);

# The data sets of Martin, Hurn and Harris (2013) [MHH2013] come with the R code of the book as .dat and .RData
# files. This script writes the series used in the MoM lab to ./data/ as csv files with column names.
# It only needs to be run once.
path2mhh  = "../books/Martin et al. (2013) 'Econometric Modelling with Time Series'/R/R/"
path2data = "./data/"
if (!dir.exists(path2data)) dir.create(path2data)

# %% ccapm: monthly, adjusted Hansen and Singleton (1982) data, 238 observations [MHH2013, Exercise 10.4]
#    cratio = c(t+1)/c(t), the gross growth of real per capita consumption
#    R      = real gross return at t+1 (gmm_ccapm.R: "real gross return"; the exercise calls it the real Treasury bill rate)
#    e      = real value weighted return
#    (column 4 of ccapm.dat is the lag of R and column 5 a 0/1 dummy; neither is used in the book's code)
x = read.table(paste0(path2mhh, "ccapm.dat"))
ccapm = data.frame(obs = 1:nrow(x), cratio = x[,1], R = x[,2], e = x[,3])
write.csv(ccapm, paste0(path2data, "mhh_ccapm.csv"), row.names = FALSE)

# %% level: monthly US zero coupon yields in percent p.a., December 1946 - February 1991 [MHH2013, Section 10.5.2, Exercise 10.9]
x = read.table(paste0(path2mhh, "level.dat"))
level = data.frame(date = format(seq(as.Date("1946-12-01"), by = "month", length.out = nrow(x)), "%Y-%m"),
                   r0m = x[,1], r1m = x[,2], r3m = x[,3], r6m = x[,4], r9m = x[,5], r10y = x[,6])
write.csv(level, paste0(path2data, "mhh_level.csv"), row.names = FALSE)

# %% equity_mp: annual Mehra and Prescott (1985) data, 1889 - 1979 [MHH2013, Exercise 10.10]
#    r = nominal risk free rate (% p.a.), p = price of consumption goods, c = real per capita consumption,
#    d = real dividend, S.nom and D.nom = nominal stock price and dividend (= s*p and d*p), s = real stock price
#    (the nominal rate of the last year, 1979, is not used: it is recorded as 0)
x = read.table(paste0(path2mhh, "equity_mp.dat"))
equity_mp = data.frame(year = x[,1], r = x[,2], p = x[,3], c = x[,4], d = x[,5], S.nom = x[,6], D.nom = x[,7], s = x[,8])
write.csv(equity_mp, paste0(path2data, "mhh_equity_mp.csv"), row.names = FALSE)

# %% equity_decomposition: daily index levels, 29 July 2004 - 3 March 2009 [MHH2013, Exercise 10.5]
x = read.table(paste0(path2mhh, "equity_decomposition.dat"))
eqdec = data.frame(obs = 1:nrow(x), sp500 = x[,1], ftse100 = x[,2], euro50 = x[,3])
write.csv(eqdec, paste0(path2data, "mhh_equity_decomposition.csv"), row.names = FALSE)

# %% hedge: daily returns in percent, 1 April 2003 - 28 May 2010 [MHH2013, Section 7.9.1, Exercise 7.8]
#    seven hedge fund indices (returns, not excess returns), the market excess return and the daily risk free rate
load(paste0(path2mhh, "hedge.RData"))
hedge = data.frame(obs = 1:nrow(hedgedata),
                   conv.arb = hedgedata[,1], distressed = hedgedata[,2], equity.hedge = hedgedata[,3],
                   event.driven = hedgedata[,4], macro = hedgedata[,5], merger.arb = hedgedata[,6],
                   eq.mkt.neutral = hedgedata[,7], mkt.excess = hedgedata[,8], rf = hedgedata[,9])
write.csv(hedge, paste0(path2data, "mhh_hedge.csv"), row.names = FALSE)

#EOF
