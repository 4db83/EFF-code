# %% CLEAR SCREEN AND WORKSPACE
rm(list = ls()); cat("\014"); gc();
# SET DEFAULTS: display options
options(digits = 8); options(scipen = 999);  options(max.print=1e4);
# INSTALL PACMAN: if not installed (note: may need to disable windows firewall for packages to install)
if(!"pacman" %in% installed.packages()){install.packages("pacman")}
# LOAD/INSTALL: other required packages (hexView reads EViews workfiles)
pacman::p_load(hexView)

# The data sets of Hurn, Martin, Phillips and Yu (2021) [HMPY2021] are only available locally inside the
# EViews answer workfiles of the book resources. This script extracts the series used in Labs 10 and 11
# and writes them to ./data/ as csv files. It only needs to be run once.
path2hurn = "../books/Hurn et al book resources/back-up/"
path2data = "./data/"

# readEViews() fails only when it builds the date index of daily or irregular workfiles,
# so mask seq() inside a copy of it; the series themselves are read correctly
readEV = readEViews
env = new.env(parent = asNamespace("hexView"))
env$seq = function(...) tryCatch(base::seq(...), error = function(e) seq_len(list(...)$length.out))
environment(readEV) = env

# EViews stores dates as days since 1 January 0001, and missing values are read as 1e-37
ev_date = function(x) as.Date(x, origin = "0001-01-01")
ev_na   = function(x) { x[abs(x) < 1e-30 & x != 0] = NA; x }

# unzip the EViews answer workfiles of Chapters 8 and 10 to a temporary folder
tmp = file.path(tempdir(), "hurn")
unzip(paste0(path2hurn, "hurn1e_answers_eviews_ch08.zip"), exdir = tmp)
unzip(paste0(path2hurn, "hurn1e_answers_eviews_ch10.zip"), exdir = tmp)
rd = function(f) suppressWarnings(readEV(file.path(tmp, f), as.data.frame = FALSE))

# %% vix: S&P 500 daily log returns and the VIX scaled to a daily variance, 2 Jan 1990 - 1 Jun 2012 [HMPY2021, Section 8.2]
w = rd("Ch8Ex2_answer.WF1")
vix = data.frame(date = ev_date(w$DATE), ret = ev_na(w$RETURNS), h = ev_na(w$IMPLIED_VIX))
write.csv(vix, paste0(path2data, "hurn_vix.csv"), row.names = FALSE)

# %% ccapm: quarterly 1947-1987, Ferson and Harvey (1992) [HMPY2021, Section 8.6]
#    dc  = growth of real nondurables consumption, tb = real return on Treasury bills,
#    gb, cb = real returns on long-term government and corporate bonds, d1, d10 = smallest and largest NYSE size deciles
w = rd("Ch8Ex4_answer.WF1")
ccapm = data.frame(date = ev_date(w$DATES), dc = w$CINST, tb = w$RINST, gb = w$GB, cb = w$CB, d1 = w$D1, d10 = w$D10)
write.csv(ccapm, paste0(path2data, "hurn_ccapm.csv"), row.names = FALSE)

# %% stockindices: daily closing prices, 4 Jan 1999 - 2 Apr 2014 [HMPY2021, Exercise 10.2]
w = rd("Ch10Ex2_answer.wf1")
idx = data.frame(date = ev_date(w$DATE), spx = w$SPX, djx = w$DJX, hkx = w$HKX, nkx = w$NKX, dax = w$DAX, ukx = w$UKX)
write.csv(idx, paste0(path2data, "hurn_stockindices.csv"), row.names = FALSE)

# %% eurodollar: daily Eurodollar interest rates in percent [HMPY2021, Exercise 10.3]
w = rd("Ch10Ex3_answer.wf1")
euro = data.frame(date = ev_date(w$DATEVEC), rate = ev_na(w$R))
write.csv(euro, paste0(path2data, "hurn_eurodollar.csv"), row.names = FALSE)

# %% amr: American Airlines trades, 1-second intervals, 9:30 to 16:00 on 1 August 2006 [HMPY2021, Section 10.3.1]
#    seconds = seconds after midnight, trade = 1 if a trade occurs, duration = seconds since the previous trade (recorded at trades)
w = rd("Ch10Ex4_answer.wf1")
amr = data.frame(seconds = w$TIME, trade = w$Y, duration = w$U)
write.csv(amr, paste0(path2data, "hurn_amr.csv"), row.names = FALSE)

# %% crisis: number of countries (of 21) with a banking or currency crisis, 1800-2010, Reinhart and Rogoff (2009) [HMPY2021, Exercise 10.7]
w = rd("Ch10Ex7_answer.wf1")
crisis = data.frame(year = 1800:2010, banking = w$BANKING, currency = w$CURRENCY)
write.csv(crisis, paste0(path2data, "hurn_crisis.csv"), row.names = FALSE)

# %% SUMMARY OF WHAT WAS WRITTEN
for (f in c("vix", "ccapm", "idx", "euro", "amr", "crisis")) {
  d = get(f)
  cat(sprintf(" %-7s %6d rows: %s\n", f, nrow(d), paste(names(d), collapse = ", ")))
}
cat(sprintf(" vix dates %s to %s, eurodollar dates %s to %s (%d missing)\n", min(vix$date), max(vix$date),
            min(euro$date), max(euro$date), sum(is.na(euro$rate))))
