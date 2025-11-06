# make_gw_data_from_goyal.R
# Purpose: Download GW predictor Excel from Amit Goyal site and construct GW predictors
# Author: (generated)
# Requires: readxl, httr (or curl), dplyr, lubridate, zoo, xts, quantmod, tidyverse
#
# Usage: Rscript make_gw_data_from_goyal.R
#
# Note: This mirrors the MATLAB blueprint: computes log and simple equity premia,
#       dp, dy, ep, de, scaled yields (x100), adds FRED macro series and monthly SP500 volume.

# ---- Packages ----
required <- c("readxl","httr","curl","dplyr","lubridate","zoo","xts","quantmod","tidyr","lubridate")
invisible(lapply(required, function(pk){
  if(!requireNamespace(pk, quietly=TRUE)) install.packages(pk, repos = "https://cloud.r-project.org")
  library(pk, character.only = TRUE)
}))

library(readxl); library(curl); library(dplyr); library(lubridate)
library(zoo); library(xts); library(quantmod); library(tidyr)

# ---- Settings ----
vintage <- "2020"                 # change if you want e.g. "2021" or "2022" and update drive_id accordingly
data_dir <- "./GWdata/"
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

# Google Drive file id used in the matlab blueprint (PredictorData2020.xlsx)
drive_file_id_2020 <- "1ACbhdnIy0VbCWgsnXkjcddiV8HF4feWv"
# alternative ids (if you want a different vintage you can swap id)
drive_file_id_2022 <- "1g4LOaRj4TvwJr9RIaA_nwrXXWTOy46bP"

# choose id (for vintage 2020 use the 2020 id)
drive_file_id <- drive_file_id_2020

gw_source_file <- file.path(data_dir, paste0("PredictorData", vintage, ".xlsx"))
drive_download_url <- paste0("https://drive.google.com/uc?export=download&id=", drive_file_id)

# ---- Download file if not present ----
if(!file.exists(gw_source_file)){
  message("Downloading PredictorData Excel from Goyal Google Drive...")
  # curl::curl_download will follow redirects
  curl::curl_download(url = drive_download_url, destfile = gw_source_file, mode = "wb")
  message("Downloaded to: ", gw_source_file)
} else {
  message("File already exists locally: ", gw_source_file)
}

# ---- Read monthly sheet ----
# The MATLAB code used xlsread and expects first column yyyymm, then many variables
sheet_name <- "Monthly"
message("Reading sheet '", sheet_name, "' ...")
raw_tbl <- readxl::read_excel(gw_source_file, sheet = sheet_name, col_names = FALSE)

# Find header row and data row assumption:
# In many Goyal files first row contains headers; MATLAB used txt(1,2:end) so we assume header in row1.
# So re-read with header = TRUE but careful about date column name
raw_hdr <- readxl::read_excel(gw_source_file, sheet = sheet_name, n_max = 1, col_names = TRUE)
# read full with names
dat <- readxl::read_excel(gw_source_file, sheet = sheet_name, col_names = TRUE)

# Try to detect first column name (date index as yyyymm)
date_colname <- names(dat)[1]
message("Detected date column: ", date_colname)

# Convert date column expected to be in yyyymm (numeric or text)
# Handle a few formats:
dates_raw <- dat[[1]]
# if numeric like 195001 convert yyyymm -> Date at end of month (we will use last day)
convert_yyyymm_to_date <- function(x){
  x_chr <- as.character(x)
  # pad left if needed
  x_chr <- ifelse(nchar(x_chr)==5, paste0("0", x_chr), x_chr)
  year <- substr(x_chr,1,4)
  month <- substr(x_chr,5,6)
  # set to last day of month
  as.Date(paste0(year,"-",month,"-01")) + months(1) - days(1)
}
if(all(grepl("^[0-9]{6}$", as.character(dates_raw)) | is.numeric(dates_raw))){
  dates <- convert_yyyymm_to_date(dates_raw)
} else if(all(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", as.character(dates_raw)))){
  dates <- as.Date(dates_raw)
} else {
  # fallback try lubridate parsing
  dates <- lubridate::parse_date_time(as.character(dates_raw), orders = c("Ym","Y-m-d","b Y","my"))
  dates <- as.Date(dates)
  # if parsed as first-of-month, convert to end-of-month
  dates <- as.Date(as.yearmon(dates)) + months(1) - days(1)
}

# Create data.frame with date and remaining columns
df <- dat %>% mutate(Date = dates) %>% select(-1) %>% relocate(Date)
# Ensure variable names: MATLAB adjusted first few names: var_names(1) = {'sp500'}; var_names(4) = {'bm'};
# But the excel from Goyal normally has headers; we'll keep the names as read.
names(df)[-1] <- make.names(names(df)[-1])

# For convenience, convert to xts (monthly) keyed by Date
# Many Goyal columns expected: D12, E12, sp500, CRSP_SPvw, Rfree, svar, bm, ntis, lty, ltr, tbl, corpr, AAA, BAA, etc.
# We'll try to be robust by referencing column names by partial matches.

# helper to find column by partial name
get_col <- function(possible_names){
  nm <- names(df)
  for(p in possible_names){
    idx <- grep(p, nm, ignore.case = TRUE)
    if(length(idx)>0) return(nm[idx[1]])
  }
  return(NA)
}

col_sp500    <- get_col(c("^sp500$","sp500","^SP500","Index"))
col_D12      <- get_col(c("^D12$","D12","Dividends"))
col_E12      <- get_col(c("^E12$","E12","Earnings"))
col_CRSP_SPvw<- get_col(c("CRSP_SPvw","CRSP","SPvw","SPvwx","CRSP_SPvwx","CRSP.SPvw"))
col_CRSP_SPvwx<- get_col(c("CRSP_SPvwx","SPvwx"))
col_Rfree    <- get_col(c("Rfree","rf","Rfree"))
col_svar     <- get_col(c("svar","SVAR"))
col_bm       <- get_col(c("^bm$","book.to.market","bm"))
col_ntis     <- get_col(c("ntis"))
col_tbl      <- get_col(c("^tbl$","TBILL","tbl"))
col_lty      <- get_col(c("^lty$","lty","long.term.yield","LTY"))
col_ltr      <- get_col(c("^ltr$","ltr","long.term.return"))
col_corpr    <- get_col(c("corpr","CORPR"))
col_AAA      <- get_col(c("AAA"))
col_BAA      <- get_col(c("BAA"))
col_infl     <- get_col(c("infl","CPI","cpiauscsl"))

# Inform user of columns found (some may be NA)
message("Found columns (may be NA if not present):")
message("sp500: ", col_sp500)
message("D12:   ", col_D12)
message("E12:   ", col_E12)
message("CRSP_SPvw: ", col_CRSP_SPvw)
message("CRSP_SPvwx: ", col_CRSP_SPvwx)
message("Rfree: ", col_Rfree)
message("svar:  ", col_svar)
message("bm:    ", col_bm)
message("ntis:  ", col_ntis)
message("tbl:   ", col_tbl)
message("lty:   ", col_lty)
message("ltr:   ", col_ltr)
message("corpr: ", col_corpr)
message("infl:  ", col_infl)

# Create a working tibble GW0 with Date and available vars (rename to consistent names)
GW0 <- df %>% rename_with(.cols = everything(), .fn = identity)
# standardize names to short names if present
if(!is.na(col_sp500))    GW0 <- GW0 %>% rename(sp500 = !!sym(col_sp500))
if(!is.na(col_D12))      GW0 <- GW0 %>% rename(D12   = !!sym(col_D12))
if(!is.na(col_E12))      GW0 <- GW0 %>% rename(E12   = !!sym(col_E12))
if(!is.na(col_CRSP_SPvw))GW0 <- GW0 %>% rename(CRSP_SPvw = !!sym(col_CRSP_SPvw))
if(!is.na(col_CRSP_SPvwx))GW0<- GW0 %>% rename(CRSP_SPvwx = !!sym(col_CRSP_SPvwx))
if(!is.na(col_Rfree))    GW0 <- GW0 %>% rename(Rfree = !!sym(col_Rfree))
if(!is.na(col_svar))     GW0 <- GW0 %>% rename(svar = !!sym(col_svar))
if(!is.na(col_bm))       GW0 <- GW0 %>% rename(bm = !!sym(col_bm))
if(!is.na(col_ntis))     GW0 <- GW0 %>% rename(ntis = !!sym(col_ntis))
if(!is.na(col_tbl))      GW0 <- GW0 %>% rename(tbl = !!sym(col_tbl))
if(!is.na(col_lty))      GW0 <- GW0 %>% rename(lty = !!sym(col_lty))
if(!is.na(col_ltr))      GW0 <- GW0 %>% rename(ltr = !!sym(col_ltr))
if(!is.na(col_corpr))    GW0 <- GW0 %>% rename(corpr = !!sym(col_corpr))
if(!is.na(col_infl))     GW0 <- GW0 %>% rename(infl = !!sym(col_infl))
if(!is.na(col_AAA))      GW0 <- GW0 %>% rename(AAA = !!sym(col_AAA))
if(!is.na(col_BAA))      GW0 <- GW0 %>% rename(BAA = !!sym(col_BAA))

# Ensure Date remains first
GW0 <- GW0 %>% relocate(Date)

# ---- Add macro data from FRED ----
# We'll add: USRECM (NBER recession indicator), CPIAUCSL, TB3MS, TB4WK
message("Fetching FRED series: USRECM, CPIAUCSL, TB3MS, TB4WK ... (via quantmod::getSymbols)")

# quantmod::getSymbols will store in environment; use auto.assign = FALSE to get object directly
safe_get_fred <- function(symbol){
  tryCatch({
    tmp <- quantmod::getSymbols(symbol, src = "FRED", auto.assign = FALSE)
    # to data.frame with Date
    df_tmp <- data.frame(Date = index(tmp), coredata(tmp))
    names(df_tmp)[2] <- symbol
    return(df_tmp)
  }, error = function(e){
    warning("Failed to download FRED series: ", symbol, " : ", conditionMessage(e))
    return(NULL)
  })
}

fred_usrecm <- safe_get_fred("USRECM")
fred_cpi    <- safe_get_fred("CPIAUCSL")
fred_tb3ms  <- safe_get_fred("TB3MS")
fred_tb4wk  <- safe_get_fred("TB4WK")

# Merge these with GW0 by Date (left join so GW0 dates preserved)
GW_joined <- GW0
if(!is.null(fred_usrecm)) GW_joined <- left_join(GW_joined, fred_usrecm, by="Date")
if(!is.null(fred_cpi))    GW_joined <- left_join(GW_joined, fred_cpi, by="Date")
if(!is.null(fred_tb3ms))  GW_joined <- left_join(GW_joined, fred_tb3ms, by="Date")
if(!is.null(fred_tb4wk))  GW_joined <- left_join(GW_joined, fred_tb4wk, by="Date")

# rename FRED columns to nice names if present
GW_joined <- GW_joined %>% rename(NBER = USRECM, CPIAUCSL = CPIAUCSL, TB3MS = TB3MS, TB4WK = TB4WK)

# ---- Add S&P500 daily data (Yahoo) and convert daily volume to monthly sum ----
message("Fetching S&P500 daily data from Yahoo (^GSPC). This may take a few seconds.")
try({
  sp500_daily <- quantmod::getSymbols("^GSPC", src="yahoo", from = as.Date("1900-01-01"), auto.assign = FALSE)
  sp500_daily_df <- data.frame(Date = index(sp500_daily), coredata(sp500_daily))
  # Ensure column names: GSPC.Open, GSPC.High, ..., GSPC.Volume
  colnames(sp500_daily_df) <- gsub("X\\^GSPC\\.|\\^GSPC\\.", "", colnames(sp500_daily_df))
  # Aggregate volume by month -> sum
  sp500_daily_df$YearMon <- format(sp500_daily_df$Date, "%Y-%m")
  sp500_month_vol <- sp500_daily_df %>%
    group_by(YearMon) %>%
    summarise(Volume = sum(Volume, na.rm=TRUE)) %>%
    mutate(Date = as.Date(as.yearmon(YearMon)) + months(1) - days(1)) %>%
    select(Date, SP500Volume = Volume)
  GW_joined <- left_join(GW_joined, sp500_month_vol, by="Date")
}, silent = FALSE)

# ---- Now construct GW predictors like MATLAB code ----
# We'll create a tibble GW with Date and computed variables
GW <- GW_joined %>% select(Date)  # start

# helper for lag: create lagged vectors by month
lag_vec <- function(x, k=1){
  if(is.null(x)) return(NULL)
  dplyr::lag(x, n = k)
}

# compute equity premia: need CRSP_SPvw and Rfree
if("CRSP_SPvw" %in% names(GW_joined) && "Rfree" %in% names(GW_joined)){
  GW <- GW %>% mutate(
    log_xRet  = 100*(log(1 + GW_joined$CRSP_SPvw) - log(1 + GW_joined$Rfree)),  # log equity premium (%)
    smpl_xRet = 100*(GW_joined$CRSP_SPvw - GW_joined$Rfree)                     # simple equity premium (%)
  )
} else {
  warning("CRSP_SPvw and/or Rfree not found; log_xRet and smpl_xRet will be NA")
  GW <- GW %>% mutate(log_xRet = NA_real_, smpl_xRet = NA_real_)
}

# dp, dy, ep, de
GW$dp <- if(all(c("D12","sp500") %in% names(GW_joined))){
  log(GW_joined$D12 / GW_joined$sp500)
} else NA_real_

GW$dy <- if(all(c("D12","sp500") %in% names(GW_joined))){
  # MATLAB used lag(sp500) for dy: dy = log(D12 / lag(sp500))
  log(GW_joined$D12 / lag_vec(GW_joined$sp500,1))
} else NA_real_

GW$ep <- if(all(c("E12","sp500") %in% names(GW_joined))){
  log(GW_joined$E12 / GW_joined$sp500)
} else NA_real_

GW$de <- if(all(c("D12","E12") %in% names(GW_joined))){
  log(GW_joined$D12 / GW_joined$E12)
} else NA_real_

# copy svar, bm, ntis
GW$svar <- if("svar" %in% names(GW_joined)) GW_joined$svar else NA_real_
GW$bm   <- if("bm" %in% names(GW_joined)) GW_joined$bm else NA_real_
GW$ntis <- if("ntis" %in% names(GW_joined)) GW_joined$ntis else NA_real_

# scale variables by 100 as in MATLAB
GW$tbl <- if("tbl" %in% names(GW_joined)) 100 * GW_joined$tbl else NA_real_
GW$lty <- if("lty" %in% names(GW_joined)) 100 * GW_joined$lty else NA_real_
GW$ltr <- if("ltr" %in% names(GW_joined)) 100 * GW_joined$ltr else NA_real_
GW$tms <- if(!is.na(GW$lty) && !is.na(GW$tbl)) GW$lty - GW$tbl else NA_real_
GW$dfy <- if(all(c("BAA","AAA") %in% names(GW_joined))) 100*(GW_joined$BAA - GW_joined$AAA) else NA_real_
GW$dfr <- if(all(c("corpr","ltr") %in% names(GW_joined))) 100*(GW_joined$corpr - GW_joined$ltr) else NA_real_
GW$infl <- if("infl" %in% names(GW_joined)) 100 * GW_joined$infl else NA_real_
GW$infl_lag <- if("infl" %in% names(GW_joined)) 100 * lag_vec(GW_joined$infl,1) else NA_real_

# volatility measures (MATLAB used running MA on absolute returns)
ma_filter12 <- function(x){
  # rolling mean of last 12 non-NA absolute values; return NA if insufficient
  if(all(is.na(x))) return(rep(NA_real_, length(x)))
  zoo::rollapply(x, width = 12, FUN = mean, align = "right", fill = NA, na.rm = TRUE)
}
if(!all(is.na(GW$smpl_xRet))){
  GW$rvol_smpl <- sqrt(6*pi) * ma_filter12(abs(GW$smpl_xRet/100))
} else GW$rvol_smpl <- NA_real_
if(!all(is.na(GW$log_xRet))){
  GW$rvol_log  <- sqrt(6*pi) * ma_filter12(abs(GW$log_xRet/100))
} else GW$rvol_log <- NA_real_

# NBER indicator if present in joined data (USRECM)
GW$NBER <- if("NBER" %in% names(GW_joined)) GW_joined$NBER else NA_real_

# dividends from CRSP: GW.div = lag(sp500) * (CRSP_SPvw - CRSP_SPvwx)
if(all(c("sp500","CRSP_SPvw","CRSP_SPvwx") %in% names(GW_joined))){
  GW$div <- lag_vec(GW_joined$sp500,1) * (GW_joined$CRSP_SPvw - GW_joined$CRSP_SPvwx)
} else GW$div <- NA_real_

# d12 (smoothed dividends from Shiller)
GW$d12 <- if("D12" %in% names(GW_joined)) GW_joined$D12 else NA_real_

# SP500 monthly volume already attached (GW_joined$SP500Volume)
GW$SP500Volume <- if("SP500Volume" %in% names(GW_joined)) GW_joined$SP500Volume else NA_real_

# Attach descriptive names as attributes (like MATLAB Properties.UserData)
varNames <- c(
  "Equity risk premium (log)",
  "Equity risk premium (simple)",
  "Dividend-price ratio (log)",
  "Dividend yield (log)",
  "Earnings-price ratio (log)",
  "Payout ratio (log)",
  "Stock variance",
  "Book-to-market ratio",
  "Net equity expansion",
  "Treasury bill rate",
  "Long-term yield",
  "Long-term return",
  "Term spread",
  "Default yield spread",
  "Default return spread",
  "CPI Inflation",
  "CPI Inflation (lagged)",
  "Volatility of EQ (MA(12))",
  "Volatility of ln(EQ) (MA(12))",
  "NBER US recession indicator",
  "Dividends computed from CRSP_SPvw and CRSP_SPvwx (excl.Divs)",
  "D12 smoothed Dividends from Shiller (or GW0 raw file)"
)

attr(GW, "variable_descriptions") <- varNames

# ---- Save outputs ----
save_rds_path <- file.path(data_dir, paste0("GW_", vintage, ".rds"))
save_csv_path <- file.path(data_dir, paste0("GW_", vintage, ".csv"))
message("Saving outputs to: ", save_rds_path, " and ", save_csv_path)
saveRDS(GW, file = save_rds_path)
write.csv(GW, file = save_csv_path, row.names = FALSE)

message("Done. Created GW predictor file: ", save_rds_path)
