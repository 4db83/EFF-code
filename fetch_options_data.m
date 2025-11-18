T = fetch_yahoo_options_robust('^GSPC', 'next');   % nearest expiry
% view first rows
head(T)


function optTable = fetch_yahoo_options_robust(symbol, expiryDate)
% Robust fetch of Yahoo option chain with retries, fallback endpoint, and User-Agent.
% Usage: T = fetch_yahoo_options_robust('^GSPC','next');

if nargin < 2 || strcmpi(expiryDate,'next'), expiryDate = 'next'; end

bases = { ...
    'https://query1.finance.yahoo.com/v7/finance/options/'; ...
    'https://query2.finance.yahoo.com/v7/finance/options/' ...
    };

maxTries = 6;
baseIdx = 1;
lastErr = [];
for baseIdx = 1:numel(bases)
    base = bases{baseIdx};
    try
        % first call to get expirations
        url0 = [base urlencode(symbol)];
        % set weboptions with custom header and timeout
        opts = weboptions('Timeout', 20, ...
                          'HeaderFields', {'User-Agent','Mozilla/5.0 (MATLAB)'; 'Accept','application/json'});
        disp(['Trying Yahoo endpoint: ' base]);
        optionsJson = webread(url0, opts);

        expirations = optionsJson.optionChain.result{1}.expirationDates;
        if strcmpi(expiryDate,'next')
            expiry_ts = expirations(1);
        elseif isnumeric(expiryDate)
            expiry_ts = expiryDate;
        else
            target = posixtime(datetime(expiryDate));
            later = expirations(expirations >= target);
            if isempty(later)
                expiry_ts = expirations(end);
            else
                expiry_ts = later(1);
            end
        end

        % now fetch the specific expiry
        url = sprintf('%s%s?date=%d', base, urlencode(symbol), expiry_ts);

        % attempt retries with exponential backoff
        for attempt = 1:maxTries
            try
                data = webread(url, opts);
                % success -> parse and return
                optTable = parse_yahoo_json(data);
                return;
            catch innerEx
                lastErr = innerEx;
                if contains(innerEx.message, '429') || contains(innerEx.message,'Too Many Requests')
                    waitSec = 2^attempt;
                    fprintf('Yahoo 429 received — backoff %d sec (attempt %d/%d)\n', waitSec, attempt, maxTries);
                    pause(waitSec);
                    continue;
                else
                    rethrow(innerEx);
                end
            end
        end
    catch outerEx
        lastErr = outerEx;
        fprintf('Endpoint %s failed: %s\n', base, outerEx.message);
        % try next base
    end
end

% If reached here, both endpoints failed after retries
error('Failed to fetch Yahoo options after retries. Last error: %s\n\nSuggestion: download CSV from your vendor or use parse_option_csv and upload the CSV for me to process.', lastErr.message);

end

function optTable = parse_yahoo_json(data)
    result = data.optionChain.result{1};
    underlying = result.quote.regularMarketPrice;
    exp_ts = result.options{1}.expirationDate;
    exp_dt = datetime(exp_ts,'ConvertFrom','posixtime','TimeZone','UTC');

    calls = result.options{1}.calls;
    puts  = result.options{1}.puts;

    toTable = @(S) table([S.strike]', [S.lastPrice]', [S.bid]', [S.ask]', ...
                         'VariableNames', {'Strike','Last','Bid','Ask'});
    if ~isempty(calls)
        Tc = toTable(calls);
        Tc.Type = repmat("call",height(Tc),1);
        Tc.Expiry = repmat(exp_dt,height(Tc),1);
    else
        Tc = table([],[],[],[],'VariableNames',{'Strike','Last','Bid','Ask'});
        Tc.Type = string.empty; Tc.Expiry = datetime.empty;
    end

    if ~isempty(puts)
        Tp = toTable(puts);
        Tp.Type = repmat("put",height(Tp),1);
        Tp.Expiry = repmat(exp_dt,height(Tp),1);
    else
        Tp = table([],[],[],[],'VariableNames',{'Strike','Last','Bid','Ask'});
        Tp.Type = string.empty; Tp.Expiry = datetime.empty;
    end

    optTable = [Tc; Tp];
    optTable.Underlying = repmat(underlying,height(optTable),1);

    % mid prices
    mid = optTable.Bid;
    hasBidAsk = ~isnan(optTable.Bid) & ~isnan(optTable.Ask);
    mid(hasBidAsk) = 0.5*(optTable.Bid(hasBidAsk) + optTable.Ask(hasBidAsk));
    mid(~hasBidAsk) = optTable.Last(~hasBidAsk);
    optTable.Mid = mid;

    optTable = sortrows(optTable,'Strike');
end







% function optTable = fetch_yahoo_options(symbol, expiryDate)
% % fetch_yahoo_options  Download options chain from Yahoo Finance
% % 
% % symbol: string, e.g. '^GSPC' (SPX). Note: on Yahoo, SPX symbol often '^GSPC'.
% % expiryDate: datetime or datenum or 'next' for nearest; or unix timestamp (seconds)
% %
% % Returns a MATLAB table with columns:
% %  Strike, Type ('call'/'put'), Bid, Ask, Last, Expiry (datetime)
% %
% % Example:
% %  T = fetch_yahoo_options('^GSPC', 'next');
% 
% if nargin < 2 || strcmpi(expiryDate,'next')
%     expiryDate = 'next';
% end
% 
% % Get main options page (first to obtain list of expiries)
% base = 'https://query2.finance.yahoo.com/v7/finance/options/';
% url0 = [base urlencode(symbol)];
% optionsJson = webread(url0);   % may error if connection blocked
% 
% % parse expirations (seconds since epoch)
% expirations = optionsJson.optionChain.result{1}.expirationDates;
% if strcmpi(expiryDate,'next')
%     expiry_ts = expirations(1);
% elseif isnumeric(expiryDate)
%     expiry_ts = expiryDate;
% else
%     % parse human date to nearest expiry
%     target = posixtime(datetime(expiryDate));
%     % choose expiration closest >= target
%     later = expirations(expirations >= target);
%     if isempty(later)
%         expiry_ts = expirations(end);
%     else
%         expiry_ts = later(1);
%     end
% end
% 
% % fetch specific expiry chain
% url = sprintf('%s%s?date=%d', base, urlencode(symbol), expiry_ts);
% data = webread(url);
% 
% % extract quotes
% result = data.optionChain.result{1};
% underlying = result.quote.regularMarketPrice;
% exp_dt = datetime(expiry_ts,'ConvertFrom','posixtime','TimeZone','UTC');
% 
% % collect calls and puts
% calls = result.options{1}.calls;
% puts  = result.options{1}.puts;
% 
% % Helper to convert struct array to table
% toTable = @(S,type) cell2table(...
%     [ {S.strike}' {S.lastPrice}' {S.bid}' {S.ask}' ], ...
%     'VariableNames', {'Strike','Last','Bid','Ask'} );
% if ~isempty(calls)
%     Tc = toTable(calls,'call');
%     Tc.Type = repmat("call",height(Tc),1);
%     Tc.Expiry = repmat(exp_dt,height(Tc),1);
% else
%     Tc = table([],[],[],[],'VariableNames',{'Strike','Last','Bid','Ask'});
%     Tc.Type = string.empty;
%     Tc.Expiry = datetime.empty;
% end
% 
% if ~isempty(puts)
%     Tp = toTable(puts,'put');
%     Tp.Type = repmat("put",height(Tp),1);
%     Tp.Expiry = repmat(exp_dt,height(Tp),1);
% else
%     Tp = table([],[],[],[],'VariableNames',{'Strike','Last','Bid','Ask'});
%     Tp.Type = string.empty;
%     Tp.Expiry = datetime.empty;
% end
% 
% optTable = [Tc; Tp];
% optTable.Underlying = repmat(underlying,height(optTable),1);
% 
% % compute mid price where bid/ask exist; fallback to last price
% mid = optTable.Bid;
% hasBidAsk = ~isnan(optTable.Bid) & ~isnan(optTable.Ask);
% mid(hasBidAsk) = 0.5*(optTable.Bid(hasBidAsk) + optTable.Ask(hasBidAsk));
% mid(~hasBidAsk) = optTable.Last(~hasBidAsk);
% 
% optTable.Mid = mid;
% 
% % sort by strike ascending
% optTable = sortrows(optTable, 'Strike');
% 
% end
