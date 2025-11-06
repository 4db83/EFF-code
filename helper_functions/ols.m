function olsout = ols(y,x,no_const,Xnames,L,INCLUDE_PW,print_results,Recession_indicator)
% olsout = ols(y,X,L,INCLUDE_PW,Recession_indicator,no_const);
% wrapper function to fullols to print results to screen.

SetDefaultValue(3,'no_const',0)						% default is to include a constant
SetDefaultValue(4,'Xnames',[]);
SetDefaultValue(5,'L',[]);								% set default value for truncation lag to L = 
SetDefaultValue(6,'INCLUDE_PW',[]);				% set to 1 to include pre-whitening in LRV computation
SetDefaultValue(7,'print_results',1)			% print results to screen is default
SetDefaultValue(8,'Recession_indicator',[]);

if isa(y,'table')
%   disp('we have a table datatype')
  y_name = y.Properties.VariableNames;
  y = y.Variables;
end

if isa(x,'table')
%   disp('we have a table datatype')
  x_name = x.Properties.VariableNames;
  x = x.Variables;
  Xnames = x_name;
end

% olsout = fullols(y,x,L,INCLUDE_PW,Recession_indicator,no_const);
olsout = fullols(y,x,no_const,L,INCLUDE_PW,Recession_indicator);

% sqrt(diag(olsout.HAC_VCV))
% sqrt(diag(olsout.HAC_VCV_pw_ARMA11))
% sqrt(diag(olsout.HAC_VCV_pw_AR1))

if ischar(print_results)
	print_fullols(olsout, Xnames, [], print_results);
elseif print_results==1
	print_fullols(olsout, Xnames);
% 	print_fullols(olsout, Xnames, [], print_results);
end


