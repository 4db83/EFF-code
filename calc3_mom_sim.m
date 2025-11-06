a = 3;
b = 10;
T = 1e3;      % sample size T
% number of simulations used for the sampling distribution
nSims = 1e5;  
tic
y = randg(a,T,nSims)./b;
toc
% define xgrid over which to plot the density
% round(max(y(:)))
xgrd = linspace(0,round(max(y(:))),1e3)';
% anonymous function for this version of the gamma pdf
py = @(y,a,b) (b^a/gamma(a))*y.^(a-1).*exp(-b.*y);

% compute MOM from 1
clc;
y_bar   = mean(y)';
y2_bar  = mean(y.^2)';
y_1_bar = mean(1./y)';
% a)
a_hat1 = y_bar.^2./(y2_bar - y_bar.^2);
b_hat1 = a_hat1./y_bar;
fprintf('    a1(se)    b1(se)\n')
disp([ mean(a_hat1) mean(b_hat1);
        var(a_hat1)  var(b_hat1) ]);
% b)
a_hat2 = y_bar.*y_1_bar./(y_bar.*y_1_bar - 1);
b_hat2 = a_hat2./y_bar;
fprintf('    a2(se)    b2(se)\n')
disp([ mean(a_hat2) mean(b_hat2);
        var(a_hat2)  var(b_hat2) ] );
% c)
SM = y_1_bar.^2.*y2_bar;
a_hat3 = 1/2*( (2*SM+1)+sqrt(8*SM+1) )./(SM-1);
b_hat3 = (a_hat3 - 1).*y_1_bar;
fprintf('    a3(se)    b3(se)\n')
disp([ mean(a_hat3) mean(b_hat3);
        var(a_hat3)  var(b_hat3) ] );

% plotting of sampling distributions
XLMa = [0 6] ; % x-axis limits to keep aspect ratio of plots fixed
XLMb = [2 20]; % x-axis limits to keep aspect ratio of plots fixed
clf; tiledlayout(6,2)
nexttile(1,[1 2]); 
  histogram(y(:),'Normalization','pdf'); hold on;
  plot(xgrd,py(xgrd,a,b),'r-','LineWidth',2)
  % plot(xgrd,gampdf(xgrd,a,1/b)) % mablab build-in function
  hold off;

nexttile; 
  histogram(a_hat1,'Normalization','pdf'); 
  xlim(XLMa);
  xlabel('$\alpha (a)$','Interpreter','latex')
  xline(a,'r-','LineWidth',2); 

nexttile; 
  histogram(b_hat1,'Normalization','pdf'); 
  xlim(XLMb);
  xlabel('$\beta (a)$','Interpreter','latex')
  xline(b,'r-','LineWidth',2); 

nexttile; 
  histogram(a_hat2,'Normalization','pdf'); 
  xlim(XLMa);
  xlabel('$\alpha (b)$','Interpreter','latex')
  xline(a,'r-','LineWidth',2);  

nexttile; 
  histogram(b_hat2,'Normalization','pdf'); 
  xlim(XLMb);
  xlabel('$\beta (b)$','Interpreter','latex')
  xline(b,'r-','LineWidth',2); 

nexttile; 
  histogram(a_hat3,'Normalization','pdf'); 
  xlim(XLMa);
  xlabel('$\alpha (c)$','Interpreter','latex')
  xline(a,'r-','LineWidth',2);  

nexttile; 
  histogram(b_hat3,'Normalization','pdf'); 
  xlim(XLMb);
  xlabel('$\beta (b)$','Interpreter','latex')
  xline(b,'r-','LineWidth',2); 



















  
  
  
  
  
  
  
  
  
  
  
  
  
%EOF














