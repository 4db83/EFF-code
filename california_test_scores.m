clc; clear;
% set path to local helper functions
addpath(genpath('.\helper_functions')) 
% read in the data
dt = readtable('.\data/caschool.xlsx');
% inspec the data that was loaded
head(dt)
tail(dt)

%% this is the main part that 
y = dt.testscr;
x = dt.str;
% xgrid
xg = linspace(10,30,1e3)';
xg = [ones(length(xg),1) xg];

fprintf([repmat('=',1,90) '\n']);
fprintf( 'Dependent Variable: Test Score\n')
% the is the call to the OLS function
fit1 = ols(y,x,[],[],0);

clf
hold on;
  scatter(x,y,33,clr(1),'filled')
  plot(xg,xg*fit1.bhat,'LineWidth',2)
  xlim([10 30])
  set(gca,'Position',[.06 .4 .9 .3],'FontName','Times','FontSize',14)
hold off
ylabel('Test Score');
xlabel('Student-Teacher-Ratio')
box on;

