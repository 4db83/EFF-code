clc; clear;
% set path to local helper functions
addpath(genpath('./helper_functions')) 
% read in the data
dt = readtable('./data/caschool.xlsx');
% inspect the data that was loaded
head(dt);tail(dt);

%% this is the main part that 
y = dt.testscr;
x = dt.str;
% xgrid
xg = linspace(10,30,1e3)';
xg = [ones(length(xg),1) xg];

fprintf([repmat('=',1,90) '\n']);
fprintf('\t\t\t\t\t Dependent Variable: Test Score\n')
% the is the call to the OLS function
fit1 = ols(y,x,[],[],0);

clf; % clear old figures, if exis;
hold on; % to plot multiple lines on top
  scatter(x,y,33,clr(1),'filled')
  plot(xg,xg*fit1.bhat,'LineWidth',2,'Color',clr(2))
  xlim([10 30])
  vline(mean(x),'k-')
  hline(mean(y),'k-')
hold off;
grid on; 
% adjust gca (current figure/axis) position, font, grid properties
set(gca,'Position',[.06 .4 .9 .3],...
        'FontName','Times','FontSize',14, ...
        'GridLineStyle',':','GridAlpha',1/3)
ylabel('Test Score');
xlabel('Student-Teacher-Ratio')
box on;