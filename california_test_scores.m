clc; clear; clf;
addpath(genpath('D:\matlab.tools\db.toolbox\db')) % set path to functions
% read in the data
dt = readtable('.\data/caschool.xlsx');

%%
y = dt.testscr;
x = dt(:,'str');
x = dt.str;
% xgrid
xg = linspace(10,30,1e3)';
xg = [ones(length(xg),1) xg];
fit1 = ols(y,x,[],{'str'});

clf
hold on;
%   scatter(x,y,'filled','MarkerEdgeColor',clr(1),'MarkerFaceColor',clr(1))
  scatter(x,y,33,clr(1),"filled")
%   sz,c
  plot(xg,xg*fit1.bhat,'LineWidth',2,'Color',clr(2))
  xlim([10 30])
  setplot([.06 .4 .9 .3],14);
hold off
box on;

