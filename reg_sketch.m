clear; clf; clc;
% set plotting defaults
set(groot,'defaultLineLineWidth',2); set(groot,'defaultAxesFontSize',14)
set(groot,'defaultAxesXTickLabelRotationMode','manual')
set(groot,'defaultAxesFontName','Times New Roman')
% set path to local toolbox if needed
% addpath(genpath('D:\matlab.tools\db.toolbox\db'))
addpath(genpath('./helper_functions')) 

% Xgrid values
x = linspace(5,100,1e3)';

% a) 
clf;
subplot(3,1,1)
hold on;
  plot(x,2 + 3*log(x),'Color',clr(3))                            % a)
  plot(x,2 - 3*log(x),'Color',clr(2))                            % b)  
  Z = 1; plot(x,2 + 3*log(x) + 4*Z,'Color',clr(1))               % c) i)
  Z = 0; plot(x,2 + 3*log(x) + 4*Z, '--','Color',clr(4))         % c) ii)
hold off;box on;grid_on
hline(0)
ylabel('$\hat{Y}$','Interpreter','latex');
xlabel('$X$','Interpreter','latex');
legend({'$\hat{Y} = 2 + 3\ln(X)$', ...
        '$\hat{Y} = 2 - 3\ln(X)$' ...
        '$\hat{Y} = 2 + 3\ln(X) + 4Z, Z = 1$',  ...
        '$\hat{Y} = 2 + 3\ln(X) + 4Z, Z = 0$'}, ...
  'Interpreter','latex','Location','best');

subplot(3,1,2)
hold on;
  Z = 1; plot(x,2 + 3*log(x) + 4*Z - 1*Z*log(x),'Color',clr(1))  % d) i)
  Z = 0; plot(x,2 + 3*log(x) + 4*Z - 1*Z*log(x),'Color',clr(2))  % d) ii)
hold off;box on;grid_on
% hline(0)
ylabel('$\hat{Y}$','Interpreter','latex');
xlabel('$X$','Interpreter','latex');
legend({'$\hat{Y} = 2 + 3\ln(X) + 4Z -Z\times\ln(X), Z = 1$' , ...
        '$\hat{Y} = 2 + 3\ln(X) + 4Z -Z\times\ln(X), Z = 0$'}, ...
  'Interpreter','latex','Location','best');

subplot(3,1,3)
hold on;
  plot(x, 1 + 125*x - 0.01*x.^2, 'Color',clr(3))                 % e) 
hold off;box on;grid_on
% hline(0)
ylabel('$\hat{Y}$','Interpreter','latex');
xlabel('$X$','Interpreter','latex');
legend({'$\hat{Y} = 1 + 125X - 0.01X^2$'}, ...
  'Interpreter','latex','Location','best');



