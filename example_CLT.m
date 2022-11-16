% example of CLT to replicate the plot on page 10 of EFF-02.
clear;clc;clf;
rng(123);   % fix the random number generator see
n = 100;    % sample size
S = 5e5;    % number of simulations just for the histograms
% generate draws from a Bernoulli RV with success probabilty = p
p = 0.78;
x = rand(S,n) < p;

%%
clf;
x2  = mean(x(:,1:2),2);
x5  = mean(x(:,1:5),2);
x25 = mean(x(:,1:25),2);
x100 = mean(x(:,1:100),2);
subplot(2,2,1)
histogram(x2,25,'Normalization','probability');
subtitle('$n=1$','Interpreter','Latex','Units','normalized','Position',[.5 .95],'FontSize',14)
ylim([0 0.7])
subplot(2,2,2)
histogram(x5,25,'Normalization','probability');
ylim([0 1/2])
subtitle('$n=5$','Interpreter','Latex','Units','normalized','Position',[.5 .95],'FontSize',14)
subplot(2,2,3)
histogram(x25,18,'Normalization','probability');
ylim([0 1/4])
xlim([0 1])
subtitle('$n=25$','Interpreter','Latex','Units','normalized','Position',[.5 .95],'FontSize',14)
subplot(2,2,4)
histogram(x100,38,'Normalization','probability');
ylim([0 1/8])
xlim([0 1])
subtitle('$n=100$','Interpreter','Latex','Units','normalized','Position',[.5 .95],'FontSize',14)