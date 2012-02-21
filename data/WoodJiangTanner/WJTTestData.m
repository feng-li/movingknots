function [y,X,MeanSurface,StdSurface]=WJTTestData(T,SigmaDGP,DeltaDGP,Model)

% Construct grid
if T==0
    % Only compute the mean and variance over a grid of 10000 points.
    X=(1/100:1/100:1)';
    Errors=zeros(size(X,1),1);
else
    X=(1/T:1/T:1)';
    % Generate errors
    Errors=randn(T,1);
end

% Compute mean surface
switch Model
    case {'SineWave','sinewave'} % Sine wave
        MeanSurface=2*sin(4*pi*X);
    case {'NormMixture1','normmixture1'} % normal mixture
        MeanSurface=normpdf(X,0.6,0.2)+normpdf(X,0.15,0.05);
    case {'NormMixture2','normmixture2'} % normal mixture - elevated
        MeanSurface=normpdf(X,0.6,0.2)+normpdf(X,0.15,0.05)+4*ones(size(X)).*(X>0.3);
    case {'BrokenSineWave','brokensinewave'} % Broken sine wave
        MeanSurface=8*sin(4*pi*X)-sign(0.3-X)-sign(X-0.6);
    case {'BlockFunction','blockfunction'}
        MeanSurface=0.5*(X>=0 & X<0.25)+2*(X>=0.25 & X<0.75)+1*(X>=0.75 & X<=1);
    otherwise
end

StdSurface=SigmaDGP*exp(DeltaDGP'*X/2);

y=MeanSurface+StdSurface.*Errors;

XPred=(1/100:1/100:1)';
WJTModel=Model;
if T>0
    save(WJTModel,'y','X','MeanSurface','StdSurface','WJTModel','XPred','SigmaDGP','DeltaDGP')
end