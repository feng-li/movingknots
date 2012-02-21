function [y,X,MeanSurface,StdSurface]=HwangTestData(T,Sigma,Model)

% Construct grid
if T==0
    % Only compute the mean and variance over a grid of 10000 points.
    [X1Grid,X2Grid]=meshgrid((1:2:199)/200,(1:2:199)/200);
    X=[X1Grid(:) X2Grid(:)];
    Errors=zeros(size(X,1),1);
else
    X=rand(T,2); % T uniform draws on the unit square.
    % Generate errors
    Errors=randn(T,1);
end

% Compute mean surface
switch Model
    case {'Simple','simple'} % Simple interaction function
        MeanSurface=10.391*((X(:,1)-0.4).*(X(:,2)-0.6)+0.36);
        StdSurface=ones(size(MeanSurface))*Sigma;
    case {'Radial','radial'} % Radial function
        r2=(X(:,1)-0.5).^2+(X(:,2)-0.5).^2;
        MeanSurface=24.234*(r2.*(0.75-r2));
        StdSurface=ones(size(MeanSurface))*Sigma;
    case {'Harmonic','harmonic'} % Harmonic function
        XTilde=X-0.5;
        MeanSurface=42.659*(0.1+XTilde(:,1).*(0.05+XTilde(:,1).^4-10*XTilde(:,1).^2.*XTilde(:,2).^2+5*XTilde(:,2).^4));
        StdSurface=ones(size(MeanSurface))*Sigma;
    case {'Additive','additive'} % Additive function
        MeanSurface=1.3356*(  1.5*(1-X(:,1)) + exp(2*X(:,1)-1).*sin(3*pi*(X(:,1)-0.6).^2) +  exp(3*(X(:,2)-0.5)).*sin(4*pi*(X(:,2)-0.9).^2) );
        StdSurface=ones(size(MeanSurface))*Sigma;
    case {'Complex','complex'} % Complicated interaction function
        MeanSurface=1.9*( 1.35 + exp(X(:,1)).*sin(13*(X(:,1)-0.6).^2).*exp(-X(:,2)).*sin(7*X(:,2))  );
        StdSurface=ones(size(MeanSurface))*Sigma;
    otherwise
end
y=MeanSurface+StdSurface.*Errors;

