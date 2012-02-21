function [y,X,MeanSurface,StdSurface]=HwangTestDataWithPlots(T,Sigma,Model,PlotData)

% Construct grid
if T==0
    % Only compute the mean and variance over a grid of 10000 points.
    [X1Grid,X2Grid]=meshgrid((1:2:199)/200,(1:2:199)/200);
    X=[X1Grid(:) X2Grid(:)];
    Errors=zeros(size(X,1),1);
else
    X=rand(T,2); % T uniform draws on the unit square.
    % Generate errors
    Errors=randn(T,1)*Sigma;
end
figure
for Model=1:5
% Compute mean surface
switch Model
    case 1 % Simple interaction function
        MeanSurface=10.391*((X(:,1)-0.4).*(X(:,2)-0.6)+0.36);
    case 2 % Radial function
        r2=(X(:,1)-0.5).^2+(X(:,2)-0.5).^2;
        MeanSurface=24.234*(r2.*(0.75-r2));
    case 3 % Harmonic function
        XTilde=X-0.5;
        MeanSurface=42.659*(0.1+XTilde(:,1).*(0.05+XTilde(:,1).^4-10*XTilde(:,1).^2.*XTilde(:,2).^2+5*XTilde(:,2).^4));
    case 4 % Additive function
        MeanSurface=1.3356*(  1.5*(1-X(:,1)) + exp(2*X(:,1)-1).*sin(3*pi*(X(:,1)-0.6).^2) +  exp(3*(X(:,2)-0.5)).*sin(4*pi*(X(:,2)-0.9).^2) );
    case 5 % Complicated interaction function
        MeanSurface=1.9*( 1.35 + exp(X(:,1)).*sin(13*(X(:,1)-0.6).^2).*exp(-X(:,2)).*sin(7*X(:,2))  );
    otherwise
end
StdSurface=ones(size(MeanSurface))*Sigma;
y=MeanSurface+Errors;

XTemp=X;
if PlotData==1
    FigHandle=figure;
    for i=1:4
        subplot(2,2,i)
        hold on
        if i==2
            view([70 20])
        end
        if i==3
            view([-70 30])
        end
        if i==4
            view([-270 10])
        end
        plot3(XTemp(:,1),XTemp(:,2),y,'k.')

        % Computing the mean surface over [0,1]x[0,1]
        [X1Grid,X2Grid]=meshgrid((1:2:199)/200,(1:2:199)/200);
        X=[X1Grid(:) X2Grid(:)];
        switch Model
            case 1 % Simple interaction function
                MeanSurface=10.391*((X(:,1)-0.4).*(X(:,2)-0.6)+0.36);
            case 2 % Radial function
                r2=(X(:,1)-0.5).^2+(X(:,2)-0.5).^2;
                MeanSurface=24.234*(r2.*(0.75-r2));
            case 3 % Harmonic function
                XTilde=X-0.5;
                MeanSurface=42.659*(0.1+XTilde(:,1).*(0.05+XTilde(:,1).^4-10*XTilde(:,1).^2.*XTilde(:,2).^2+5*XTilde(:,2).^4));
            case 4 % Additive function
                MeanSurface=1.3356*(  1.5*(1-X(:,1)) + exp(2*X(:,1)-1).*sin(3*pi*(X(:,1)-0.6).^2) +  exp(3*(X(:,2)-0.5)).*sin(4*pi*(X(:,2)-0.9).^2) );
            case 5 % Complicated interaction function
                MeanSurface=1.9*( 1.35 + exp(X(:,1)).*sin(13*(X(:,1)-0.6).^2).*exp(-X(:,2)).*sin(7*X(:,2))  )
            otherwise
        end
        Y=reshape(MeanSurface,size(X1Grid));
        surf(X1Grid,X2Grid,Y);
        xlabel('x1')
        ylabel('x2')
        zlabel('y')
    end
end
X=XTemp;
end