% %Perform optimisation
% obj = @(x) objective(x, param);
% nlc = @(x) nonlcon(x, param);
% [x,fval,exitflag,output] = fmincon(obj,X0,A,b,Aeq,beq,lb,ub,nlc, opts);

%Optimisation functions
function [c, ceq] = nonlcon(X, param)

ceq = zeros(3,size(X,2)+1);

%be careful of the subscripts:
%X = [ u(0), u(1), ... , u(N-1)
%      xM(1), xM(2), ..., xM(N)
%      xS(1), xS(2), ..., xS(N)
%      xW(1), xW(2), ..., xW(N)]
%kp = [kp(0), kp(1), ... kp(N-1)]
for i = 2:size(X,2)
    xm = X(2,i-1) + param.deltat * (X(1,i) * X(3,i-1) - param.kk * X(2,i-1));
    if X(4, i-1) > 0
        kp = param.kp;
    else
        kp = 0;
    end
    xs = X(3,i-1) + param.deltat * ((kp - param.kr) * X(2,i-1) - X(1,i) * X(3,i-1));
    xw = X(4,i-1) + param.deltat * (param.ri - param.kw * kp * X(2, i-1));
    ceq(:,i) = X(2:4,i) - [xm ; xs ; xw];
end

%Find the values for initial states
xm = param.X0(1) + param.deltat * (X(1,i) * X(3,i-1) - param.kk * X(2,i-1));
if param.X0(3) > 0
        kp = param.kp;
    else
        kp = 0;
    end
xs = param.X0(2) + param.deltat * ((kp - param.kr) * param.X0(1) - X(1,1) * param.X0(2));
xw = param.X0(3) + param.deltat * (param.ri - param.kw * kp * param.X0(1));
ceq(:,1) = X(2:4,1) - [xm ; xs ; xw];

%Set c as empty/zero
c=[];
end