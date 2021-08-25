param.S0 = S0;
param.M0 = M0;
param.W0 = W0;
param.X0 = [param.M0 ; param.S0; param.W0];
param.horizon = tw;
param.ks = ks;
param.kr = kr;
param.kp = kp;
param.kk = kk;
param.kw = kw;
param.kf = kf;
param.ri = ri;
param.kd = kd;
param.deltat = delta_t;
param.kp=kp;

fn = file_name

%Time sequence
time_sequence = param.deltat:param.deltat:param.horizon;
input_length = size(time_sequence, 2);

%Define optimisation inputs
X0 = zeros(4,input_length);
X0(1,:) = ones(1,input_length)*ks_start;
X0(2,:) = ones(1,input_length)*param.M0;
X0(3,:) = ones(1,input_length)*param.S0;
X0(4,:) = ones(1,input_length)*param.W0;
lb = zeros(4,input_length);
ub = ones(4,input_length);
ub(1,:) = ub(1,:)*param.ks;
ub(2:4,:) = ub(2:4,:)*inf;
A = [];
b = [];
Aeq = [];
beq = [];

opts = optimoptions('fmincon','MaxFunctionEvaluations',100000,'MaxIterations',100000);

%Perform optimisation
obj = @(x) objective_discounting(x, param);
nlc = @(x) nonlcon(x, param);
[x_orig,fval,exitflag,output] = fmincon(obj,X0,A,b,Aeq,beq,lb,ub,nlc, opts);
if(param.kf ~= 0)
    param.kf = 0;
    obj = @(x) objective_discounting(x, param);
    nlc = @(x) nonlcon(x, param);
    [x_maxS,fval,exitflag,output] = fmincon(obj,X0,A,b,Aeq,beq,lb,ub,nlc, opts);
else
    x_maxS = x_orig;
end
if(param.kf ~= 1)
    param.kf = 1;
    obj = @(x) objective_discounting(x, param);
    nlc = @(x) nonlcon(x, param);
    [x_maxM,fval,exitflag,output] = fmincon(obj,X0,A,b,Aeq,beq,lb,ub,nlc, opts);
else
    x_maxM = x_orig;
end

x = [x_orig; x_maxS; x_maxM];

if exitflag < 0 
    uout = -999;
else 
    uout = x_orig(1,2);
end

disp(uout)

csvwrite(fn, x')



% exit;