%The objective function
function [eval] = objective(X, param)

%Remember that were minimising kf * Mt + (1-kf) * St
eval = -sum(param.kf * X(2,:)' + (1-param.kf) * X(3,:)');

end


