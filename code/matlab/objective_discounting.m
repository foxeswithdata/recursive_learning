%The objective function diminishing returns
function [eval] = objective_discounting(X, param)

%Remember that were minimising kf * Mt + (1-kf) * St
eval = 0;

time_sequence = param.deltat:param.deltat:param.horizon;
for i=1:size(time_sequence,2)
    eval = eval - (param.kf * X(2,i) + (1-param.kf) * X(3,i))* exp(param.kd * time_sequence(i));
end

end