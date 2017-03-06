function results=pis_bootstrap(nBoots,pis,nRuns)
% pis_bootstrap is used to generate standard errors for the probablity estimates from calc_MLE. 
% pis_bootstrap returns a N x nBoots matrix (where N is the # of
% strategies), containing probability distributions across the N strategies
% from nBoots different bootstraps. The SEs are then given by
% std(results'), and the t-test p-values for different comparisons are
% generated using the normcdf function, together with the appropriate
% estimate and SE
%
%
% Inputs:
%
% nBoots = number of bootstrap samples to perform
% pis = output from calc_pis containing the likelihood of each strategy for
%       each subject
% nRuns = number of optimization runs to perform for each bootstrap (10
%         should be sufficient, at least for our data)
%
% Output:
% 
% results = probability distribution over the strategy set for each bootstrap


% initialization
rand('state',floor(sum(clock*10000)));
warning off all
fnmeHere=['drew_ll_' num2str(round(rand*1e6))];
results=[];


% main loop
for i=1:nBoots
    
    ['bootstrap sample # ' num2str(i)]
    % randomly resample (with replacement) a new sample of equal size
    dataHere=pis(randsample(length(pis(:,1)),length(pis(:,1)),'true'),:);
    
    % perform optimization on the bootstrapped sample to maximize
    % likelihood fn
    resultHere=run_internal(dataHere,fnmeHere,nRuns);
    
    % save results
    results=[results resultHere];

end

% tidy up
delete([fnmeHere '.m'],'w');
eval(['save drew2_'  num2str(round(rand*1e8)) '.mat']);



function result=run_internal(pis,fnmeHere,nRuns)
% internal function to perform maximization of likelihood fn



% create vector of symbolic variabls p1 thru pN (where N is the # of
% strategies)
ps=[];
pIdx=1;
for i=1:length(pis(1,:))%(max(find(stratsToUse==1))-1)%length(stratsToUse)-1
        eval(['syms p' num2str(pIdx) ';']);
        eval(['ps=[ps p' num2str(pIdx) '];']);
        pIdx=pIdx+1;
end

% because the probabilities have to sum to 1, replace pN with 1-sum(p1:pN-1)
eval(['tIdx=find(ps==p' num2str(length(pis(1,:))) ');']);    
ps(tIdx)=0;
ps(tIdx)=1-sum(ps);

% add up log likelihood fn
LL=0;
for i=1:length(pis(:,1))
    LL=LL+log(pis(i,:)*transpose(ps));
end

% since the optimizier is a minimizer rather than a maximizer, multiply the
% LL by -1
LL=-1*(LL);

% create a function file that takes a set of probabilities over the
% strategies as input and returns the corresponding LL (times -1)
delete([fnmeHere '.m'],'w');
f=fopen([fnmeHere '.m'],'w');
fprintf(f,['function LL=' fnmeHere '(x)']);
fprintf(f,'\r');
for i=1:(length(pis(1,:))-1)
fprintf(f,['p' num2str(i) '=x(' num2str(i) ');']);
fprintf(f,'\r');
end
fprintf(f,['G=x(' num2str(i+1) ');']);
fprintf(f,'\r');
fprintf(f,'LL=');
fprintf(f,'%s',char(LL));
fprintf(f,';');
fprintf(f,'\r');
fprintf(f,'if (~isreal(LL)) | isnan(LL)');
fprintf(f,'\r');
fprintf(f,'LL=1e6;');
fprintf(f,'\r');
fprintf(f,'end');
fprintf(f,'\r');
fclose(f);
rehash


% use the fmincon minimization function to minimize the inverse LL (and thus maximize LL) 
rand('state',floor(sum(clock*10000)));
numStratsToCheck=length(pis(1,:));
constraintA=[eye(numStratsToCheck); -1*eye(numStratsToCheck); [-1*ones(1,numStratsToCheck-1) 0]; [ones(1,numStratsToCheck-1) 0] ];
constraintB=[ones(numStratsToCheck-1,1); 10; zeros(numStratsToCheck,1); 0; 1];
options = optimset('MaxSQPIter', round(numStratsToCheck),'Display', 'off');
warning off all

% to make sure you get the real global minimum, do the minimization nRuns
% times, and then take the best result. In our experience with our data,
% there is always a clear 'correct' minimum.
resultsHere=[];
for run=1:nRuns
    bla=[0; sort(rand(numStratsToCheck-1,1)); 1];
    inits=bla(2:end)-bla(1:end-1);
    x=[inits(1:(numStratsToCheck-1));rand];
    eval(['[vals, val]=fmincon(@' fnmeHere ',x,constraintA,constraintB,[],[],[],[],[],options);']);
    resultsHere=[resultsHere [vals; real(val)/1e3]];
end

% pick out the best fit and store it
bestFit=find(resultsHere(end,:)==min(resultsHere(end,:)));
result=resultsHere(:,bestFit(1));
result=[result(1:(numStratsToCheck-1)); 1-sum(result(1:(numStratsToCheck-1))); result((numStratsToCheck))];