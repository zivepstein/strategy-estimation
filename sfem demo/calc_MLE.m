function [MLE_estimates, SEs, pvals]=calc_MLE(pisAll,nBoots)

% initialize
MLE_estimates=[];
SEs=[];
pvals=[];
numStrats=length(pisAll(1,:))-2;

% step through each error rate and b/c ratio pairing
for err=unique(pisAll(:,end))'
    for bc=unique(pisAll(:,end-1))'
    
        % pick off the part of pisAll pertaining to the current condition,
        % and leave off the last 2 columns, which specify b/c and error
        % rate
        pisHere=pisAll(find(pisAll(:,end-1)==bc & pisAll(:,end)==err),1:end-2);

        % if there is data for this b/c,error pair
        if ~isempty(pisHere)
            
            % First create a vector of p variables, 1 for each strategy except the last
            ps=[];
            for i=1:(numStrats-1)
                % for each strategy i, define a new symbolic variable pi, and add it
                % to the vector ps
                eval(['syms p' num2str(i) ';']);
                eval(['ps=[ps p' num2str(i) '];']);
            end
            % add a final value which is 1-sum(other ps)
            ps=[ps 1-sum(ps)];


            % now calculate the log likelihood LL by stepping through each subject, and
            % adding the log of the p * p_i'
            LL=0;
            for i=1:length(pisHere(:,1))
                LL=LL+log(pisHere(i,:)*transpose(ps));
            end



            % Now generate a fn which takes a vector [p1 p2 .... pN-1 1-sum(ps) G] as
            % an input, and returns the LL.  This fn will be called by the optimizer
            % later. 

            % Since the optimizer in Matlab is a minimizer instead of a
            % maximizer, I multiply LL by -1 (so minizing this new value will maximize
            % the LL).
            LL=-1*(LL);

            % the fn will be called LL_fn. so first delete any old version which might
            % exist
            delete('LL_fn.m')
            % open the file
            f=fopen('LL_fn.m','w');
            % print the header
            fprintf(f,'function LL=drew_LL_cur(x)');
            fprintf(f,'\r');

            % the inputs get passed a vector x. first thing you need to do is pull out
            % each individual variable, such that p1 = x(1), p2 = x(2), etc.
            for i=1:(numStrats-1)
            fprintf(f,['p' num2str(i) '=x(' num2str(i) ');']);
            fprintf(f,'\r');
            end
            % and then G = x(end)
            fprintf(f,'\r');
            fprintf(f,['G=x(' num2str(i+1) ');']);
            fprintf(f,'\r');

            % write out the LL fn
            fprintf(f,'LL=');
            fprintf(f,'%s',char(LL));
            fprintf(f,';');

            % put in a check that prevents the optimizer from getting stuck on NaN or
            % imaginary LLs - if LL is NaN or not real, I just assign it an arbitrarily
            % high value, so it steers the optimizer away
            fprintf(f,'\r');
            fprintf(f,'if ~isreal(LL) | isnan(LL)');
            fprintf(f,'\r');
            fprintf(f,'LL=1e6;');
            fprintf(f,'\r');
            fprintf(f,'end');
            fprintf(f,'\r');
            % close the file
            fclose(f);

            % refresh matlab's file list so it sees the new file
            rehash


            %%%%%%%%%%%%%%%%%%%%%
            % Now do the actual optimization
            numStratsToCheck=numStrats;

            % specifiy constraints on the values of the variables. I constrain each p
            % to be between 0 and 1, and G to be between 0 and 10
            constraintA=[eye(numStratsToCheck); -1*eye(numStratsToCheck); [-1*ones(1,numStratsToCheck-1) 0]; [ones(1,numStratsToCheck-1) 0] ];
            constraintB=[ones(numStratsToCheck-1,1); 10; zeros(numStratsToCheck,1); 0; 1];



            % to make sure im getting the real minimum, I run the optimizer from nRuns
            % different initial conditions, and record each run in resultsHere
            nRuns=10;
            resultsHere=[];

            % set the optimizer settings so it doesnt hang and run forever
            options = optimset('MaxSQPIter', round(numStratsToCheck),'Display', 'off');
            % turn off warning messages - the optimizer throws a lot of 0/0
            % warnings
            warning off all

            for run=1:nRuns    
                % generate a random initial condition, where sum(inits)=1
                tmp=[0; sort(rand(numStratsToCheck-1,1)); 1];
                inits=tmp(2:end)-tmp(1:end-1);

                % create the initial x vector, which is the initial condition plus a
                % random # between 0 and 1 for G
                x=[inits(1:(numStratsToCheck-1)); rand];

                % run the minimizer
                [vals, val]=fmincon(@LL_fn,x,constraintA,constraintB,[],[],[],[],[],options);

                % record the output
                resultsHere=[resultsHere [vals; real(val)/1e3]];
            end

            % find the fit with the lowest -LL, and return it as the final optimized
            % result
            bestFit=find(resultsHere(end,:)==min(resultsHere(end,:)));
            resultsHere=resultsHere(:,bestFit(1));
            % add in freq of last strategy, which is 1-(others), and remove
            % cost (last element in resultsHere)
            resultsHere=[resultsHere(1:end-2); 1-sum(resultsHere(1:end-2)); resultsHere(end-1)]
            
            % now that we have the esimates, we bootstrap standard errors
            nRuns=10;
            boots=pis_bootstrap(nBoots,pisHere,nRuns);
            SEsHere=std(boots')';

            
            % Test for whether each strategy frequency is significantly
            % different from 0
            psHere=[];
            for jidx=1:(length(resultsHere))
                psHere(jidx)=2*normcdf(-1*resultsHere(jidx),0,SEsHere(jidx));
            end
            
            % append these results onto the main results matrix
            MLE_estimates=[MLE_estimates [bc; err; resultsHere]];
            SEs=[SEs [bc; err; SEsHere]];
            pvals=[pvals [bc; err; psHere']];
        end
    end
end

% turn it from symoblic back into actual numbers
MLE_estimates=subs(MLE_estimates);
SEs=subs(SEs);
pvals=subs(pvals);

