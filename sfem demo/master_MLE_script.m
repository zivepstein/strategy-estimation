%%%%%%%%%%%%%%%%%
% Script Outline:
%%%%%%%%%%%%%%%%%%
% Step through each session, load the datafile, calculate the LL of each
% strategy for each subject in the session using calc_pis, append until 
% you have a matrix with one row for every subject in the entire paper 
% (across all treatments etc), and one column for each strategy.  
% Then pass this matrix to the calc_MLE function, which will return (for 
% each treatment the estimated probability of choosing each 
% strategy, along with bootstrapped standard errors and p-values for whether 
% each strategy is different from 0.
%
%%%%%%%%%%%%%%%%%%%%%%%%%
% Define set of strategies to investigate (as per the key in calc_pis)
% 28 new strategy set (all hard-coded):
posStrats=[-101*ones(1,8); ... % 
    %-102*ones(1,8); ... % 
    %-103*ones(1,8); ... % 
    -104*ones(1,8); ... % 
    -105*ones(1,8); ... % 
    %-106*ones(1,8); ... % 
    %-107*ones(1,8); ... % 
    %-108*ones(1,8); ... % 
    %-109*ones(1,8); ... % 
    %-110*ones(1,8); ... % 
    %-111*ones(1,8); ... % 
    %-112*ones(1,8); ... % 
    %-113*ones(1,8); ... % 
    %-114*ones(1,8); ... % 
    %-115*ones(1,8); ... % 
    %-116*ones(1,8); ... % 
    %-117*ones(1,8); ... % 
    %-118*ones(1,8); ... % 
    %-119*ones(1,8); ... % 
    %-120*ones(1,8); ... % 
    %-121*ones(1,8); ... % 
    %-122*ones(1,8); ... % 
    ];

%%%%%%%%%%%%%%%%%%%%%%%%
% pisAll will hold the aggregated likelihoods for all subjects
pisAll=[];
for file=1:1
    % Load the current datafile    
    if file==1
        load data/s1
        bc=4;
        error=0;    
    elseif file==2
        load data\s4
        bc=4;
        error=0;
    elseif file==3
        load data\s6
        bc=4;
        error=0; 
    elseif file==4
        load data\s8
        bc=4;
        error=0; 
    elseif file==5
        load data\s10
        bc=4;
        error=0; 
    elseif file==6
        load data\s12
        bc=4;
        error=0; 
    elseif file==7
        load data\s14
        bc=4;
        error=0;      
    end        
    numInteractions=max(data(:,1));
    numSubjects=max(data(:,3));    
    % analyze all interactions of each session
    %startFrom=22;
    %endAt=numInteractions;
    % analyze first half of the interactions of each session
    %half=ceil(numInteractions/2);   % rounds up
    %startFrom=1;
    %endAt=half;
    % analyze second half of the interactions of each session
    %startFrom=half+1;
    %endAt=numInteractions;
    % analyze first four interactions
    %startFrom=1;
    %endAt=4;
    % analyze last four interactions
    startFrom=1;
    endAt=1;   
    
    
    % calculate the likelihoods for this session
    [pis]=calc_pis(posStrats,startFrom,endAt,data,bc,error,numSubjects);    
    % append to the growing list of pis results    
    pisAll=[pisAll; pis];         
end