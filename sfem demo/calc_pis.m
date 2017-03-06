function pis=calc_pis(posStrats,startFrom,endAt,data,bc,error,numSubjects)
% calc_pis returns a [i x s] matrix of probabilities, corresponding to the 
% likelihood that each of i subjects with histories in 'data' uses each 
% of the s strategies listed in 'posStrats'.
%
% INPUTS:
% posStrats = list of strategies to explore (using the key given below)
% startFrom = first interaction in 'data' to consider
% endAt = last ineraction in 'data' to consider
% data = data file listing all decisions
% bc = the b/c ratio for the games listed in 'data'
% error = error rate for games listed in 'data'
% numSubjects = number of subjects in 'data
%
% OUTPUT
% pis = i x s matrix of pi(s) values (likelihood of subject i's history if
%       she was playing strategy s)
% 
%%%%%%%%%%%%%%%%%%%%%%%
% KEY FOR 'posStrats' 
%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%


syms G 

% will hold likelihood for subject i of strategy s - here we initialize it
% to be all ones (and multiple and divide by G so that matlab considers it
% a matrix of symbolic values)
pis=G*ones(numSubjects,length(posStrats(:,1))+2)/G;

% loop through each subject
for s=1:numSubjects
    [s numSubjects]

    % loop through each interaction, starting from startFrom and ending at endAt
    for m=startFrom:endAt
            
          
            % a dummy variable to keep track of the
            % state of the T2 trigger.
            % 0 = happy
            % 1 = 1st punishment is this round
            % 2 = 2nd punishment is this round 
            T2trigger=0;
            
            % dummies to keep track of whether each Grim strategy has
            % triggered
            G_last2_trig=0;
            G_last3_trig=0;
            
            
            % loop through each period of interacion m 
            for p=1:max(data(find((data(:,1)==m & data(:,3)==s)),2))
                
                %%%%%%%%%%%%%%%%%%%%
                % ESTABLISH HISTORY BASED ON DATA
                %%%%%%%%%%%%%%%%%%%%
                % 'data' has one row per decision, with the following
                % relevant columns:
                % 1 = interaction
                % 2 = period
                % 3 = subject
                % 4 = actual move (1=C, 2=D)
                % 5 = intended move (1=C, 2=D)
                % 6 = other player's actual move (1=C, 2=D)                
                %%%%%%%%%%%%%%%%
                
                
                % MOVES LAST ROUND
                % if p=1, assume everyone played C last move
                if p==1
                    you_p_1=1;
                    other_p_1=1;
                else
                    % your move in p-1 (1=C,2=D)
                    you_p_1=length(find((data(:,1)==m & data(:,2)==(p-1) & data(:,3)==s & data(:,4)==2)))+1;
                    % other's move in p-1 (1=C,2=D)
                    other_p_1=length(find((data(:,1)==m & data(:,2)==(p-1) & data(:,3)==s & data(:,6)==2)))+1;
                end

                % MOVES 2 ROUNDS AGO
                % if p=1 or 2, assume evryone placed C 2 moves ago
                if p<3
                    you_p_2=1;
                    other_p_2=1;
                else
                    % your move in p-2 (1=C,2=D)
                    you_p_2=length(find((data(:,1)==m & data(:,2)==(p-2) & data(:,3)==s & data(:,4)==2)))+1;
                    % other's move in p-2 (1=C,2=D)
                    other_p_2=length(find((data(:,1)==m & data(:,2)==(p-2) & data(:,3)==s & data(:,6)==2)))+1;
                end
                
                % others move 3 rounds ago
                if p<4
                    you_p_3=1;
                    other_p_3=1;
                else
                    you_p_3=length(find((data(:,1)==m & data(:,2)==(p-3) & data(:,3)==s & data(:,4)==2)))+1;
                    other_p_3=length(find((data(:,1)==m & data(:,2)==(p-3) & data(:,3)==s & data(:,6)==2)))+1;
                end
               

               
                % UPDATE T2 trigger
                % if either player defected, and T2trigger
                % is 0, then start the T2trigger
                if (you_p_1==2 || other_p_1==2) && T2trigger==0
                    T2trigger=1;
                    % if both people played C, no
                    % problem - leave T2trigger alone
                elseif you_p_1==1 && other_p_1==1 && T2trigger==0
                    T2trigger=0;
                    % otherwise, increment T2trigger
                elseif T2trigger==1 
                    T2trigger=2;
                elseif T2trigger==2
                    T2trigger=0;
                end

                
                % UPDATE Grim triggers
                % for Grim, you just need to check if there has ever been a
                % D
                been_1_D=((length(find(data(:,1)==m & data(:,2)<p & data(:,3)==s & (data(:,4)==2 | data(:,6)==2))))>0);
                % if either player played D in the last 2 rounds, Grim2 is triggered
                if (you_p_1==2 || other_p_1==2) && (you_p_2==2 || other_p_2==2)
                    G_last2_trig=1;
                end
                % if either player played D in the last 3 rounds, Grim3 is triggered
                if (you_p_1==2 || other_p_1==2) && (you_p_2==2 || other_p_2==2) && (you_p_3==2 || other_p_3==2)
                    G_last3_trig=1;
                end
                
                


                
                
                % RECORD THE SUBJECTS REAL [INTENDED] CHOICE THIS ROUND (0=D,1=C)
                y_imr=length(find((data(:,1)==m & data(:,2)==p & data(:,3)==s & data(:,5)==1)));


                

                % now step through each strategy in posStrats, and figue
                % out what it's predicted move s_imr is given the current
                % history (-1=D, 1=C)
                for sk=1:length(posStrats(:,1))

                   % pull out the current strategy, and reshape it in a way
                   % that makes it easier to look up the move
                    stratHere=reshape(posStrats(sk,:),2,2,2);

                    % first check if the current strategy is any of the
                    % hardcoded ones; then if not, just look up the move as
                    % specified by the strategy

                    
                    if stratHere(1,1,1)==-101
                        %101 C ALLC
                        if p==1
                            s_imr=1;                            
                        else
                            s_imr=1;                            
                        end
                        
                    elseif stratHere(1,1,1)==-102
                        %102 D ALLC
                        if p==1
                            s_imr=-1;                            
                        else
                            s_imr=1;                            
                        end
                        
                    elseif stratHere(1,1,1)==-103
                        %103 C ALLD
                        if p==1
                            s_imr=1;                            
                        else
                            s_imr=-1;                            
                        end
                        
                    elseif stratHere(1,1,1)==-104
                        %104 D ALLD
                        if p==1
                            s_imr=-1;                            
                        else
                            s_imr=-1;
                        end
                    
                        
                    elseif stratHere(1,1,1)==-105
                        %105 C Grim1
                        if p==1
                            s_imr=1;                           
                        else                            
                            if been_1_D==1
                                s_imr=-1;                                
                            else
                                s_imr=1;
                            end                                
                        end    
                        
                    elseif stratHere(1,1,1)==-106
                        %106 D Grim1
                        if p==1
                            s_imr=-1;                           
                        else                            
                            if been_1_D==1
                                s_imr=-1;                                
                            else
                                s_imr=1;
                            end                                
                        end    
                        
                    elseif stratHere(1,1,1)==-107
                        %107 C TFT
                        if p==1
                            s_imr=1;                            
                        else     
                            if other_p_1==2
                                s_imr=-1;                                
                            else
                                s_imr=1;                                
                            end
                        end
                        
                    elseif stratHere(1,1,1)==-108
                        %108 D TFT
                        if p==1
                            s_imr=-1;                            
                        else     
                            if other_p_1==2
                                s_imr=-1;                                
                            else
                                s_imr=1;                                
                            end
                        end
                    
                    elseif stratHere(1,1,1)==-109
                        %109 C Grim2
                        if p==1
                            s_imr=1;                           
                        else                            
                            if G_last2_trig==1
                                s_imr=-1;                                
                            else
                                s_imr=1;
                            end                                
                        end    
                        
                    elseif stratHere(1,1,1)==-110
                        %110 D Grim2
                        if p==1
                            s_imr=-1;                           
                        else                            
                            if G_last2_trig==1
                                s_imr=-1;                                
                            else
                                s_imr=1;
                            end                                
                        end    
                    
                    elseif stratHere(1,1,1)==-111
                        %111 C 2TFT
                        if p==1
                            s_imr=1;
                        else     
                            if other_p_1==2 || other_p_2==2
                                s_imr=-1;
                            else
                                s_imr=1;
                            end
                        end         
                        
                    elseif stratHere(1,1,1)==-112
                        %112 D 2TFT
                        if p==1
                            s_imr=-1;
                        else     
                            if other_p_1==2 || other_p_2==2
                                s_imr=-1;
                            else
                                s_imr=1;
                            end
                        end           
                        
                   elseif stratHere(1,1,1)==-113
                        %113 C TF2T
                        if p==1
                            s_imr=1;
                        else     
                            if other_p_1==2 && other_p_2==2
                                s_imr=-1;
                            else
                                s_imr=1;
                            end
                        end            
                        
                   elseif stratHere(1,1,1)==-114
                        %114 D TF2T
                        if p==1
                            s_imr=-1;
                        else     
                            if other_p_1==2 && other_p_2==2
                                s_imr=-1;
                            else
                                s_imr=1;
                            end
                        end       

                    elseif stratHere(1,1,1)==-115
                        %115 C Grim3
                        if p==1
                            s_imr=1;                           
                        else                            
                            if G_last3_trig==1
                                s_imr=-1;                                
                            else
                                s_imr=1;
                            end                                
                        end    
                        
                    elseif stratHere(1,1,1)==-116
                        %116 D Grim3
                        if p==1
                            s_imr=-1;                           
                        else                            
                            if G_last3_trig==1
                                s_imr=-1;                                
                            else
                                s_imr=1;
                            end                                
                        end    
                   
                    elseif stratHere(1,1,1)==-117
                        %117 C 3TFT
                        if p==1
                            s_imr=1;
                        else     
                            if other_p_1==2 || other_p_2==2 || other_p_3==2
                                s_imr=-1;
                            else
                                s_imr=1;
                            end
                        end         
                   
                    elseif stratHere(1,1,1)==-118
                        %118 D 3TFT
                        if p==1
                            s_imr=-1;
                        else     
                            if other_p_1==2 || other_p_2==2 || other_p_3==2
                                s_imr=-1;
                            else
                                s_imr=1;
                            end
                        end      
                    
                     elseif stratHere(1,1,1)==-119
                        %119 C TF3T
                        if p==1
                            s_imr=1;
                        else     
                            if other_p_1==2 && other_p_2==2 && other_p_3==2
                                s_imr=-1;
                            else
                                s_imr=1;
                            end
                        end        
                    
                    elseif stratHere(1,1,1)==-120
                        %120 D TF3T
                        if p==1
                            s_imr=-1;
                        else     
                            if other_p_1==2 && other_p_2==2 && other_p_3==2
                                s_imr=-1;
                            else
                                s_imr=1;
                            end
                        end     
                        
                    elseif stratHere(1,1,1)==-121
                        %121 C 2TF2T
                        if p==1
                            s_imr=1;
                        else     
                            if (other_p_1==2 && other_p_2==2 && other_p_3==1) ...
                                || (other_p_1==1 && other_p_2==2 && other_p_3==2) ...
                                || (other_p_1==2 && other_p_2==2 && other_p_3==2)
                                s_imr=-1;                               
                            else
                                s_imr=1;                                
                            end
                        end
                        
                    elseif stratHere(1,1,1)==-122
                        %122 D 2TF2T
                        if p==1
                            s_imr=-1;
                        else     
                            if (other_p_1==2 && other_p_2==2 && other_p_3==1) ...
                                || (other_p_1==1 && other_p_2==2 && other_p_3==2) ...
                                || (other_p_1==2 && other_p_2==2 && other_p_3==2)
                                s_imr=-1;                               
                            else
                                s_imr=1;                                
                            end
                        end
                        
                    % otherwise, normal strategy lookup
                    else
                        s_imr=stratHere(other_p_1,you_p_1,other_p_2);
                    end

                % update pis based for subject s and strategy sk based on
                % the actual move y_imr and the predicted move s_imr
                pis(s,sk)=pis(s,sk)*...
                    ((1/(1+exp(-1*s_imr/G))) ^y_imr)...
                    *((1/(1+exp(1*s_imr/G))) ^(1-y_imr));


            end %sk (strategy)
        end %p (period)
    end %m (interaction)
    pis(:,end)=error;
    pis(:,end-1)=bc;

end %s (subject)


