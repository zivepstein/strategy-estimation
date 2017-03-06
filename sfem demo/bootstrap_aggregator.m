clear all

thisDir=['C:\Users\Antonio\Dropbox\LM\sfem demo\'];
files=dir([thisDir 'drew2*.mat']);

bs=1;

%{
bs=[159 409];
meansHere=[         0    0.0639
    0.0255         0
         0         0
    0.0261         0
         0         0
         0         0
         0         0
         0         0
         0    0.1097
    0.2483    0.1266
    0.0590    0.0198
    0.2890    0.3408
    0.0555         0
    0.1808    0.1886
    0.0280    0.1108
    0.0513    0.0386
    0.0365    0.0011
    0.4331    0.5121
];

%}



rAll=cell(length(bs),1);
fileNum=0;

for f=1:length(files)
    fileNum=fileNum+1;
    load([thisDir files(f).name]);
b=1;
    if ~isnan(mean(mean(results)))
        if ismember(b,bs)
            tmp=rAll{find(b==bs)};
            tmp=[tmp results];
            rAll{find(b==bs)}=tmp;
        end
    end
end

numStrats=length(tmp(:,1))
%{
meansHereC=[];
for i=1:length(bs)
meansHereC=[ meansHereC; mean(rAll{i}') ];
end
meansHereC=meansHereC'
%}

SEsHere=[];
for i=1:length(bs)
SEsHere=[SEsHere; std(rAll{i}') ];
end
SEsHere=SEsHere'

%{
NsHere=[];
for i=1:length(bs)
NsHere=[NsHere length(rAll{i}') ];
end
NsHere



%%%%%%%%%%%
% Hypothesis testing using normality assumption
'Different from 0?'
for i=1:length(meansHere(:,1))
    for j=1:length(meansHere(1,:))
        p0Here(i,j)=2*normcdf(-1*meansHere(i,j),0,SEsHere(i,j));
    end
end
p0Here
%}
