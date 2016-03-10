%% This code will conduct the bivariate analysis on Prey and Focal species  on dataframes created in R
%tic % start timer
%cd('C:\Users\rystanley\Documents\GitHub\Groundfish\Data\Crab\'); % for surface
cd('C:\Users\RyanStanley\Documents\GitHub\Groundfish\Data\Crab\'); % for samsung
CapDir=cd;
dirinfo=dir();
dirinfo(~[dirinfo.isdir])=[];%obtain the names of all the subfolders
dirinfo=dirinfo(3:numel(dirinfo));%this removes the '.' and '..' which are attributes of the first two entries found by 'dir'
first='.\'; % This is for creating the sub directories and the current directory command handle

for k=1:length(dirinfo)
cd(CapDir)%this resets the current directory to the root Zone folder
thisdir=dirinfo(k).name;%Sets "thisdir" to the name of the next folder in sequence
a=[first thisdir];%this compiles th enext folder in sequence and adds ./ to the front so the current directory can be changed to the new folder
cd(a);
Species='Crab';
    FileList = dir('*.dat');
            for i=1:length(FileList)
               tic % start timer to see how long each loop takes
                data=load(FileList(i).name);
                output = bivar_ass_mc2(data,1000);
                year=FileList(i).name; % extract the year from the dataframe
                year=flipdim(year,2);
                year=flipdim(year(5:8),2);
				nafo=dirinfo(i).name;
				fileCSV=['Cod' '_' Species '_' nafo '_' year '_'  'BivariateOutput.dat'] % taking this off will print the name so taht the timer can be associated with a file interval in the data
                csvwrite(fileCSV,output) % save the data in csv format (.dat) file which can be opened in R
               
				% this code will jacknife the data to create parameter estimates and error
                        % for PC_t and XC_t at give spatial scales 10km, 20km, 50km, 60km, 100km
                        % and 150km
                       summary=zeros(1000,6,3);
                                                                      
                       for q=1:1000
                       dataJ=data; 
                         % using the formula a + (b-1)*rand(n,1) we create
                         % create a random distribution of rows that will
                         % be used to calculate a bootstrapped distribution
                         % of stations and then the calcuations will be
                         % used                   
                       randrow= round(1 + (length(data)-1).*rand(round(length(data)),1));
                       dataJ=dataJ(randrow,:);%delete the row for randomly jacknifed data 
                         % I ammended the code so that each jacknifed run
                         % does not require 1 km spacing and all
                         % iterations. Instead the code uses the spaceing
                         % required for the estimation in meters (i.e.
                         % 10000 = 10km). And forcres the code to just look
                         % for the first loop. 
                       c=bivar_ass_mc2_Jack(dataJ,10000);% Run the bivariate assignment on jacknifed data
                       d=bivar_ass_mc2_Jack(dataJ,20000);
                       e=bivar_ass_mc2_Jack(dataJ,50000);
                       f=bivar_ass_mc2_Jack(dataJ,60000);
                       g=bivar_ass_mc2_Jack(dataJ,100000);
                       h=bivar_ass_mc2_Jack(dataJ,150000);
                       allJack=[c;d;e;f;g;h]; %Stack them all on top of eachother
                       allJack=allJack(:,[1,2,7]); % Keep only the scale (specified by t_increment in bivar_ass_mc2_Jack.m) PC_T and XC_T
                       summary(q,:,:)=allJack; 
					   clear c d e f g h allJack dataJ randrow						 
                       end
                    summary=reshape(summary,6*1000,3);
                    SeName=['Cod' '_' Species '_' nafo '_' year '_' 'BootsErrorOutput.dat']
                    csvwrite(SeName,summary)                            
                    toc % end timer and print time in seconds
                clear year fileCSV output data
            end
    end

clear FileList a Spname Species dinfo 




%toc % end timer