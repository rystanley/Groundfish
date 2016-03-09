function y = bivar_ass_mc2_Jack(x,t_increment)

% Function calculates the average number of capelin 
% associated with each cod.
% The x input matrix contains 4 columns.  The first
% column must contain the decimal latitude, and the
% second column the decimal longitude.  The third 
% column contains capelin density (fish per metre squared), and the fourth
% column contains cod density (fish per metre squared).  The rows give the 
% values for each point estimate.  
% The association is calculated in two dimensions at
% distance increments of t-increment (in metres), up to the 
% maximum separation of the fartherest point estimates.
% The bivariate association is calculated for the data
% and for 99 randomisations.
% **************NOTE THAT MAX INCREMENT IS SET TO 1 as there is only one spatial scale
% (t_increment) that is of inerest. **********

[m,n] = size(x);
dist = zeros(m,m);
latm = zeros (m,1);
longm = zeros (m,1);

latm(:) = 111060.*(x(:,1));
longm(:) = 111060.*(x(:,2)).*cos((x(:,1)).*0.01745);

% This converts the decimal latitude and longitude to their 
% value in metres and put these into separate column matrices.

for i = 1:m
   for j = 1:m
      dist(i,j) = sqrt(((latm(i)-latm(j)).*(latm(i)-latm(j))) + ((longm(i)-longm(j)).*(longm(i)-longm(j))));
   end
end

% Calculates the distance in metres between acoustic point 
% estimates.

max_dist = max(max(dist));
max_increment=1; %% ************this is set to one because we are only intrested in the extra/potential contact of a species to prey at the specified scale (t_increment)**************
%max_increment = ceil((max_dist./t_increment));

% Calculates the maximum distance increment to assess bivariate
% associations.

cap_dens = zeros(m,1);
cap_dens = x(:,3);
cod_dens = zeros (m,1);
cod_dens = x(:,4);

% Puts capelin and cod densities into their own column matrices.

out1 = zeros(100,max_increment);
y = zeros (9,max_increment);
rnd_data = zeros(m,1);

out1(1,:) = bivar_ass2(dist,cap_dens,cod_dens,t_increment,max_increment);

% Performs the analysis on the data.

for i = 2:100
   rnd_data = randomize_val(cod_dens);
   out1(i,:) = bivar_ass2(dist,cap_dens,rnd_data,t_increment,max_increment);
end

% Performs the analysis on 99 randomisations of the cod data.

out3 = zeros(100,max_increment);
out3 = sort(out1);

% Arranges the data and randomisations in ascending order

y(1,:) = 1:max_increment;
y(1,:) = y(1,:).*(t_increment);
y(2,:) = out1(1,:);
y(2,:) = y(2,:).*(3.14159.*(y(1,:).*y(1,:)));
y(3,:) = out3(5,:);
y(3,:) = y(3,:).*(3.14159.*(y(1,:).*y(1,:)));
y(4,:) = out3(95,:);
y(4,:) = y(4,:).*(3.14159.*(y(1,:).*y(1,:)));
y(5,:) = mean(out1(2:100,:));
y(5,:) = y(5,:).*(3.14159.*(y(1,:).*y(1,:)));
y(6,:) = randomize_sig1(out1,out1(1,:));
y(7,:) = y(2,:)-y(5,:);
y(8,:) = y(3,:)-y(5,:);
y(9,:) = y(4,:)-y(5,:);

y = y';

% Outputs the results of the randomisations with 95% 
% confidence intervals.
% First column is the spatial scale, radius t.  Second column
% is the observed potential contact with capelin PC(t).  Third and fourth columns 
% are the 90% limit values of PC(t) from a random arrangement PC(t)0.05 and
% PC(t)0.95.  Fifth column is the average potential contact from a random
% arrangement PC(t)ran. Sixth column is the significance (p) value.  This is the
% number of randomisations in which potential contact was higher than that observed.
% Seventh column is the extra contact with capelin XC(t).  Eighth and ninth columns 
% are the 5% and 95% confidence intervals of XC(t).