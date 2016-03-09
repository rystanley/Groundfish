function y = n_patch(dist,x,t_increment,max_increment)

% This function calculates the average density of
% neighbours around each individual.
% The function should be run from within function y =
% cod_patch_mc(), or y = cap_patch_mc() or y = gen_patch_mc()
% which calculate the necessary variables
% for this function from an input data matrix and increment
% parameter. x is a column matrix containing density
% values (the data or a randomization of the data).

y = zeros(1,max_increment);

[m,n] = size(x);

out = zeros(m,max_increment);
outcount = zeros(m,max_increment);

for t = 1:max_increment
   for i = 1:m
      if x(i) > 0
         for j = 1:m
            if dist(i,j) < (t.*t_increment)
               out(i,t) = out(i,t)+(x(j));
               outcount(i,t) = outcount(i,t)+1;
            end
         end
         out(i,t) = out(i,t)./outcount(i,t);
         out(i,t) = out(i,t).*x(i);
      end
   end
end

% Calculates the average density of neighbours within distance t
% of each individual.

n_sum = 0;

n_sum = sum(x(:));

% Determines the number of individuals 

for i = 1:max_increment
   y(i) = (sum(out(:,i)))./n_sum;
end

% Calculates the average density of neighbours within distance t
% averaged for all individuals.





