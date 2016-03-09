function y = randomize_indiv_dens(x)

% Function randomizes density increments to the second decimal place.  
% If data are more or less precise, then this routine should be altered to
% randomize to level of precision.

[m,n] = size(x);

y = zeros(m,n);

num = sum(sum(x));
num = 100.*num;
num = abs(num);

for i = 1:num
   p = floor(rand*m)+1;
   q = floor(rand*n)+1;
   y(p,q) = y(p,q)+1;
end

y = y./100;
