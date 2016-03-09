function y = randomize_val(x)

% This function randomly redistributes densities in
% a column matrix.

[m,n] = size(x);

y = zeros(m,1);

a = zeros(1,m);
a = randperm(m);
b = a';

for i = 1:m
   y(i) = x(b(i));
end
