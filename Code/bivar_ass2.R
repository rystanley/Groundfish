function y = bivar_ass2(dist,cap_dens,x,t_increment,max_increment)

# This function calculates the average density of capelin
# around each cod.
# The function should be run from within function y =
  # bivar_ass_mc(), which calculates the necessary variables
# for this function from an input data matrix and increment
# parameter. x is a column matrix containing cod density
# values (the data or a randomization of the data).

y = zeros(1,max_increment);

[m,n] = size(x);

out = zeros(m,max_increment);
outcount = zeros(m,max_increment);

for t = 1:max_increment
for i = 1:m
if x(i) > 0
for j = 1:m
if dist(i,j) < (t.*t_increment)
out(i,t) = out(i,t)+cap_dens(j);
outcount(i,t) = outcount(i,t)+1;
end
end
out(i,t) = out(i,t)./outcount(i,t);
out(i,t) = out(i,t).*x(i);
end
end
end

# Calculates the average capelin density within distance t
# of each cod.

cod_sum = 0;

cod_sum = sum(x(:));

# Determines the total density of cod 

for i = 1:max_increment
y(i) = (sum(out(:,i)))./cod_sum;
end

# Calculates the average capelin density within distance t
# averaged for all cod = E[d(t)].
