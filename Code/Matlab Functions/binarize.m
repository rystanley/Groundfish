function y = binarize(x,threshold)

[m,n] = size(x);

y = zeros(m,n);

for i = 1:m
   for j = 1:n
      if(x(i,j) >= threshold)
         y(i,j) = 1;
      end
   end
end
