function y = randomize_sig1(rnd_x,x)

[m,n] = size(rnd_x);

y = zeros(1,n);

for i = 1:n
   y(i) = sum(binarize(rnd_x(:,i),x(i)))/m;
end
