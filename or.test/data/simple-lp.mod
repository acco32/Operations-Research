var x1;
var x2;
maximize obj: 0.6 * x1 + 0.5 * x2;
s.t. c1: x1 + 2 * x2 <= 1;
s.t. c2: 3 * x1 + x2 <= 2;
solve;
display x1, x2;
end;
