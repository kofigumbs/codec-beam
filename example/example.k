def pi()
  3.14;

def e_with_args(x y)
  2.72;

def identity(x)
  x;

def always(x y)
  identity(x);

def add_numbers()
  5 + 3;

def foo(a b)
  a*a + 2*a*b + b*b;

def bar(a)
  foo(a, 4.0) + bar(31337);
