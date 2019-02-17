load("./make1d.rb")

def make2d(m,n)
  a = Array.new(m)
  for i in 0..(m-1)
    a[i] = make1d(n)
  end
  a
end
