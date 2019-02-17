load("./make2d.rb")
load("~/info_science/8th/oo_line.rb")

include(Math)

def max(x,y)
  if x < y
    y
  else
    x
  end
end

def q(x,y,z)
  if x==y
    3
  else
    -2
  end
end

def align_sub(s,t,i,j)
  if i == 0 || j == 0
    i*g() + j*g()
  else
    v0 = align_sub(s,t,i,  j-1) + g()
    v1 = align_sub(s,t,i-1,j-1) + q(s[i-1],t[j-1])
    v2 = align_sub(s,t,i-1,j)   + g()
    max(v0,max(v1,v2))
  end
end

def align_rec(s,t)
  align_sub(s,t,s.length(),t.length())
end

def align_rec_cal_rec(m,n)
  if m == 0 || n == 0
    3
  else
    align_rec_cal(m,n-1) + align_rec_cal(m-1,n-1) + align_rec_cal(m-1,n) + 2
  end
end

def align_rec_cal_loop(m,n)
  a = make2d(m+1,n+1)
  for i in 0..m
    a[i][0] = 1
  end
  for j in 0..n
    a[0][j] = 1
  end
  for i in 1..m
    for j in 1..n
      a[i][j] = a[i][j-1] + a[i-1][j] + a[i-1][j-1] + 1
    end
  end
  a
end

def align_rec_cal(n)
  ff = open("align_rec_cal.dat","w")
  a = align_rec_cal_loop(n,n)
  step = 50
  m = n/step
  for i in 0..step
    l = m*i
    ff.puts sprintf("%i\t%.15e\n", l, a[l][l])
  end
  ff.close
  p = Point.new(m, a[m][m])
  q = Point.new(m*step, a[m*step][m*step])
  line = Line.new(p,q)
  line.base_semilog
end

def align_rec_cal_randomlength(n)
  ff = open("align_rec_cal_randomlength.dat","w")
  a = align_rec_cal_loop(n,n)
  for i in 0..200
    k = rand(n)
    l = rand(n)
    m = sqrt(k*l)
    ff.puts sprintf("%i\t%i\t%f\t%e\n",k,l,m,a[k][l])
  end
  ff.close
end
