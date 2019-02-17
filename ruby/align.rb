def make3d(l,m,n)
  a = Array.new(l)
  for i in 0..(l-1)
    a[i] = Array.new(m)
    for j in 0..(m-1)
      a[i][j] = Array.new(n){0}
    end
  end
end

def g()
  -2
end

def p(x,y)
  if x == y
    0.5
  else
    -1
  end
end

def q(x,y,z)
  if x == y && y == z
    5
  else
    if x == y || y == z || z == x
      1
    else
      -1
    end
  end
end

def align(r,s,t)
  l = r.length()
  m = s.length()
  n = t.length()
  a = make3d(l+1,m+1,n+1)
  a[0][0][0] = 0
  for k in 1..n
    a[0][0][k] = a[0][0][k-1] + g()
  end
  for j in 1..m
    a[0][j][0] = a[0][j-1][0] + g()
  end
  for i in 1..l
    a[i][0][0] = a[i-1][0][0] + g()
  end
  for i in 1..l
    for j in 1..m
      for k in 1..n
        v0 = a[i-1][j][k] + g()
        v1 = a[i][j-1][k] + g()
        v2 = a[i][j][k-1] + g()
        v3 = a[i-1][j-1][k] + p(r[i-1],s[j-1])
        v4 = a[i-1][j][k-1] + p(r[i-1],t[k-1])
        v5 = a[i][j-1][k-1] + p(s[j-1],t[k-1])
        v6 = a[i-1][j-1][k-1] + q(r[i-1],s[j-1],t[k-1])
        a[i][j][k] = [v0,v1,v2,v3,v4,v5,v6].max
      end
    end
  end
  a
end
