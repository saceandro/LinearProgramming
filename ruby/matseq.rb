def f(l,m,n)
  l*n*(2*m-1)
end

def make2d(n)
  a = Array.new(n)
  for i in 0..n-1
    a[i] = Array.new(n){0}
  end
  a
end

# dは、d_0,...,d_nの配列
def matseq(d)
  n = d.length()
  m = make2d(n)
  for l in 1..n-1
    for j in 2..n
      for i in 1..(n-l)
        min = m[i][i] + m[i+1][j] + f(d[i-1],d[i],d[j])
        for k in i+1..j-1
          v = m[i][k] + m[k+1][j] + f(d[i-1],d[k],d[j])
          if v < min
            v = min
          end
        end
        m[i][j] = min
      end
    end
  end
  m[1][n]
end
          
        
  
