import scipy as sci
Sol = sci.integrate.quad(x**3/(exp(x)-1),0,inf)
print(Sol)