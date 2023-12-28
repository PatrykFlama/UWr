from matplotlib import pyplot as plt
import numpy as np

# plt.gca().set_aspect('equal', adjustable='box')

file_data = []
with open("punkty.csv", "r") as file:
    for line in file:
        x, y = line.strip().replace(' ', '').split(',')
        file_data.append((float(x), float(y)))

def f(t):
    return (t-1.2)*(t+4.7)*(t-2.3)

class NIFS3:
    def __init__(self, x0=[], y0=[]):
        self.x0 = x0
        self.y0 = y0
        self.n = 0
        self.t = []
        self.Mx = []
        self.My = []
        self.nifs3_update()

    def get_nifs3(self, step = 0.001):
        res_x = []
        res_y = []
        T = self.t[0]
        while T <= self.t[-1]:
            res_x.append(self.calc_nifs3(T, self.x0, self.Mx))
            res_y.append(self.calc_nifs3(T, self.y0, self.My))
            T += step
        return res_x, res_y

    def nifs3_update(self):
        self.n = len(self.x0)-1
        
        self.t = []
        for i in range(len(self.x0)):
            self.t.append(i/self.n)

        self.Mx = self.calc_M(self.x0)
        self.My = self.calc_M(self.y0)
    
    def calc_M(self, x):
        q = [0]
        p = [0]
        u = [0]

        k = 1
        while k <= self.n-1:
            lk = self.calc_lam(k)
            p.append(lk * q[k-1] + 2)
            q.append((lk-1)/p[k])
            u.append((self.dk(k, x) - lk * u[k-1])/p[k])
            k += 1
        
        M = [0] * (self.n+1)
        M[self.n] = 0
        M[self.n-1] = u[self.n-1]
        k = self.n-2
        while k >= 0:
            M[k] = u[k] + q[k] * M[k+1]
            k -= 1
        
        return M
    
    def calc_lam(self, k):
        return (self.t[k] - self.t[k-1])/(self.t[k+1] - self.t[k-1])
    
    def dk(self, k, x):
        t1 = (x[k+1] - x[k])/(self.t[k+1] - self.t[k])
        t2 = (x[k] - x[k-1])/(self.t[k] - self.t[k-1])
        return 6 * (t1 - t2)/(self.t[k+1] - self.t[k-1])
    
    def calc_nifs3(self, X, x0, M):
        k = 1
        while self.t[k] < X: k += 1

        return (1/(self.t[k]-self.t[k-1])) \
            * (M[k-1]*pow(self.t[k] - X, 3)/6.
            +  M[k] * pow(X - self.t[k-1], 3)/6. 
            + (x0[k-1] - M[k-1]*pow(self.t[k]-self.t[k-1], 2)/6.)*(self.t[k]-X)
            + (x0[k] - M[k]*pow(self.t[k]-self.t[k-1], 2)/6.)*(X-self.t[k-1]))
    
# least squares function approximation
class LSF:
    def __init__(self, x0=[], y0=[], pow = 15):
        self.X = x0
        self.Y = y0
        self.pow = pow

        self.c = []
        self.d = []
        self.a = []

        self.update()

    def calc(self, X):      # clenshaws algorithm
        Qk2 = 1
        Qk1 = (X - self.c[0]) * Qk2
        res = self.a[0] * Qk2 + self.a[1] * Qk1

        for k in range(2, self.pow+1):
            Qk2, Qk1 = Qk1, (X - self.c[k]) * Qk1 - self.d[k] * Qk2
            res += self.a[k] * Qk1
        return res

    # info about notation:
    # E stands for sum, Pk1/Pk2 stands for polynomial k-1/k-2, sq - squared    
    def calc_ck(self, Pk1_sq, EPk1sq):
        EPsq = ExPsq = 0
        for x, psq in zip(self.X, Pk1_sq):
            ExPsq += x * psq
        return ExPsq/EPk1sq

    def calc_dk(self, EPk1_sq, EPk2_sq):
        return EPk1_sq/EPk2_sq

    def calc_ak(self, Pk, EPk_sq):
        EfPk = 0
        for y, p in zip(self.Y, Pk):
            EfPk += y * p
        return EfPk/EPk_sq

    def update(self):
        self.Pk2 = [1]*len(self.X)
        self.EPk1_sq = self.EPk2_sq = len(self.X)
        ck1 = self.calc_ck(self.Pk2, self.EPk2_sq)
        self.Pk1 = [x-ck1 for x in self.X]
        self.EPk_sq = sum([x*x for x in self.Pk1])
        self.c.append(0)
        self.c.append(ck1)
        self.d.append(0)
        self.d.append(0)
        self.a.append(self.calc_ak(self.Pk2, self.EPk2_sq))
        self.a.append(self.calc_ak(self.Pk1, self.EPk_sq))

        for k in range(2, self.pow+1):
            self.EPk2_sq, self.EPk1_sq = self.EPk1_sq, self.EPk_sq
            ck = self.calc_ck(self.Pk1, self.EPk1_sq)
            dk = self.calc_dk(self.EPk1_sq, self.EPk2_sq)
            self.Pk2, self.Pk1 = self.Pk1, [(x-ck)*xPk1 - dk*xPk2 for x, xPk1, xPk2 in zip(self.X, self.Pk1, self.Pk2)]
            self.c.append(ck)
            self.d.append(dk)
            self.EPk_sq = sum([x*x for x in self.Pk1])
            self.a.append(self.calc_ak(self.Pk1, self.EPk_sq))

# ------------------------------
    
def plot_f(from_x=-5, to=5, step=0.01):
    x = np.arange(from_x, to, step)
    y = f(x)
    plt.plot(x, y, 'g')

def plot_data_points():
    x = [x for x, y in file_data]
    y = [y for x, y in file_data]
    plt.plot(x, y, 'ro')

def plot_nifs3(step=0.001):
    sorted_data = sorted(file_data, key=lambda x: x[0])
    nifs3 = NIFS3([x for x, y in sorted_data], [y for x, y in sorted_data])
    x, y = nifs3.get_nifs3(step)
    plt.plot(x, y, 'b')

def plot_lsf(pow = 15, from_x=-5, to=5, step=0.01):
    lsf = LSF([x for x, y in file_data], [y for x, y in file_data], pow)
    x = np.arange(from_x, to, step)
    plt.plot(x, [lsf.calc(X) for X in x], 'y')

deg = "1"
while deg.isnumeric():
    plt.cla()
    # plot_f()
    plot_data_points()
    # plot_nifs3()
    plot_lsf(int(deg), -5, 5)
    # plt.legend(['f(x)', 'data points', 'NIFS3'])
    plt.grid(True)
    plt.show(block=False)

    deg = input("Enter degree of polynomial: ")


