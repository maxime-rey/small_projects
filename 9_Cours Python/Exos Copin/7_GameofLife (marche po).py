import random

class Life:
    def __init__(self, n=10):
        self.n = n
        self.grid=[ [ 0 for i in range(n) ] for j in range(n) ]
        for i in self.grid:
            for j in i:
                if random.uniform(0,1)<0.2:
                    self.grid[i][j] = 1
    def BecomeAlive(self):

        i
