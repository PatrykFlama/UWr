class Formula:
    def calculate(self, values):
        pass

    def __add__(self, other):
        return And(self, other)
    
    def __mul__(self, other):
        return Or(self, other)
    
    def __str__(self):
        return "Formula"
    
    def is_tautology(self):
        return False

class Constant(Formula):
    def __init__(self, value):
        self.value = value

    def calculate(self, values):
        return self.value

    def __str__(self):
        return str(self.value)

    def is_tautology(self):
        return self.value

class Variable(Formula):
    def __init__(self, name):
        self.name = name

    def calculate(self, values):
        return values[self.name]
    
    def __str__(self):
        return self.name
    
    def is_tautology(self):
        False
    
class Not(Formula):
    def __init__(self, V):
        self.V = V
    
    def calculate(self, values):
        return not self.V.calculate(values)
    
    def __str__(self):
        return "(!" + self.V + ")"
    
    def is_tautology(self):
        pass    # TODO

class And(Formula):
    def __init__(self, L, R):
        self.L = L
        self.R = R
    
    def calculate(self, values):
        return self.L.calculate(values) and self.R.calculate(values)
    
    def __str__(self):
        return "(" + self.L + " & "+ self.R + ")"
    
    def is_tautology(self):
        if self.L.is_tautology() and self.R.is_tautology():
            return True

class Or:
    def __init__(self, L, R):
        self.L = L
        self.R = R
    
    def calculate(self, values):
        return self.L.calculate(values) or self.R.calculate(values)
    
    def __str__(self):
        return "(" + self.L + " | "+ self.R + ")"
    
    def is_tautology(self):
        pass   # TODO
