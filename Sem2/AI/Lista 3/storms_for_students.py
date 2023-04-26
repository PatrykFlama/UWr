def B(i,j):
    return 'B_%d_%d' % (i,j)

def domains(Vs):
    return [q + ' in 0..1' for q in Vs]

#* ---- generate arcs ----
# ---- block neighbours rules ----
def vertical(R, C):
    constraint = []
    for i in range(1, R - 1):
        for j in range(C):
            constraint.append(B(i - 1, j) + " + 2 * " + B(i, j) + " + 3 * " + B(i + 1, j) + " #\= 2")
    return constraint

def horizontal(R, C):
    constraint = []
    for i in range(R):
        for j in range(1, C - 1):
            constraint.append(B(i, j - 1) + " + 2 * " + B(i, j) + " + 3 * " + B(i, j + 1) + " #\= 2")
    return constraint

def square(R, C):
    constraint = []
    values = [6, 14, 9, 13, 11, 7]      # forbidden formations represented by values
    for i in range(R - 1):
        for j in range(C - 1):
            for value in values:
                # we multiply cells by 2^k to get a unique value for each combination of cells
                constraint.append(B(i, j) + " + 2 * " + B(i, j + 1) + " + 4 * " + B(i + 1, j) + " + 8 * " + B(i + 1, j + 1) + " #\= " + str(value))
    return constraint

# ---- game rules ----
def vertical_sums(R, C):
    constraint = []
    for j in range(C):
        s = ""
        for i in range(R):
            s += B(i, j)
            if i != R - 1:
                s += " + "
        s += " #= " + str(cols[j])
        constraint.append(s)
    return constraint

def horizontal_sums(R, C):
    constraint = []
    for i in range(R):
        s = ""
        for j in range(C):
            s += B(i, j)
            if j != C - 1:
                s += " + "
        s += " #= " + str(rows[i])
        constraint.append(s)
    return constraint

#* ---- assign values to cells ----
def add_assigments(assigments):
    constraint = []
    for i, j, val in assigments:
        constraint.append( '%s #= %d' % (B(i,j), val) )
    return constraint

#* ---- writing ----
def write(s):
    output.write(s)
def writeln(s):
    output.write(s + '\n')

def print_constraints(Cs, indent, d):
    position = indent
    write (indent * ' ')
    for c in Cs:
        write (c + ', ')
        position += len(c)
        if position > d:
            position = indent
            writeln ('')
            write (indent * ' ')

#* ---- main ----
def storms(rows, cols, assigments):
    writeln(':- use_module(library(clpfd)).')

    R = len(rows)
    C = len(cols)

    # variables are our cells
    variables = [B(i,j) for i in range(R) for j in range(C)]

    bs = domains(variables) + vertical(R, C) + horizontal(R, C) + square(R, C) + vertical_sums(R, C) + horizontal_sums(R, C) + add_assigments(assigments)

    writeln('solve([' + ', '.join(variables) + ']) :- ')

    print_constraints(bs, 4, 70)
    
    writeln('    labeling([ff], [' +  ', '.join(variables) + ']).' )
    writeln('')
    writeln(":- tell('prolog_result.txt'), solve(X), write(X), nl, told.")

#* ---- run ----
txt = open('zad_input.txt').readlines()
output = open('zad_output.txt', 'w')

rows = (list(map(int, txt[0].split())))
cols = (list(map(int, txt[1].split())))
triples = []

for i in range(2, len(txt)):
    if txt[i].strip():
        triples.append(list(map(int, txt[i].split())))

# --debug--
# print(rows)
# print(cols)
# print(triples)

storms(rows, cols, triples)            
        

