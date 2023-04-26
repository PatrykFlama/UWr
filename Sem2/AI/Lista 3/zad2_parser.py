# generates domains and runs the solver code (which is in c++ for _SPEEEED_)
import itertools
import subprocess
    
def read_input():
    with open("zad_input.txt", "r") as input:
        s = input.readline().split()
        R = (int)(s[0])
        C = (int)(s[1])
        row_vals = []
        col_vals = []
        for i in range(R): row_vals.append(list(map(int, input.readline().strip('\n').split())))
        for i in range(C): col_vals.append(list(map(int, input.readline().strip('\n').split())))
        return R, C, row_vals, col_vals

def get_domain(n, m, vals):
    domain = []
    for i in range(0, n):
        block_num = len(vals[i])
        all_combs = [list(x) for x in itertools.combinations(range(m), block_num)]
        cnt_domain = []
        for comb in all_combs:
            flag = True
            for j in range(0, block_num - 1):
                cnt_pos = comb[j]
                cnt_len = vals[i][j]
                nxt_pos = comb[j + 1]
                if cnt_pos + cnt_len >= nxt_pos:
                    flag = False
                    break
            if comb[-1] + vals[i][-1] - 1 >= m:
                flag = False
            if flag == True:
                cnt_domain.append(comb)
        domain.append(cnt_domain)
    return domain

def build_domain(R, C, row_vals, col_vals):
    row_domain = get_domain(R, C, row_vals)                
    col_domain = get_domain(C, R, col_vals)
    return row_domain, col_domain

if __name__ == '__main__':
    R, C, row_vals, col_vals = read_input()
    row_domain, col_domain = build_domain(R, C, row_vals, col_vals)
    with open("zad2_cpp_input.txt", "w") as output:
        output.write(str(R))
        output.write(' ')
        output.write(str(C))
        output.write('\n')
        for row in row_vals:
            output.write(str(len(row)))
            output.write(' ')
            for block in row:
                output.write(str(block))
                output.write(' ')
            output.write('\n')
        for col in col_vals:
            output.write(str(len(col)))
            output.write(' ')
            for block in col:
                output.write(str(block))
                output.write(' ')
            output.write('\n')
        for row in row_domain:
            output.write(str(len(row))) 
            output.write('\n')
            for domain in row:
                output.write(str(len(domain)))
                output.write(' ')
                for pos in domain:
                    output.write(str(pos))
                    output.write(' ')
                output.write('\n')
        for col in col_domain:
            output.write(str(len(col))) 
            output.write('\n')
            for domain in col:
                output.write(str(len(domain)))
                output.write(' ')
                for pos in domain:
                    output.write(str(pos))
                    output.write(' ')
                output.write('\n')

    subprocess.run(["zad2.exe"])