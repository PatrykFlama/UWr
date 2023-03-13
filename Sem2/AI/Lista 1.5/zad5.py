# amount of different configs from high to low strength, precalculated
Blotter = [20, 288, 1728, 484, 5100, 16128, 36288, 193536, 123420]
All_Blotter = 376992

Figurant =[0, 48, 288, 0, 0, 768, 1728, 1536, 0]
All_Figurant = 4368

# amt of conf with higher or same power as blotter
figurant_power = 0
res = 0

for i in range(len(Blotter)):
    figurant_power += Figurant[i]
    res += Blotter[i] * (All_Figurant - figurant_power)

combinations = All_Blotter * All_Figurant
print(res/combinations)