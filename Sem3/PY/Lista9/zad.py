# import matplotlib.pyplot as plt

# fig, ax = plt.subplots(3, 1, figsize=(8, 10), tight_layout=True)

# # single point
# ax[0].plot(105, 110, '-ro', label='line & marker - no line because only 1 point')
# ax[0].plot(200, 210, 'go', label='marker only')  # use this to plot a single point
# ax[0].plot(160, 160, label='no marker - default line - not displayed- like OP')
# ax[0].set(title='Markers - 1 point')
# ax[0].legend()

# # two points
# ax[1].plot([105, 110], [200, 210], '-ro', label='line & marker')
# ax[1].plot([105, 110], [195, 205], 'go', label='marker only')
# ax[1].plot([105, 110], [190, 200], label='no marker - default line')
# ax[1].set(title='Line & Markers - 2 points')
# ax[1].legend()

# # scatter plot
# ax[2].scatter(x=105, y=110, c='r', label='One Point')  # use this to plot a single point
# ax[2].scatter(x=[80, 85, 90], y=[85, 90, 95], c='g', label='Multiple Points')
# ax[2].set(title='Single or Multiple Points with using .scatter')
# ax[2].legend()

# plt.show()


import matplotlib.pyplot as plt
import csv

years = [2021, 2022]
months = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]
filename_bitcoin = ["BTC-USD 2021.csv", "BTC-USD 2022.csv"]
filename_dogecoin = ["DOGE-USD 2021.csv", "DOGE-USD 2022.csv"]

bitcoin = [[], []]
dogecoin = [[], []]

def readData(fileName, data):
    with open(fileName) as csvfile:
        csv_reader = csv.reader(csvfile)
        for row in csv_reader:
            try:
                data.append(float(row[1]))
            except:
                pass
            
def predictData(data):
    res = []
    for i in range(0, len(data[0])):
        res.append((float(data[1][i]) + (float(data[1][i]) - float(data[0][i])) / 4))
        if i != 0:
            res[i] += (data[1][i] - data[1][i - 1]) / 4
    
    return res

for(i, filename) in enumerate(filename_bitcoin):
    readData(filename, bitcoin[i])
for(i, filename) in enumerate(filename_dogecoin):
    readData(filename, dogecoin[i])

bitcoin2023 = predictData(bitcoin)
dogecoin2023 = predictData(dogecoin)


fig, ax = plt.subplots(3, 1, figsize=(8, 10), tight_layout=True)
ax2 = [ax[0].twinx(), ax[1].twinx(), ax[2].twinx()]

for i in range(0, 3):
    ax[i].set_ylabel("Price bitcoin", color='b')
    ax2[i].set_ylabel("Price dogecoin", color='g')

for (i, year) in enumerate(years):
    ax[i].plot(months, bitcoin[i], 'b-', label="Bitcoin")
    ax2[i].plot(months, dogecoin[i], 'g-', label="Dogecoin")
    ax[i].title.set_text(str(year))

ax[2].plot(months, bitcoin2023, 'b-', label="Bitcoin")
ax2[2].plot(months, dogecoin2023, 'g-', label="Dogecoin")
ax[2].title.set_text("2023 (predicted)")

plt.show()
