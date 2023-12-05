import matplotlib.pyplot as plt 
import random

vetor = []

for i in range(100):
    aleatorio = random.randint(0, 50)
    vetor.append(aleatorio)
    
plt.boxplot(vetor)
plt.show()