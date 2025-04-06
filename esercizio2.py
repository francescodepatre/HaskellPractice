'''
Write a Python iterator
Instantiated over a list of ints
Produced values: each new maximum
Tops seen looking from left to right
Run over a random list
Print all produced values
Testo della risposta Domanda 2
'''

import random as rnd

random_list = [rnd.randint(1, 100) for _ in range(10)]

class Iterator:
    def __init__(self,inputList):
        self.list = inputList
        self.maxVal = 0
        self.index = 0

    def __iter__(self):
        return self
    
    def __next__(self):
        while self.index < len(self.list):
            num = self.list[self.index]
            self.index += 1
            if num > self.maxVal:
                self.maxVal = num
                return num  
        raise StopIteration


it = Iterator(random_list)
print("Maximum list: ",list(it))