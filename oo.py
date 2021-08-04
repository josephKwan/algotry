class Car:
    def __init__ (self,name,color,mileage):
        self.name = name
        self.color = color
        self.mileage = mileage

    def chargeType(self,charge):
        return f"{self.name} uses {charge}"

blue_car = Car(name="Normal", color="blue", mileage=20_000)
red_car = Car(name="Tesla", color="red", mileage=30_000)

class Tesla(Car):
    pass

for car in (blue_car,red_car):
    print (f"The {car.color} car has {car.mileage:,} miles")

modelY = Tesla(name="Tesla", color="red", mileage=30_000)
print (modelY.chargeType(charge="Electric"))

