from sept16.car import Car

test_car = Car("toyota", 2010)
other_car = Car("honda", 2010)

print("Before, test car: {}".format(test_car.oil_change_dates))
print("Before, other car: {}".format(other_car.oil_change_dates))

test_car.change_oil("2022-09-16")

print("After, test car: {}".format(test_car.oil_change_dates))
print("After, other car: {}".format(other_car.oil_change_dates))



