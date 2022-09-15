from datetime import datetime


class Car:
    wheel_num = 5
    oil_tank_size = 7

    def __init__(self, car_make, year):
        self.car_make = car_make
        self.year = year

        self.hood_open = False
        self.oil_change_dates = []
        self.oil_level = self.oil_tank_size

    def change_oil(self):
            self.hood_open = True
            self.empty_oil()
            self.add_oil(self.oil_tank_size)
            self.hood_open = False
            self.oil_change_dates.append(datetime.today())

    def empty_oil(self):
        self.oil_level = 0

    def add_oil(self, oil_added):
        self.oil_level += oil_added

