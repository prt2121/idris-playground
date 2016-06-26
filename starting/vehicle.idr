
data PowerSource = Petrol | Pedal | Electric

-- 4.2.4 Exercises
-- Extend the Vehicle data type so that it supports unicycles and motorcycles, and wheels and refuel
data Vehicle : PowerSource -> Type where
     Bicycle : Vehicle Pedal
     Unicycle : Vehicle Pedal
     Car : (fuel : Nat) -> Vehicle Petrol
     Bus : (fuel : Nat) -> Vehicle Petrol
     Motorcycle : (fuel : Nat) -> Vehicle Petrol
     Tram : (charge : Nat) -> Vehicle Electric
     ElectricCar : (charge : Nat) -> Vehicle Electric

wheels : Vehicle power -> Nat
wheels Bicycle = 2
wheels Unicycle = 1
wheels (Car fuel) = 4
wheels (Bus fuel) = 4
wheels (Tram charge) = 40
wheels (ElectricCar charge) = 4

refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Bus 120

data Vect : Nat -> Type -> Type where
  Nil : Vect Z a
  (::) : (x : a) -> (xs : Vect k a) -> Vect (S k) a
%name Vect xs, ys, zs
