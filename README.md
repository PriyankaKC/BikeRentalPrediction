# BikeRentalPrediction

Problem Statement 

In this project we aim to predict the number of bikes rented based on seasonal and environmental settings. By doing so we will be able to manage the number of bikes that shold be made available everyday in an efficient manner, such that there is no loss to the business either by surplus or scarcity if bikes.

Data

1) instant : Record index
2) dteday : Date
3) season : Season (1:springer, 2:summer, 3:fall, 4:winter)
4) yr : Year (0: 2011, 1:2012)
5) mnth : Month (1 to 12)
6) hr : Hour (0 to 23)
7) holiday : weather day is holiday or not (extracted fromHoliday Schedule)
8) weekday : Day of the week
9) workingday : If day is neither weekend nor holiday is 1, otherwise is 0.
10) weathersit : (extracted fromFreemeteo)1: Clear, Few clouds, Partly cloudy, Partly cloudy 2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist 3: Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds 4: Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog
11) temp : Normalized temperature in Celsius. The values are derived via (t-t_min)/(t_max-t_min), t_min=-8, t_max=+39 (only in hourly scale)
12) atemp : Normalized feeling temperature in Celsius. The values are derived via (t-t_min)/(t_maxt_min), t_min=-16, t_max=+50 (only in hourly scale)
13) hum : Normalized humidity. The values are divided to 100 (max)
14) windspeed : Normalized wind speed. The values are divided to 67 (max)
15) casual : count of casual users
16) registered : count of registered users
17) cnt : count of total rental bikes including both casual and registered

