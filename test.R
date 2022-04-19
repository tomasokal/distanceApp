latitude = 43
longitude = 43
distance = 150
radius = 6371000
bearing = pi / 2

phi = latitude
lambda = longitude
delta = distance / radius
theta = bearing

phi2 = asin( sin(phi) * cos(delta) + cos(phi) * sin(delta) * cos(theta) )
lambda2 = lambda + atan2( (sin(theta) * sin(delta) * cos(phi) ), (cos(delta) - sin(phi) * phi2) )
