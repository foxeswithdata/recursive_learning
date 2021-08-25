## Power Law Test

## How much water is being 

water_input = seq(from = 0, to = 50, by = 0.05)

k = -0.75
c = 10 # water average

k = 4
scale = 0.75

expectation = k * scale

probability = dchisq(water_input, df = 3)
 # = dgamma(water_input, scale, rate = k)

plot(probability ~ water_input, type = "l")


cumulative = c * (water_input/10)^(1-k)

plot(cumulative ~ water_input)





