set.seed(1213)
neuroticism = rnorm(100)
support = rnorm(100)
gender = sample(c("m", "f"), size = 100, replace = T)
age = sample(c("young", "middle", "old"), size = 100, replace = T, prob = c(.4, .3, .3))

depression = neuroticism*gender + -support + rnorm(100)


model.1 = lm(depression ~ neuroticism)
summary(model.1)

model.2 = lm(depression ~ neuroticism + support)
summary(model.2)

model.3 = lm(depression ~ neuroticism + gender)
summary(model.3)

model.4 = lm(depression ~ neuroticism*gender)
summary(model.4)

model.5 = lm(depression ~ neuroticism*support)
summary(model.5)

model.6 = lm(depression ~ age*gender)
summary(model.6)

model.7 = lm(depression ~ neuroticism*age)
summary(model.7)
