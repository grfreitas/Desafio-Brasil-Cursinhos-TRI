library(ltm)

#modeloIRT 2PL
mod2PL = ltm(acertos ~ z1, 
             IRT.param = TRUE)
#modeloIRT 3PL
mod3PL = tpm(acertos, 
             type = "latent.trait", 
             IRT.param = TRUE)

anova(mod2PL, mod3PL) 

coef(mod3PL)

plot(mod3PL, type = "ICC", items = c(1:45))
plot(mod3PL, type = "ICC", items = c(46:90))
plot(mod3PL, type = "ICC", items = c(91:135))
plot(mod3PL, type = "ICC", items = c(135:180))
plot(mod3PL, type = "IIC", items = 0)

factor.scores(mod3PL)
person.fit(mod3PL)
item.fit(mod3PL)