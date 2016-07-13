library(betareg)

data("GasolineYield", package = "betareg")

gy_logit <- betareg(yield ~ temp, data = GasolineYield, subset = batch == 6)
gy_loglog <- betareg(yield ~ temp, data = GasolineYield, subset = batch == 6, link = "loglog")

  
library(ggplot2)

ggplot(GasolineYield, aes(x = temp, y = yield)) +
  geom_point(size = 4, aes(fill = batch), shape = 21) +
  scale_fill_grey() +
  geom_line(aes(y = predict(gy_loglog, GasolineYield),
                colour = "log-log", linetype = "log-log")) +
  geom_line(aes(y = predict(gy_logit, GasolineYield), 
                colour = "logit", linetype = "logit")) +
  scale_colour_manual("", values = c("red", "blue")) +
  scale_linetype_manual("", values = c("solid", "dashed")) +
  theme_bw()
