m1 <- "
IRT:
attitude BY Comfort, Work, Future, Benefit;
Class:
GRM
"
model1 <- irtree_model(m1)
data(Science, package = "mirt")

fit1 <- fit(model1, Science, engine = "mirt", quadpts = 15)

glance(fit1)
tidy(fit1)
augment(fit1)
