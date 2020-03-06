data("jackson")
df1 <- jackson[1:234, paste0("C", 1:5)]
irtree_create_template(names(df1))
m1 <- "
IRT:
t BY C1@1, C2@1, C3@1, C4@1, C5@1;
Class:
GRM"
fit1 <- fit(irtree_model(m1), data = df1)

tidy(fit1, par_type = "difficulty")

glance(fit1)

augment(fit1)
