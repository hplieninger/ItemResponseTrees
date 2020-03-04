data("jackson")
test_that("jackson data are recoded", {
    df1 <- select(jackson, matches("^N\\d+", FALSE))
    df2 <- select(jackson, matches("^E\\d+", FALSE))
    df3 <- select(jackson, matches("^O\\d+", FALSE))
    df4 <- select(jackson, matches("^A\\d+", FALSE))
    df5 <- select(jackson, matches("^C\\d+", FALSE))

    expect_true(all(1 == sign(cor(df1, use = "pair"))))
    expect_true(all(1 == sign(cor(df2, use = "pair"))))
    expect_true(all(1 == sign(cor(df3, use = "pair"))))
    expect_true(all(1 == sign(cor(df4, use = "pair"))))
    expect_true(all(1 == sign(cor(df5, use = "pair"))))
})
