data(iris)

mod0 = lm(Sepal.Length ~ 1,
          data = iris)
mod1 = lm(Sepal.Length ~ Sepal.Width,
          data = iris)
mod2 = lm(Sepal.Length ~ 1 + Sepal.Width + Petal.Length + Petal.Width,
          data = iris)
mod3 = lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
          data = iris)

modglm = glm(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
             data = iris[iris$Species %in% c("versicolor", "virginica"), ],
             family = "binomial")

modcm = lm(Sepal.Length ~ -1 + Species,
           data = iris)

test_that("my_anova works", {
  expect_equal(my_anova(mod0), anova(mod0))
  expect_equal(my_anova(mod1), anova(mod1))
  expect_equal(my_anova(mod2), anova(mod2))
  expect_equal(my_anova(mod3), anova(mod3))
  expect_equal(my_anova(mod0, mod1), anova(mod0, mod1))
  expect_equal(my_anova(mod0, mod3), anova(mod0, mod3))
  expect_equal(my_anova(mod1, mod2), anova(mod1, mod2))
  expect_equal(my_anova(mod1b, mod2b, mod3b), anova(mod1b, mod2b, mod3b))
  expect_equal(my_anova(mod0, mod1, mod2, mod3), anova(mod0, mod1, mod2, mod3))
  expect_error(my_anova(modglm), "At least one of the objects supplied to my_anova does not have class \"lm\".")
  expect_error(my_anova(modcm), "A cell-means coded linear model cannot be the only object supplied to my_anova.")
})
