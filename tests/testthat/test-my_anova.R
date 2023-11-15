data(iris)

mod0 = lm(Sepal.Length ~ 1,
          data = iris)
mod1 = lm(Sepal.Length ~ Sepal.Width,
          data = iris)
mod2 = lm(Sepal.Length ~ 1 + Sepal.Width + Petal.Length + Petal.Width,
          data = iris)
mod3 = lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
          data = iris)

modni0 = lm(Sepal.Length ~ - 1,
            data = iris)

modni1 = lm(Sepal.Length ~ - 1 + Sepal.Width,
            data = iris)

modni2 = lm(Sepal.Length ~ - 1 + Sepal.Width + Species,
            data = iris)

modglm = glm(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
             data = iris[iris$Species %in% c("versicolor", "virginica"), ],
             family = "binomial")

modint1 = lm(Sepal.Length ~ Sepal.Width + Petal.Length * Petal.Width,
             data = iris)

modint2 = lm(Sepal.Length ~ Sepal.Width + Petal.Length * Petal.Width + Species,
             data = iris)

test_that("my_anova works", {
  expect_equal(my_anova(mod0), anova(mod0))
  expect_equal(my_anova(mod1), anova(mod1))
  expect_equal(my_anova(mod2), anova(mod2))
  expect_equal(my_anova(mod3), anova(mod3))
  expect_equal(my_anova(mod0, mod1), anova(mod0, mod1))
  expect_equal(my_anova(mod0, mod3), anova(mod0, mod3))
  expect_equal(my_anova(mod1, mod2), anova(mod1, mod2))
  expect_equal(my_anova(mod0, mod1, mod2, mod3), anova(mod0, mod1, mod2, mod3))
  expect_equal(my_anova(modni0), anova(modni0))
  expect_equal(my_anova(modni1), anova(modni1))
  expect_equal(my_anova(modni2), anova(modni2))
  expect_equal(my_anova(modni0, modni1), anova(modni0, modni1))
  expect_equal(my_anova(modni0, modni2), anova(modni0, modni2))
  expect_equal(my_anova(modni1, modni2), anova(modni1, modni2))
  expect_equal(my_anova(modni0, modni1, modni2), anova(modni0, modni1, modni2))
  expect_equal(my_anova(modint1, modint2), anova(modint1, modint2))
  expect_equal(my_anova(mod3, modint2), anova(mod3, modint2))
  expect_error(my_anova(modglm), "At least one of the objects supplied to my_anova does not have class \"lm\".")
  expect_error(my_anova(modint1), "The my_anova function is not intended for ANOVA analysis of an individual linear interaction model")
})
