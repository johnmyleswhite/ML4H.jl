using DataFrames
using GLM
using VGPlot

ages = readtable(joinpath("data", "longevity.csv"))

# areaplot(x = vector(with(select(:(Smokes .== 0), ages), :(AgeAtDeath))))

# ggplot(ages, aes(x = AgeAtDeath, fill = factor(Smokes))) +
#   geom_density() +
#   facet_grid(Smokes ~ .)

guess = 73.0

se = (ages["AgeAtDeath"] .- guess).^2
mean(se)

guess_accuracy = DataFrame()
guess_accuracy["Guess"] = 63.0:83.0
guess_accuracy["Error"] = DataArray(Float64, nrow(guess_accuracy))

for i in 1:nrow(guess_accuracy)
    guess = guess_accuracy[i, "Guess"]
    se = (ages["AgeAtDeath"] .- guess).^2
    mse = mean(se)
    guess_accuracy[i, "Error"] = mse
end

vgplot(guess_accuracy, x = "Guess", y = "Error") +
  geom_point() +
  geom_line()

constant_guess = mean(ages["AgeAtDeath"])

rmse = sqrt(mean((ages["AgeAtDeath"] .- constant_guess).^2))

non_smokers_guess = mean(ages[:(Smokes .== 0), :]["AgeAtDeath"])
smokers_guess = mean(ages[:(Smokes .== 1), :]["AgeAtDeath"])

ages["NewPrediction"] = datazeros(nrow(ages))
for i in 1:nrow(ages)
    if ages[i, "Smokes"] == 0
        ages[i, "NewPrediction"] = non_smokers_guess
    else
        ages[i, "NewPrediction"] = smokers_guess
    end
end

sqrt(mean((ages["AgeAtDeath"] .- ages["NewPrediction"]).^2))

heights_weights = readtable(joinpath("data", "01_heights_weights_genders.csv"))

vgplot(heights_weights, x = "Height", y = "Weight") +
  geom_point()# +
  # geom_smooth(method = 'lm')

fitted_regression = lm(:(Weight ~ Height), heights_weights)

coef(fitted_regression)

intercept = coef(fitted_regression)[1]
slope = coef(fitted_regression)[2]
predict(fitted_regression)

true_values = with(heights_weights, :(Weight))
errors = true_values - predict(fitted_regression)

residuals(fitted_regression)

# plot(fitted.regression, which = 1)

x = 1.0:10.0
y = x.^2
df = DataFrame()
df["x"] = x
df["y"] = y

fitted_regression = lm(:(y ~ x), df)

# plot(fitted_regression, which = 1)

x = 1.0:10.0
y = x.^2
df = DataFrame()
df["x"] = x
df["y"] = y

fitted_regression = lm(:(y ~ x), df)

errors = residuals(fitted_regression)
squared_errors = errors.^2
sum(squared_errors)

x = 1.0:10.0
y = x.^2
df = DataFrame()
df["x"] = x
df["y"] = y

fitted_regression = lm(:(y ~ x), df)

errors = residuals(fitted_regression)
squared_errors = errors.^2
mse = mean(squared_errors)
mse

x = 1.0:10.0
y = x.^2

fitted_regression = lm(:(y ~ x), df)

errors = residuals(fitted_regression)
squared_errors = errors.^2
mse = mean(squared_errors)
rmse = sqrt(mse)
rmse

mean_mse = 1.09209343
model_mse = 0.954544

r2 = 1 - (model_mse / mean_mse)

top_1000_sites = readtable(joinpath("data", "top_1000_sites.tsv"),
                           separator = '\t')

vgplot(top_1000_sites, x = "PageViews", y = "UniqueVisitors") +
  geom_point()
# ggsave(file.path("images", "page_views_vs_visitors.pdf"))

# ggplot(top.1000.sites, aes(x = PageViews)) +
#   geom_density()

# ggplot(top.1000.sites, aes(x = log(PageViews))) +
#   geom_density()

vgplot(top_1000_sites, x = "PageViews", y = "UniqueVisitors",
       scalex = :log, scaley = :log) +
  geom_point()
# ggsave(file.path("images", "log_page_views_vs_log_visitors.pdf"))

# ggplot(top.1000.sites, aes(x = log(PageViews), y = log(UniqueVisitors))) +
#   geom_point() +
#   geom_smooth(method = 'lm', se = FALSE)
# ggsave(file.path("images", "log_page_views_vs_log_visitors_with_lm.pdf"))

lm_fit = lm(:(log(PageViews) ~ log(UniqueVisitors)), top_1000_sites)

lm_fit = lm(:(log(PageViews) ~ HasAdvertising + log(UniqueVisitors) + InEnglish),
             top_1000_sites)
describe(lm_fit)

lm_fit = lm(:(log(PageViews) ~ HasAdvertising), top_1000_sites)
inference(lm_fit).r_squared

lm_fit = lm(:(log(PageViews) ~ log(UniqueVisitors)),
            data = top_1000_sites)
summary(lm_fit).r_squared
#[1] 0.4615985

lm_fit = lm(:(log(PageViews) ~ InEnglish),
            data = top_1000_sites)
summary(lm_fit).r_squared
#[1] 0.03122206

# Twenty-sixth snippet
x = [1.0:10.0]
y = x.^2

vgplot(DataFrame(x = x, y = y)) +
  geom_point() # +
  # geom_smooth(method = 'lm', se = FALSE)

# Twenty-seventh snippet
cor(x, y)
#[1] 0.9745586

# Twenty-eighth snippet
coef(lm(:(scale(y) ~ scale(x))))
# (Intercept) scale(x)
#-1.386469e-16 9.745586e-01
