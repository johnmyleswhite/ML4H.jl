using Optim, DataFrames, GLM, Vega

# Load in the data set from disk
data_file = joinpath("data", "01_heights_weights_genders.csv")
heights_weights = readtable(data_file, header = true, separator = ',')

# Create a numeric vector containing just the heights data.
heights = with(heights_weights, :Height)
describe(heights)

# Expected output
# Min      54.2631333250971
# 1st Qu.  63.505620481218955
# Median   66.31807008178464
# Mean     66.36755975482106
# 3rd Qu.  69.1742617268347
# Max      78.9987423463896

# Define our own mean and median functions
my_mean(x::AbstractVector) = sum(x) / length(x)

function my_median(x::AbstractVector)
    sorted_x = sort(x)
    if rem(length(x), 2) == 0
        indices = [length(x) / 2, length(x) / 2 + 1]
        return mean(sorted_x[indices])
    else
        index = iceil(length(x) / 2)
        return float(sorted_x[index])
    end
end

# Compare means and medians on toy examples
my_vector = [0, 100]

mean(my_vector)

median(my_vector)

my_vector = [0, 0, 100]

mean(my_vector)

median(my_vector)

# Confirm that our mean and median functions produce the correct answer
my_mean(heights)

my_median(heights)

mean(heights) - my_mean(heights)

median(heights) - my_median(heights)

# Experiment with functions for assessing the range of a data set
min(heights)

max(heights)

min(heights), max(heights)

minmax(heights)

# Try out the quantile function for computing arbitrary quantiles
quantile(heights)
# 5-element Float64 Array:
#  54.2631
#  63.5056
#  66.3181
#  69.1743
#  78.9987

quantile(heights, [0:0.20:1])
# 6-element Float64 Array:
#  54.2631
#  62.859 
#  65.1942
#  67.4354
#  69.8116
#  78.9987

[0:0.20:1]
# 6-element Float64 Array:
#  0.0
#  0.2
#  0.4
#  0.6
#  0.8
#  1.0

# Define a variance function to assess the spread of data
function my_var(x::AbstractVector)
    m = mean(x)
    return sum((x .- m).^2) / length(x)
end

# Test our variance function for correctness
my_var(heights) - var(heights)

# Update the variance function to make it unbiased
function my_var(x::AbstractVector)
    m  = mean(x)
    return sum((x .- m).^2) / (length(x) - 1)
end

# Test our variance function again for correctness
my_var(heights) - var(heights)

# Check the range predicted by the variance function
[mean(heights) - var(heights), mean(heights) + var(heights)]
# 2-element Float64 Array:
#  51.5641
#  81.171 

minmax(heights)
# (54.2631333250971,78.9987423463896)

# Switch to standard deviations instead for thinking about ranges
my_std(x::AbstractVector) = sqrt(my_var(x))

# Test our standard deviation function for correctness
my_std(heights) - std(heights)

[mean(heights) - std(heights), mean(heights) + std(heights)]
# 2-element Float64 Array:
#  62.52  
#  70.2151

minmax(heights)
# (54.2631333250971,78.9987423463896)

[mean(heights) - std(heights), mean(heights) + std(heights)]
# 2-element Float64 Array:
#  62.52  
#  70.2151

[quantile(heights, 0.25), quantile(heights, 0.75)]
# 2-element Float64 Array:
#  63.5056
#  69.1743

# Start visualizing data using the VGPlot package
using VGPlot

# Load the data from scratch for purity
data_file = joinpath("data", "01_heights_weights_genders.csv")
heights_weights = readtable(data_file, header = true, separator = ',')

# # Experiment with histograms
# ggplot(heights.weights, aes(x = Height)) +
#   geom_histogram(binwidth = 1)

# ggplot(heights.weights, aes(x = Height)) +
#   geom_histogram(binwidth = 5)

# ggplot(heights.weights, aes(x = Height)) +
#   geom_histogram(binwidth = 0.01)

# # Experiment with kernel density estimates
# ggplot(heights.weights, aes(x = Height)) +
#   geom_density()

# # Separate out heights and weights based on gender
# ggplot(heights.weights, aes(x = Height, fill = Gender)) +
#   geom_density()

# ggplot(heights.weights, aes(x = Weight, fill = Gender)) +
#   geom_density()

# # Produce two facets in a single plot to make it easier to see the hidden structure
# ggplot(heights.weights, aes(x = Weight, fill = Gender)) +
#   geom_density() +
#   facet_grid(Gender ~ .)

# Experiment with random numbers from the normal distribution
# m = 0
# s = 1
# ggplot(data.frame(X = rnorm(100000, m, s)), aes(x = X)) +
#   geom_density()

# Compare the normal distribution with the Cauchy distribution
srand(1)
normal_values = rand(Normal(0, 1), 250)
cauchy_values = rand(Cauchy(0, 1), 250)
minmax(normal_values)
minmax(cauchy_values)

# ggplot(data.frame(X = normal.values), aes(x = X)) +
#   geom_density()
# ggplot(data.frame(X = cauchy.values), aes(x = X)) +
#   geom_density()

# Experiment with random numbers from the gamma distribution
gamma_values = rand(Gamma(1, 0.001), 100000)
# ggplot(data.frame(X = gamma.values), aes(x = X)) +
#   geom_density()

# # Generate scatterplots of the heights and weights to see their relationship
# ggplot(heights.weights, aes(x = Height, y = Weight)) +
#   geom_point()

# # Add a smooth shape that relates the two explicitly
# ggplot(heights.weights, aes(x = Height, y = Weight)) +
#   geom_point() +
#   geom_smooth()

# # See how the smooth shape gets better with more data
# ggplot(heights.weights[1:20, ], aes(x = Height, y = Weight)) +
#   geom_point() +
#   geom_smooth()
# ggplot(heights.weights[1:200, ], aes(x = Height, y = Weight)) +
#   geom_point() +
#   geom_smooth()
# ggplot(heights.weights[1:2000, ], aes(x = Height, y = Weight)) +
#   geom_point() +
#   geom_smooth()

# # Visualize how gender depends on height and weight
# ggplot(heights.weights, aes(x = Height, y = Weight)) +
#   geom_point(aes(color = Gender, alpha = 0.25)) +
#   scale_alpha(guide = "none") + 
#   scale_color_manual(values = c("Male" = "black", "Female" = "gray")) +
#   theme_bw()

# # An alternative using bright colors
# ggplot(heights.weights, aes(x = Height, y = Weight, color = Gender)) +
#   geom_point()

heights_weights["Male"] =
  float([z == "Male" for z in heights_weights["Gender"]])

logit_model = glm(:(Male ~ Weight + Height),
                  heights_weights,
                  Bernoulli())

# ggplot(heights.weights, aes(x = Height, y = Weight)) +
#   geom_point(aes(color = Gender, alpha = 0.25)) +
#   scale_alpha(guide = "none") + 
#   scale_color_manual(values = c("Male" = "black", "Female" = "gray")) +
#   theme_bw() +
#   stat_abline(intercept = -coef(logit.model)[1] / coef(logit.model)[2],
#               slope = - coef(logit.model)[3] / coef(logit.model)[2],
#               geom = 'abline',
#               color = 'black')
