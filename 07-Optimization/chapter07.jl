using Optim
using DataFrames
using GLM
using Vega

height_to_weight(height::AbstractVector, a::Real, b::Real) = a + b * height

heights_weights =
  readtable(joinpath("data", "01_heights_weights_genders.csv"),
            makefactors = true)

coef(lm(:(Weight ~ Height), heights_weights))
# 2-element Float64 Array:
#  -350.737  
#     7.71729

function squared_error(heights_weights::DataFrame, a::Real, b::Real)
    heights_weights["predictions"] =
      height_to_weight(heights_weights["Height"], a, b)
    errors = with(heights_weights, :(Weight - predictions))
    return sum(errors.^2)
end

# Fourth code snippet
for a in -1:1
    for b in -1:1
        println(squared_error(heights_weights, a, b))
    end
end

# Fifth code snippet
optimize(x -> squared_error(heights_weights, x[1], x[2]), [0.0, 0.0])
# Results of Optimization Algorithm
#  * Algorithm: Nelder-Mead
#  * Starting Point: [0.0,0.0]
#  * Minimum: [-350.72215153079594,7.7170645464835825]
#  * Value of Function at Minimum: 1492934.847406
#  * Iterations: 54
#  * Convergence: true
#    * |x - x'| < NaN: false
#    * |f(x) - f(x')| / |f(x)| < 1.0e-08: true
#    * |g(x)| < NaN: false
#    * Exceeded Maximum Number of Iterations: false
#  * Objective Function Calls: 104
#  * Gradient Call: 0

a_error(a::Real) = squared_error(heights_weights, a, 0)
@vectorize_1arg Real a_error

x = [-1000:1000]
plot(x = x, y = a_error(x), kind = :line)

b_error(b::Real) = squared_error(heights_weights, 0, b)
@vectorize_1arg Real b_error

x = [-1000:1000]
plot(x = x, y = b_error(x), kind = :line)

function ridge_error(heights_weights::DataFrame, a::Real, b::Real, lambda::Real)
    heights_weights["predictions"] =
      height_to_weight(heights_weights["Height"], a, b)
    errors = with(heights_weights, :(Weight - predictions))
    return sum(errors.^2) + lambda * (a^2 + b^2)
end

lambda = 1

optimize(x -> ridge_error(heights_weights, x[1], x[2], lambda), [0.0, 0.0])
# Results of Optimization Algorithm
#  * Algorithm: Nelder-Mead
#  * Starting Point: [0.0,0.0]
#  * Minimum: [-340.5693335224347,7.564589136710431]
#  * Value of Function at Minimum: 1612442.199855
#  * Iterations: 63
#  * Convergence: true
#    * |x - x'| < NaN: false
#    * |f(x) - f(x')| / |f(x)| < 1.0e-08: true
#    * |g(x)| < NaN: false
#    * Exceeded Maximum Number of Iterations: false
#  * Objective Function Calls: 119
#  * Gradient Call: 0

a_ridge_error(a, lambda) = ridge_error(heights_weights, a, 0, lambda)

x = [-1000:1000]
plot(x = x, y = [a_ridge_error(x_i, lambda) for x_i in x], kind = :line)

b_ridge_error(b, lambda) = ridge_error(heights_weights, 0, b, lambda)

x = [-1000:1000]
plot(x = x, y = [b_ridge_error(x_i, lambda) for x_i in x], kind = :line)

function absolute_error(heights_weights, a, b)
    heights_weights["predictions"] =
      height_to_weight(heights_weights["Height"], a, b)
    errors = with(heights_weights, :(Weight - predictions))
    return sum(abs(errors))
end

a_absolute_error(a) = absolute_error(heights_weights, a, 0)

x = [-1000:1000]
plot(x = x, y = [a_absolute_error(x_i) for x_i in x], kind = :line)

english_letters = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k',
                   'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
                   'w', 'x', 'y', 'z']

caesar_cipher = Dict()

inverse_caesar_cipher = Dict()

for index in 1:length(english_letters)
    caesar_cipher[english_letters[index]] = english_letters[mod(index, 26) + 1]
    inverse_caesar_cipher[english_letters[mod(index, 26) + 1]] = english_letters[index]
end

println(caesar_cipher)

apply_cipher_to_string(string, cipher) =
 join(map(chr -> cipher[chr], collect(string)))

function apply_cipher_to_text(text, cipher)
    output = ASCIIString[]

    for string in text
        push!(output, apply_cipher_to_string(string, cipher))
    end

    return output
end

apply_cipher_to_text(["sample", "text"], caesar_cipher)

function generate_random_cipher()
    cipher = Dict(english_letters,
                  english_letters[randperm(length(english_letters))])
    return cipher
end

function modify_cipher(cipher, input, output)
    new_cipher = copy(cipher)
    new_cipher[input] = output
    old_output = cipher[input]
    collateral_input = 
      first(collect(keys(cipher))[find(map(key -> cipher[key] == output, collect(keys(cipher))))])
    new_cipher[collateral_input] = old_output
    return new_cipher
end

function propose_modified_cipher(cipher)
    input = collect(keys(cipher))[rand(1:length(cipher))]
    output = english_letters[rand(1:length(english_letters))]
    return modify_cipher(cipher, input, output)
end

lexicon = readtable(joinpath("data", "lexical_database.csv"))
lexical_database = Dict(lexicon["Word"], lexicon["Frequency"])

lexical_database["a"]
lexical_database["the"]
lexical_database["he"]
lexical_database["she"]
lexical_database["data"]

function one_gram_probability(one_gram, lexical_database = Dict())
    if !haskey(lexical_database, one_gram)
        return eps()
    else
        return lexical_database[one_gram]
    end
end

function log_probability_of_text(text, cipher, lexical_database = Dict())
    log_probability = 0.0

    for string in text
        decrypted_string = apply_cipher_to_string(string, cipher)
        log_probability += log(one_gram_probability(decrypted_string,
                                                    lexical_database))
    end

    return log_probability
end

function metropolis_step(text, cipher, lexical_database = Dict())
    proposed_cipher = propose_modified_cipher(cipher)

    lp1 = log_probability_of_text(text, cipher, lexical_database)
    lp2 = log_probability_of_text(text, proposed_cipher, lexical_database)

    if lp2 > lp1
        return proposed_cipher
    else
        a = exp(lp2 - lp1)
        x = rand()
        if x < a
          return proposed_cipher
        else
          return cipher
        end
    end
end

decrypted_text = ["here", "is", "some", "sample", "text"]

encrypted_text = apply_cipher_to_text(decrypted_text, caesar_cipher)

srand(1)

cipher = generate_random_cipher()

results = DataFrame()

number_of_iterations = 50_000

for iteration in 1:number_of_iterations
    log_probability = log_probability_of_text(encrypted_text,
                                              cipher,
                                              lexical_database)

    current_decrypted_text = join(apply_cipher_to_text(encrypted_text, cipher), " ")

    correct_text = int(current_decrypted_text == join(decrypted_text, " "))

    results = rbind(results,
                    DataFrame(Iteration = iteration,
                              LogProbability = log_probability,
                              CurrentDecryptedText = current_decrypted_text,
                              CorrectText = correct_text))

    cipher = metropolis_step(encrypted_text, cipher, lexical_database)
end

writetable(joinpath("data/results.tsv"), results)
