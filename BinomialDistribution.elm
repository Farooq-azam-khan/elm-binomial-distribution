module Main exposing (main)
-- https://en.wikipedia.org/wiki/Binomial_distribution
import Html exposing (..)


render_int_new_line txt = div [] [txt |> String.fromInt |> text]
  

choose : Int -> Int -> Int 
choose n k = 
    let
        in_map i = (toFloat n+1.0-toFloat i) / (toFloat i)
    in 
     List.range 1 k 
     |> List.map in_map  
     |> List.foldl (*) 1.0 |> truncate

binomial_pmf : Int -> Int -> Float -> Float
binomial_pmf n k p =
    toFloat (choose n k) * p^(toFloat k) * (1-p)^(toFloat (n-k))
    
binomial_cdf : Int -> Int -> Float -> Float    
binomial_cdf n k p = 
    List.range 0 k
    |> List.map (\i -> binomial_pmf n i p) -- (\i -> toFloat (choose n i) * p^(toFloat i) * (1.0-p)^(toFloat (n-i)))
    |> List.sum

binomial_mean :  Int -> Int -> Float -> Float 
binomial_mean n _ p = toFloat n * p 

binomial_variance : Int -> Int -> Float -> Float  
binomial_variance n _ p = toFloat n * p * (1-p)
binomial_var :Int -> Int -> Float -> Float 
binomial_var = binomial_variance


main : Html msg 
main = 
    div []
        [ choose 0 0 |> render_int_new_line
        , choose 1 0 |> render_int_new_line
        , choose 2 0 |> render_int_new_line
        , choose 2 1 |> render_int_new_line
        , choose 2 2 |> render_int_new_line
        , choose 10 5 |> render_int_new_line -- 252
        -- https://www.khanacademy.org/math/ap-statistics/random-variables-ap/binomial-random-variable/e/calculating-binomial-probability
        , h1 [] [text "Binomial Distribution Example"]
        , h2 [] [text "PDF"]
        , p [] [text "70%, percent of a certain species of tomato live after transplanting from pot to garden. Najib transplants 3 of these tomato plants. Assume that the plants live independently of each other. Let X = the number of tomato plants that live."]
        , p [] [text <| "P(X=2) = " ++ String.fromFloat (binomial_pmf 3 2 0.7)]
        , h2 [] [text "CDF"]
        , p [] [text "Layla has a coin that has a 60% chance of showing heads each time it is flipped. She is going to flip the coin 5 times. Let X represent the number of heads she gets."]
        , p [] [text <| "P(X>3) = " ++ String.fromFloat (1.0 - (binomial_cdf 5 3 0.6) )]
        
        ]

