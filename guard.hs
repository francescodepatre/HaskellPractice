bmiTell :: Float -> Float -> String
bmiTell weight height
  | bmi <= 18.5 = "Underweight, you emo, you!"
  | bmi <= 25.0 = "Normal. Pfft, I bet you're ugly!"
  | bmi <= 30.0 = "Fat! Lose some weight, fatty!"
  | otherwise = "A whale, congratulations!"
  where
    bmi = weight / (height ** 2)