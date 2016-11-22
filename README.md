# CS4012-svg-server

## Usage Instructions:

Build with 'stack build' then run with 'stack exec shapes'. 
Point your browser at localhost:3000/form, where you will be presented with the input form. 

## Sample Inputs:

Drawing [

  (Compose (Translate (Vector 100 50)) (Rotate 45), Square, [Fill Green, Stroke Blue, Outline 5, Size 100]), 
  (Translate (Vector 200 200), Circle, [Fill (Hex "#000000"), Size 30])

]
 
Drawing [

  (Translate (Vector 100 100), Square, [Fill (Hex "#FF0000"), Size 100])

] 

Drawing [

  (Identity, Circle, [Size 50, Fill Blue, Stroke Green, Outline 2.5]),

  (Compose (Compose (Translate (Vector 100 100)) (Rotate 45)) (Scale (Vector 50 50)) , Square, [Fill Red, Size 1]),

  (Identity, Empty, [Size 50, Fill Green])

] 
