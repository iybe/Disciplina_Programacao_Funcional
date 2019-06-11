data Complex = Complex {real :: Float, img :: Float} deriving (Eq, Show)

somaC :: Complex -> Complex -> Complex
somaC (Complex r1 i1) (Complex r2 i2) = Complex (r1+r2) (i1+i2)

subC :: Complex -> Complex -> Complex
subC (Complex r1 i1) (Complex r2 i2) = Complex (r1-r2) (i1-i2)

multC :: Complex -> Complex -> Complex
multC (Complex a b) (Complex c d) = Complex (a*c - b*d) (a*d + b*c)