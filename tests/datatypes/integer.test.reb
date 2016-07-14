; datatypes/integer.r
[integer? 0]
; bug#33
[integer? -0]
[not integer? 1.1]
[integer! = type-of 0]
[integer? 1]
[integer? -1]
[integer? 2]
; 32bit minimum
[integer? -2147483648]
; 32bit maximum
[integer? 2147483647]
; 64bit minimum
#64bit
[integer? -9223372036854775808]
; 64bit maximum
#64bit
[integer? 9223372036854775807]
[0 == make integer! 0]
[0 == make integer! "0"]
[0 == to integer! 0]
[-2147483648 == to integer! -2147483648.0]
[-2147483648 == to integer! -2147483648.9]
[2147483647 == to integer! 2147483647.9]
#32bit
[error? try [to integer! -2147483649.0]]
#32bit
[error? try [to integer! 2147483648.0]]
; bug#921
[error? try [to integer! 9.2233720368547765e18]]
[error? try [to integer! -9.2233720368547779e18]]
[0 == to integer! "0"]
[error? try [to integer! false]]
[error? try [to integer! true]]
[0 == to integer! #"^@"]
[1 == to integer! #"^a"]
[0 == to integer! #0]
[1 == to integer! #1]
[0 == to integer! #{00}]
[1 == to integer! #{01}]
#32bit
[-1 == to integer! #{ffffffff}]
#64bit
[-1 == to integer! #{ffffffffffffffff}]
#64bit
[302961000000 == to integer! "3.02961E+11"]
[error? try [to integer! "t"]]
["0" = mold 0]
["1" = mold 1]
["-1" = mold -1]