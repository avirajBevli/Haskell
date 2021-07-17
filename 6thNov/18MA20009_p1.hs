--Aviraj Singh Bevli
--18MA20009
{-
1) Write a program to decide if a given number  is expressible as a sum of two cubes in two different ways. 
For example, 1729 =  1^3 + 12^3 = 9^3 + 10^3. Your program should also show the pairs of two numbers whose cube sum up to the input. [5 marks]
-}

--Function takes as input a number and represents a list of 2 tuples which represent integers whose sum of entries equals the input number
--If the output is [], this means that at max one pair of numbers having sum of cubes eqaul to n exist
--If the ouptut is non-empty, the list of tuples which give the sum of cubes = n is printed
are_two_cube_sum_same :: Int -> [(Int, Int)]
--We are only taking b only from 1 to (n-a^3) because going till n is a waste as for b>(n-a^3), it is impossible to obtain a^3+b^3 equal to n
--If we take b from 1 to n, computation will take a long time
are_two_cube_sum_same n
    | ( length( [(a,b) | a <- [1..n], b <- [1..n-a*a*a], a<b, (a*a*a+b*b*b)==n] ) <= 1 ) = []
    | otherwise = [(a,b) | a<-[1..n], b<-[1..n-a*a*a], a<b, (a*a*a+b*b*b)==n]

main = do
    print $ are_two_cube_sum_same 65728
    print $ are_two_cube_sum_same 27
    print $ are_two_cube_sum_same 13832
    print $ are_two_cube_sum_same 125