
member :: Int -> [Int] ->Bool
member a []= False
member a(b:xs)
 |a==b = True
 |otherwise = member a xs

union :: [Int] -> [Int] -> [Int]

union [] [] = []
union [] ys  = removedups ys 
union xs []  = removedups xs 
union (xs) (y:ys) | member y (xs) == False =   y :union  (xs)  (removedups ys) 
union (xs) (y:ys) | member y (xs) == True = union  (xs)  (removedups ys)  

removedups  :: [Int] -> [Int]
removedups [] = [] 
removedups (x:xs) 
 |elem x xs = removedups xs 
 |otherwise = x:removedups xs


delete:: Int->[Int]->[Int]
delete 0 ys  = ys
delete k [] = []
delete k (ys) = delete2  ys k k
  
delete2 :: [Int]->Int-> Int->[Int]
delete2 [] k n = []
delete2 (y:ys) k n
   | k>1 = y:delete2 ys (k-1) n
   | k==1 =  delete2 ys n n

data Tree = Leaf Int | Node Tree Int Tree
occurs :: Int -> Tree -> Bool
occurs n (Leaf y)
            | y == n = True
	    | otherwise = False
occurs n (Node tr1 n1 tr2)
	    | n1 == n = True

            | otherwise = (occurs n tr1) || (occurs n tr2)


delete1 :: Int -> [Int] -> [Int]
delete1 x[]=[]
delete1 x(y:ys)
 |x==y =ys
 |otherwise = y : delete1 x ys

delete_last :: Int -> [Int] -> [Int]
delete_last x []=[]
delete_last x (ys) 
 =reverse (delete1 x (reverse (ys)))


switchelement :: Int -> [Int]->[Int]
switchelement y[]=[y]
switchelement y (x:xs) 
 |y >= x = [x] ++ (switchelement y xs) 
 |y < x = [y] ++ [x] ++ xs                       

isort :: [Int] -> [Int]
isort []=[]
isort (y:ys) = switchelement y(isort ys)

	    