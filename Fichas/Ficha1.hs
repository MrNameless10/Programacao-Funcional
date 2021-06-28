-------------- exercicio 1 -----------------

--a ( que calcula o per´ımetro de uma circunferˆencia, dado o comprimento do seu raio. )
perimetro:: Double -> Double
perimetro r= 2*pi*r


--b (que calcula a distˆancia entre dois pontos no plano Cartesiano. Cada ponto ´e um par de valores do tipo Double.)
dist:: (Float,Float)->(Float,Float)->Float
dist (x1,y1) (x2,y2)= sqrt ((x1-x2)^2 + (y1-y2)^2)
   -----------ou----------
dist':: (Float,Float)->(Float,Float)->Float
dist' p1 p2 = sqrt ((fst p1 - fst p2)^2 + (snd p1 + snd p2)^2)


--c (que recebe uma lista e devolve um par com o primeiro e o u´ltimo elemento dessa lista.)
primUlt:: [Int] -> (Int,Int)
primUlt l = (head l, last l)


--d (tal que multiplo m n testa se o nu´mero inteiro m ´e mu´ltiplo de n.)
multiplo:: Int->Int->Bool
multiplo x y = if (mod x y) == 0
                  then True
                  else False
   -----------ou----------
multiplo':: Int->Int->Bool
multiplo' x y = (mod x y)==0


--e  (que recebe uma lista e, se o comprimento da lista for´ımpar retiralhe o primeiro elemento, caso contr´ario devolve a pr´opria lista.)
truncaImpar:: [a]-> [a]
truncaImpar l = if mod (length l) 2 == 0 
                 then l
                 else tail l
   -----------ou----------
truncaImpar':: [a]->[a]
truncaImpar' l = if even (length l)
                  then l
                  else tail l


--f  (que calcula o maior de dois nu´meros inteiros.)
maior2:: Int->Int->Int
maior2 x y = if x<y then y else x


--g ( que calcula o maior de trˆes nu´meros inteiros, usando a func¸˜ao max2.)
maior3:: Int->Int->Int->Int
maior3 x y z = if x>y then
                           if x>z then x else z
                       else
                           if y>z then y else z
   -----------ou----------
maior3':: Int->Int->Int->Int
maior3'  x y z = maior2 (maior2 x y) z



--------------- exercicio 2 ----------------

-- a
nRaizes:: Double->Double->Double->Int
nRaizes a b c = let 
                  d= b^2-4*a*c
                in 
                  if d<0
                     then 0
                     else if d==0
                             then 1
                             else 2
   -----------ou----------
nRaizes':: Double->Double->Double->Int
nRaizes' a b c
               |d<0 = 0
               |d==0 =1
               |otherwise = 2

     where d= b^2-4*a*c


--b
raizes:: Double->Double->Double->[Double] 
raizes a b c
               |d<0 = []
               |d==0 =[(-b)/(2*a)]
               |otherwise = [r1,r2]

      where d= b^2-4*a*c
            r1= (-b+sqrt d)/(2*a)
            r2= (-b-sqrt d)/(2*a)


--------------- exercicio 3 ----------------

--a
type Hora = (Int,Int)

horavalida:: Hora->Bool
horavalida (h,m) = h>0 && h<24 && m>= 0 && m<60
   -----------ou----------
horavalida':: Hora->Bool
horavalida' (h,m) = (elem h [0..23]) && (elem m [0..59])


--b
depoisde:: Hora->Hora->Bool
depoisde (h1,m1) (h2,m2)
            |h1>h2 =True
            |h1==h2 && m1>m2=True
            |otherwise= False
   -----------ou----------
depoisde':: Hora->Hora->Bool
depoisde' (h1,m1) (h2,m2)
            |(h1>h2) || (h1==h2 && m1>m2) = True
            |otherwise= False 




zip'::[a]->[b]->[(a,b)]
zip' [] l = []
zip' (x:xs) [] = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys
