 type Cell = (Int,Int)
 data MyState = Null | S Cell [Cell] String MyState  deriving (Show,Eq) 
 up :: MyState -> MyState
 up (S (x,y) (x2:y2) b k) | (x>0) =  (S (x-1,y) (x2:y2) "up" (S (x,y) (x2:y2) b k))
                          |otherwise = Null
 down :: MyState -> MyState
 down (S (x,y) (x2:y2) b k) | x<3 =  (S (x+1,y) (x2:y2) "down" (S (x,y) (x2:y2) b k))
                            |otherwise = Null
 left :: MyState -> MyState 
 left (S (x,y) (x2:y2) b k) | y>0 =  (S (x,y-1) (x2:y2) "left" (S (x,y) (x2:y2) b k))
                            |otherwise = Null
 right :: MyState -> MyState
 right (S (x,y) (x2:y2) b k) | y<3 =  (S (x,y+1) (x2:y2) "right" (S (x,y) (x2:y2) b k))
                             |otherwise = Null
 removehelp::Cell->[Cell]->[Cell]
 removehelp x []=[]
 removehelp  x (xs:ys)|x==xs=removehelp x ys
                      |otherwise = xs:removehelp x ys
 isfound :: Cell ->[Cell]->Bool
 isfound x []  = False
 isfound x (xs:ys) |x==xs=True
                   |otherwise = isfound x ys
 collect:: MyState -> MyState
 collect (S x (xs:ys) s a)| isfound x (xs:ys) = (S x (removehelp x (xs:ys)) "collect" (S x (xs:ys) s a))
                          | otherwise = (S x (xs:ys) s a)                                               
 nextMyStates::MyState->[MyState]
 nextMyStates (a) = filter (/=Null) ((up a): (left a): (down a): (right a): (collect a) : [])
 isGoal::MyState->Bool 
 isGoal (S (x,y) [] b k) = True
 isGoal (S (x,y) n b k) = False
 search::[MyState]->MyState
 search (x:xs) | isGoal (x) = x
               | otherwise = search (xs++(nextMyStates x))
 constructSolution:: MyState ->[String]
 constructSolution Null = []
 constructSolution (S (x,y) l "" k) = constructSolution k
 constructSolution (S (x,y) l xs k) = (constructSolution k) ++[xs]
 solve :: Cell->[Cell]->[String]
 solve x xs = constructSolution (search (nextMyStates (S x xs "" Null)))
 








