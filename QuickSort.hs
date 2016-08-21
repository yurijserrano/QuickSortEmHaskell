-- Faça o QuickSort em Haskell


pri(a,_) = a
seg(_,a) = a

qs[] = []
qs[a] = [a]
qs(p:x) = (qs lmen) ++ [p] ++ (qs lmai)
	where
		lmen = pri tupla
		lmai = seg tupla
		tupla = partit p x

partit p lst = part p lst [] []
part p [] lmen lmai = (lmen,lmai)
part p (a:x) lmen lmai
		|a<p = part p x (a:lmen) lmai
		|otherwise = part p x lmen (a:lmai)