
match(N,[Pv,Sv,Av],Iv) :- findall(_, clause( row([Pv, Sv, Av], Iv) , _), Rs), length(Rs,N).

feature(FCs, Fs) :- pairs_keys(FCs, Fs).
nth1_feature(N, Fs) :- nth1_featureclass(N, FCs), pairs_keys(FCs, Fs).
classes(FCs, Cs) :- pairs_keys(FCs, Clist), list_to_ord_set(Clist, Cs).

nth1_featureclass(N, FCs) :-
	findall(F-C, clause(db(F, C), _), Dbs),
	maplist(nth1_FC(N), Dbs, FCs).

nth1_FC(N, Fs-C, F-C) :-
	nth1(N, Fs, F).

nth1_boundaries(N, N-Boundaries) :-
	nth1_feature(N, Flist), 
	list_to_ord_set(Flist, Features),
	boundaries(Features, Boundaries).	

nth1_feature_boundaries(Boundaries) :-
	findall(Bages, nth1_boundaries(_, Bages), Boundaries).	

africount(L, A) :- (L > 1 -> A = many; A = one).

boundaries([], _).
boundaries(Vs, Bs) :-
	length(Vs, L),
	africount(L, A),
	boundaries_(A, Vs, [], Bs).

boundaries_(none, _, As, Bs) :- reverse(As, Bs).
boundaries_(one, _, As, Bs) :- boundaries_(none, [], As, Bs).
boundaries_(many, [V0,V1|Vs], As, Bs) :-
	V is (V0+V1)/2,
	length(Vs, L),
	africount(L+1, A),
	boundaries_(A, [V1|Vs], [V|As], Bs).

bisect_on_boundary([F-C|FCs], Boundary, Boundary-Bisected) :-
	bisect_on_boundary_([F-C|FCs], Boundary, [], Bisected).

bisect_on_boundary_([], _, Bisected, Bisected).

bisect_on_boundary_([F-C|FCs], Boundary, Accum, Bisected) :-
	(F < Boundary -> V = 0; V = 1),
	bisect_on_boundary_(FCs, Boundary, [V-C|Accum], Bisected).
	
bisect_along_boundaries(FeatureN, BoundaryN, Bisection) :-
	nth1_feature_boundaries(FBs), 
	nth1(FeatureN, FBs, FeatureN-Bs), 
	nth1_featureclass(FeatureN, FCs), 
	nth1(BoundaryN, Bs, Boundary), 
	bisect_on_boundary(FCs, Boundary, Bisection).



prob(F, C, P) :-
	match(All, F, _),
	match(Count, F, C),
	P is Count / All.

fpair(F, PCs) :-
	classes(Cs),
 	maplist(prob(F), Cs, Ps), 
	zipp(Cs, Ps, PCs).	

zipp([], [], []).
zipp([A|As], [B|Bs], [A-B|Ps]) :- zipp(As, Bs, Ps).

fplus(F1, F2, Fp) :- Fp is F1 + F2.

sum(Ls, S) :- foldl(fplus, Ls, 0, S).

square(S, SS) :- SS is *(S, S).

oneminus(S, G) :- 
	G is +(1, *(-1, S)),
	true.

giniLeaf(F, G) :- 
	classes(Cs),
	maplist(prob(F), Cs, Ps),
	maplist(square, Ps, PPs),
	sum(PPs, Sm),
	oneminus(G, Sm).

giniWeight(F, W) :-
	match(All, _, _),
	match(Count, F, _), 
	W is Count / All.

giniWeightedLeaf(F, WL) :-
	giniLeaf(F, L),
	giniWeight(F, W),
	WL is W * L.

giniImpurity(Fs, Gi) :-
	maplist(giniWeightedLeaf, Fs, WLs),
	sum(WLs, Gi).


