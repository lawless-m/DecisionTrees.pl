/*

example db using data from Statquest's tutorial video

https://www.youtube.com/watch?v=_L39rN6gz7Y

db([1, 1, 7], 0).
db([1, 0, 12], 0).
db([0, 1, 35], 1).
db([1, 1, 38], 1).
db([1, 0, 50], 0).
db([0, 0, 83], 0).
db([0, 1, 18], 1).

the tree for this dataset

Tree = choice(2, 0.5, choice(3, 12.5, class(0), class(1)), class(0)),
classify(Tree, [1, 2, 15], Class).


*/

classify(class(Class), _, Class).
classify(choice(FeatureN, LessThan, ChoiceL, ChoiceR), Features, Class) :-
	nth1(FeatureN, Features, FVal),
	(FVal < LessThan -> classify(ChoiceL, Features, Class); classify(ChoiceR, Features, Class)).

nth1_feature(N, Fs) :- nth1_featureclass(N, FCs), pairs_keys(FCs, Fs).

nth1_featureclass(N, FCs) :-
	% N is an integer
	% FCs is the list of FeatureValue-ClassValue from the db
	% db must exist
	findall(F-C, clause(db(F, C), _), Dbs),
	maplist(nth1_FC(N), Dbs, FCs).

nth1_FC(N, Fs-C, F-C) :-
	nth1(N, Fs, F).

nth1_boundaries(N, Boundaries) :-
	% N is an integer
	% Boundaries are the numeric values between the features
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

bisect_on_boundary([F-C|FCs], Boundary, Bisected) :-
	bisect_on_boundary_([F-C|FCs], Boundary, [], Bisected).

bisect_on_boundary_([], _, Bisected, Bisected).
bisect_on_boundary_([F-C|FCs], Boundary, Accum, Bisected) :-
	(F < Boundary -> V = lt; V = gt),
	bisect_on_boundary_(FCs, Boundary, [V-C|Accum], Bisected).
	
bisect_giniBoundaries(FeatureN, Lt, WGini) :-
	% FeatureN and BoundaryN indexes
	% Lt-Gini is the Less than value and the Weighted Gini Impurity for that boundary
	nth1_feature_boundaries(FBs), 
	nth1(FeatureN, FBs, Bs), 
	nth1_featureclass(FeatureN, FCs), 
	nth1(_, Bs, Lt), 
	bisect_on_boundary(FCs, Lt, Bisection),
	bisection_weightedGiniImpurity(Bisection, WGini).

pairs_KMinV([K0-V0|KVs], K-MinV) :-
	% A list of Kn-Vn pairs, K is the key of the minimum value MinV
	pairs_KMinV_(KVs, K0, V0, K, MinV).

pairs_KMinV_([], KMinV, MinV, KMinV, MinV).
pairs_KMinV_([K1-V1|KVs], K0, V0, KMinV, MinV) :-
	% Support function for pairs_KMinV
	(V1 < V0 -> pairs_KMinV_(KVs, K1, V1, KMinV, MinV);  pairs_KMinV_(KVs, K0, V0, KMinV, MinV)).

min_wgini(FeatureN, LtMinWGini) :-
	% The Numbered Feature and the lowest LessThan-WeightedGini pair
	findall(Lt-WGini, bisect_giniBoundaries(FeatureN, Lt, WGini), LWs),
	pairs_KMinV(LWs, LtMinWGini).

ltsFn_wGinisFn(Lts, Wginis) :-
	% list of Lt-WGini for each feature
	indexesOfFeatures(FIdxs),
	featureIdxs_Lts_WGinis(FIdxs, Lts, Wginis).

featureIdxs_Lts_WGinis([Fi|FIdxs], Lts, Wginis) :-
	featureIdxs_Lts_WGinis_([Fi|FIdxs], [], [], Lts, Wginis).

featureIdxs_Lts_WGinis_([], LtA, WginisA, Lts, Wginis) :- reverse(LtA, Lts), reverse(WginisA, Wginis).
featureIdxs_Lts_WGinis_([Fi|FIdxs], LtA, WginisA,  Lts, Wginis) :-
	min_wgini(Fi, Lt-GiniMin),
	featureIdxs_Lts_WGinis_(FIdxs, [Lt-Fi|LtA], [GiniMin-Fi|WginisA], Lts, Wginis).
	
indexesOfFeatures(Idxs) :-
	clause(db(Fs, _), _), 
	length(Fs, N),
	indexesOfFeatures_(Fs, [], N, Idxs),
	!.

indexesOfFeatures_([], Idxs, _, Idxs).
indexesOfFeatures_([_|Fs], Idxa, N, Idxs) :-
	Nm1 is N-1,
	indexesOfFeatures_(Fs, [N|Idxa], Nm1, Idxs).

is_class_counts(YesNos, Yes, No) :-
	% given a list of _-C pairs, count the _-1 as Yes, _-0 as No
	length(YesNos, L),
	include(is_class, YesNos, Yess),
	length(Yess, Yes),
	No is L-Yes.

bisection_counts(B, LtYes, LtNo, GtYes, GtNo) :-
	% given a Bisection List of one of lt-1, lt-0, gt-1, gt-0, count each
	include(is_lt, B, Lts),
	is_class_counts(Lts, LtYes, LtNo),
	include(is_gt, B, Gts),
	is_class_counts(Gts, GtYes, GtNo).

probSqrd(All, Pcount, Psqr) :-
	% guards against Div0 and returns 0 in that case
	(All > 0 -> P is (Pcount/All), Psqr is P*P; Psqr is 0).

leafImpurity(All, Yes, No, I) :-
	% unweighted Gini Impurity for one leaf 1 - P(yes)^2 - P(no)^2
	probSqrd(All, Yes, PYes),
	probSqrd(All, No, PNo),
	I is 1 - PYes - PNo.

bisection_weightedGiniImpurity(B, Gi) :-
	% Bisection B, Gini Impurity for Lt leaf and Gt Leaf
	bisection_counts(B, LtYes, LtNo, GtYes, GtNo),
	Lts is LtYes + LtNo,
	leafImpurity(Lts, LtYes, LtNo, LtGi),
	Gts is GtYes + GtNo,
	leafImpurity(Gts, GtYes, GtNo, GtGi),
	LtWeight is (Lts / (Lts + Gts)),
	GtWeight is (Gts / (Lts + Gts)),
	Gi is LtWeight * LtGi + GtWeight * GtGi.

% filter functions 
is_lt(LtGt-_) :- LtGt = lt.
is_gt(LtGt-_) :- LtGt = gt.
is_class(_-C) :- C = 1.

root(choice(Fn, Lt, 0, 0)) :-
	ltsFn_wGinisFn(Lts, Wginis),
	sort(Wginis, [_-Fn|_]),
	nth1(Fn, Lts, Lt-Fn).
	


/*

% no longer used

match(N,[Pv,Sv,Av],Iv) :- findall(_, clause( row([Pv, Sv, Av], Iv) , _), Rs), length(Rs,N).

classes(FCs, Cs) :- pairs_keys(FCs, Clist), list_to_ord_set(Clist, Cs).

feature(FCs, Fs) :- pairs_keys(FCs, Fs).

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


*/
