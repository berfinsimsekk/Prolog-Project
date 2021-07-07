% berfin simsek
% 2018400009
% compiling: yes
% complete: yes
% include the knowledge base
:- ['load.pro'].
:- encoding(utf8).

%subtracts and squares two lists
sub_sq_list([], [] , []).
sub_sq_list([H1|T1],[H2|T2], [Head|Tail]):-
    sub_sq_list(T1,T2,Tail),
    (( H1 \= -1 ,Head is (H1-H2)*(H1-H2));
    ( H1 = -1 , Head is 0)).

%sums the list
add_elements([], 0).
add_elements([H|T], R):-
    add_elements(T,Sum),
    R is H + Sum.

%multiplies two list
mul_lists([], [], []).
mul_lists([H1|T1], [H2|T2], [Head|Tail]) :-
    mul_lists(T1,  T2, Tail),
    Head is H1*H2.

%finds the current city of a glanian
current_city(Name, City):-
    (city(X,List,_), member(Name, List)),
    City = [X].

%concatenates two list without duplicates
concatenate([], List, List).
concatenate([H1|T1], List2, List3):-
    member( H1,List2), !,
   concatenate(T1, List2, List3).
concatenate([H1|T1],List2,[H1|Tail]):-
    concatenate(T1,List2,Tail).

%finds common elements and return them as a list
common([],_,[]).
common([H1|T1],List2,X):-
    X=[H1|Tail],
    ( member(H1,List2),common(T1,List2,Tail)  );
    common(T1,List2,X).

%finds the targets with expected gender
target_list(ExpGen, List):-
        findall(X,( glanian(X, G, _), Gen = G, member(Gen, ExpGen)),List).

%generates a distance list to a glanian
distance_list(_,[],[]).
distance_list(Name, [H1|T1], [Head|Tail]):-
    glanian_distance(Name,H1,D),
    Head is D,
    distance_list(Name,T1,Tail).

%generates a weighted distance list to a glanian
weighted_distance_list(_,[],[]).
weighted_distance_list(Name, [H1|T1], [Head|Tail]):-
    weighted_glanian_distance(Name,H1,D),
    Head is D,
    weighted_distance_list(Name,T1,Tail).

%takes two lists and generates a dashed list
dashed_list([],[],[]).
dashed_list([H1|T1],[H2|T2],[H1-H2|Tail]):-
    dashed_list(T1,T2,Tail).

%takes a dashed list and divide it into two list
divide_dashed_list([], [], []).
divide_dashed_list([Head|Tail], [HeadFirst|TailFirst], [HeadSecond|TailSecond]) :-
    HeadFirst-HeadSecond = Head,
    divide_dashed_list(Tail, TailFirst, TailSecond).

%checks whether two glanian has an old relation
has_old_relation(Name1,Name2):-
    old_relation(X),member(Name1,X),member(Name2,X).

%checks the tolerance limit
in_tolerance_limit([],_).
in_tolerance_limit(Hlim,Hf):-
    [Min|T]= Hlim,
    [Max|_]=T,
    ( Min < Hf, Max > Hf  ).

% takes limit and feature lists and decide whether the features in the
% limit
tolerance([],[]).
tolerance(Limit,Feature):-
    [H|T]=Limit,
    [Hf|Tf]=Feature,
    in_tolerance_limit(H,Hf),
    tolerance(T,Tf).

%checks whether two list has a common element
has_common([H1|T1],List2):-
    member(H1,List2),!;
    has_common(T1,List2).

% checks whether there is an liked activity in the given city
likedact_incity(City,Name):-
    ( city(City,_,Act), A=Act ),
    ( likes(Name,Likedact,_), Lact=Likedact),
    has_common(Lact,A).

%finds the length of a list
length_of([], 0).
length_of(List, Length) :-
    [_|Tail] = List,
    length_of(Tail, TailLength),
    Length is TailLength+1.

% divide 4 dashed list to 2 seperate list such as A-B-C-D into A-B and
% C-D
fourtotwo([],[],[]).
fourtotwo([Head|T],[L1h|T1],[L2h|T2]):-
    X-Y-Z-W=Head,
    X-Y=L1h,
    Z-W=L2h,
    fourtotwo(T,T1,T2).

%changes the places of the elements of a dashed list A-B to B-A
reversedash([],[]).
reversedash([Head|T],[L1h|T1]):-
    X-Y=Head,
    Y-X=L1h,
    reversedash(T,T1).

%returns the number of common elements of two list
how(List1,List2,Res):-
    findall(X,(member(X,List1),member(X,List2)),L),
    length_of(L,Res).

%whether there are more than 2 conflicting activities
intersection(Name1,Name2):-
    (   dislikes(Name1,D,_,_), DisAc=D ),
    (   likes(Name2,L,_),  LikedAc=L),
    how(DisAc,LikedAc,X),
    X =< 2.

%whether the city is compatible with the given glanians
is_compatible_city(Name,Name2,City):-
   ( dislikes(Name,_,Dc,_), \+member(City,Dc)),
   ( find_possible_cities(Name, C), ((member(City,C),!);likedact_incity(City,Name))),
  (    merge_possible_cities(Name,Name2,Merged),member(City,Merged)).

%finds the targets and distances as a list
target(Name, Postar,  Targets, Distances):-
    findall(Name2, ( member(Name2,Postar), \+has_old_relation(Name,Name2),
    (   glanian(Name2,_, Feature), F= Feature , dislikes(Name,_,_,Limits), L= Limits, tolerance(L,F)),
    intersection(Name,Name2)), Targets),
    weighted_distance_list(Name,Targets,Distances).

%generates a distance list from the average of wgd
bestdist(_,[],[]).
bestdist(Name, [Tar1|T], [Dbest|D]):-
    weighted_glanian_distance(Name,Tar1,Dist1),
    weighted_glanian_distance(Tar1,Name,Dist2),
    Dbest is (Dist1 + Dist2)/2,
    bestdist(Name,T,D).

%finds the targets and distances as a list
bestmatch(Name, Targets, Bestar ,Bestdist):-
    findall(Tar1,
   ( member(Tar1,Targets) , (expects(Tar1, ExpGen,_),glanian(Name,Gender,_),member(Gender,ExpGen)),
    (glanian(Name,_, Feature), F= Feature , dislikes(Tar1,_,_,Limits), L= Limits, tolerance(L,F)),
    intersection(Name,Tar1)), T),
    bestdist(Name,T,D),
    dashed_list(D,T,Dash),keysort(Dash,Sortedl),divide_dashed_list(Sortedl, Bestdist, Bestar).


%generates an activitiy list from the given city
activities(Name,City,ActL):-
    city(City,_,Cact),
    dislikes(Name,Da,_,_),
   ((find_possible_cities(Name,C),member(City,C),findall(Activities,(member(Activities,Cact),\+member(Activities,Da)),ActL),!);
  (likedact_incity(City,Name), findall( Act, (likes(Name,Likedact,_),member(Act,Likedact),member(Act,Cact)), ActL) )).

%generates an activity list for the given glanians
bestactivities(Name,Name2,City,ActL):-
    activities(Name,City,A1),activities(Name2,City,A2),
    findall(Acti, (member(Acti,A1),member(Acti,A2)), ActL).

%generates a city list for the given glanians
cities(Name,Name2,List):-
    findall(C,  ( city(C,_,_), is_compatible_city(Name,Name2,C) ) , List).

%generates a city list for the given glanians
bestcities(Name,Name2,List):-
    findall(C,  (  city(C,_,_), is_compatible_city(Name,Name2,C), is_compatible_city(Name2,Name,C)  ) ,List).

%generates a dashed list from  4 list like A-C-T-D
city_act(_,_,_,[],[]).
city_act(Targetname,Distance,Cityname,[Act1|T],[Act1-Cityname-Targetname-Distance|T2]):-
    city_act(Targetname,Distance,Cityname,T,T2).

% generates a dashed list with all possible activities and cities with
% the distance for the target in the form A-C-T-D
bigmatch(_,_,_,[],[]).
bigmatch(Name,Name2,Dist1,[City1|Tail],[Ht|Tt]):-
    activities(Name,City1,Actlist),
    city_act(Name2,Dist1,City1,Actlist,ACTD),
    Ht=ACTD,
    bigmatch(Name,Name2,Dist1,Tail,Tt).

% generates a dashed list with all possible activities and cities with
% the distance for the target in the form A-C-T-D
bmatch(_,_,_,[],[]).
bmatch(Name,Name2,Dist1,[City1|Tail],[Ht|Tt]):-
    bestactivities(Name,Name2,City1,Actlist),
    city_act(Name2,Dist1,City1,Actlist,ACTD),
    Ht=ACTD,
    bmatch(Name,Name2,Dist1,Tail,Tt).

% after finding A-C-D-T list for every target it sorts A-C-D-T lists
% alphabetically and divide it into C-A and T-D for every target
muko(_,[],[],[],[]).
muko(Name,[Name2|Tarlist],[Dist1|Distail],[Htardis|Ttardis],[Hcitact|Tcitact]):-
    cities(Name,Name2,Citl),
    bigmatch(Name,Name2,Dist1,Citl,Actd),
    append(Actd,ACDT),
    keysort(ACDT,Sorted),fourtotwo(Sorted,AC,TD),reversedash(AC,CA),
    Htardis=TD,
    Hcitact=CA,
    muko(Name,Tarlist,Distail,Ttardis,Tcitact).

% after finding A-C-D-T list for every target it sorts A-C-D-T lists
% alphabetically and divide it into C-A and T-D for every target
bmuko(_,[],[],[],[]).
bmuko(Name,[Name2|Tarlist],[Dist1|Distail],[Htardis|Ttardis],[Hcitact|Tcitact]):-
    bestcities(Name,Name2,Citl),
    bmatch(Name,Name2,Dist1,Citl,Actd),
    append(Actd,ACDT),
    keysort(ACDT,Sorted),fourtotwo(Sorted,AC,TD),reversedash(AC,CA),
    Htardis=TD,
    Hcitact=CA,
    bmuko(Name,Tarlist,Distail,Ttardis,Tcitact).


%glanian_distance:
glanian_distance(Name1, Name2, Distance):-
    ( expects(Name1, _ , X), List1 = X),
    ( glanian(Name2, _ , Y), List2 = Y),
    ( sub_sq_list(List1, List2, D), Result = D ),
    add_elements(Result, Sums),
    Distance is sqrt(Sums).


% 3.2 weighted_glanian_distance(Name1, Name2, Distance):
weighted_glanian_distance(Name1, Name2, Distance):-
    ( expects(Name1, _ , X), List1 = X),
    ( glanian(Name2, _ , Y), List2 = Y),
    ( sub_sq_list(List1, List2, D), Result = D ),
    ( weight(Name1,Z), Wlist = Z),
    ( mul_lists(Result, Wlist , W),  Mult = W ),
    add_elements(Mult, Sums),
    Distance is sqrt(Sums).


% 3.3 find_possible_cities(Name, CityList) 5 points
find_possible_cities(Name, CityList):-
    ( current_city(Name,X), Cur = X),
    ( likes(Name,_,List), Liked = List),
   ( concatenate(Cur,Liked,Res), CityList = Res).


% 3.4 merge_possible_cities(Name1, Name2, MergedCities) 5 points
merge_possible_cities(Name1,Name2, MergedCities):-
    (   find_possible_cities(Name1, City1), List1 = City1 ),
    (   find_possible_cities(Name2, City2), List2 = City2 ),
    (   concatenate(List1, List2, Res), MergedCities = Res).




% 3.5 find_mutual_activities(Name1, Name2, MutualActivities) 5 points
find_mutual_activities(Name1,Name2, MutualActivities):-
    ( likes(Name1, Liked, _ ), List1 = Liked),
    ( likes(Name2, Liked2, _ ), List2 = Liked2),
    common(List1, List2, MutualActivities).


% 3.6 find_possible_targets(Name, Distances, TargetList) 10 points
find_possible_targets(Name, Distances, TargetList):-
    ( expects(Name, ExpGen,_), GenList = ExpGen ),
    ( target_list(GenList, T), Tlist = T ),
    ( distance_list(Name, Tlist, D), Dlist=D ),
    ( dashed_list(Dlist,Tlist,Dash), Dashedl = Dash),
    ( keysort(Dashedl,X), Sortedl = X),
    divide_dashed_list(Sortedl, Distances, TargetList).



% 3.7 find_weighted_targets(Name, Distances, TargetList) 15 points
find_weighted_targets(Name, Distances, TargetList):-
    ( expects(Name, ExpGen,_), GenList = ExpGen ),
    ( target_list(GenList, T), Tlist = T ),
    ( weighted_distance_list(Name, Tlist, D), Dlist=D ),
    ( dashed_list(Dlist,Tlist,Dash), Dashedl = Dash),
    ( keysort(Dashedl,X), Sortedl = X),
     divide_dashed_list(Sortedl, Distances, TargetList).



% 3.8 find_my_best_target(Name, Distances, Activities, Cities, Targets) 20 points
find_my_best_target(Name, Distances, Activities, Cities, Targets):-
    ( find_weighted_targets(Name, _, T), PosTar = T),
    target(Name, PosTar, Targetl, Distancel),
    muko(Name, Targetl,Distancel,Dashedtardis,Dashedcitact),
    append(Dashedtardis,Tardis),append(Dashedcitact,Citact),
    divide_dashed_list(Tardis,Targets,Distances),
    divide_dashed_list(Citact,Cities,Activities).

% 3.9 find_my_best_match(Name, Distances, Activities, Cities, Targets) 25 points
find_my_best_match(Name, Distances, Activities, Cities, Targets):-
    ( find_weighted_targets(Name, _, T), PosTar = T),
     target(Name, PosTar, Targetl, _),
     bestmatch(Name,Targetl,Ta,Di),
     bmuko(Name,Ta,Di,Dashedtardis,Dashedcitact),
    append(Dashedtardis,Tardis),append(Dashedcitact,Citact),
    divide_dashed_list(Tardis,Targets,Distances),
    divide_dashed_list(Citact,Cities,Activities).



