%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).

replace([],_,_,[]).

replace([H|List],Elem1,Elem2,[H|NewList]) :-
    H \= Elem1,
    replace(List,Elem1,Elem2,NewList).

replace([H|List],Elem1,Elem2,[Elem2|NewList]) :-
    H = Elem1,
    replace(List,Elem1,Elem2,NewList).
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_character(X) :- X >= 97, X =< 122.
is_character(X) :- X >= 65, X =< 90.

merge_sort([],[]).

merge_sort([X],[X]).

merge_sort(List,SortedList) :-
    length(List,Length_list),
    Length_list > 1,
    split_list(List,Length_list,1,First_half,Second_half),
    merge_sort(First_half,SortedList1),
    merge_sort(Second_half,SortedList2),
    my_merge(SortedList1,SortedList2,SortedList).

split_list([],_,_,[],[]).

split_list([X|List],Full_length,Curr_Index,[X|First_half],Second_half) :-
    Half_length is Full_length // 2,
    Curr_Index =< Half_length,
    Curr_Index1 is Curr_Index + 1,
    split_list(List,Full_length,Curr_Index1,First_half,Second_half).

split_list([X|List],Full_length,Curr_Index,First_half,[X|Second_half]) :-
    Half_length is Full_length // 2,
    Curr_Index > Half_length,
    Curr_Index1 is Curr_Index + 1,
    split_list(List,Full_length,Curr_Index1,First_half,Second_half).

my_merge([],[],[]) :- !.
my_merge(L,[],L) :- !.
my_merge([],L,L) :- !.

my_merge([X|List1],[Y|List2],[X|SortedList]) :-
    length(X,X_Length),
    length(Y,Y_Length),
    X_Length =< Y_Length,
    my_merge(List1,[Y|List2],SortedList).

my_merge([X|List1],[Y|List2],[Y|SortedList]) :-
    length(X,X_Length),
    length(Y,Y_Length),
    X_Length > Y_Length,
    my_merge([X|List1],List2,SortedList).

create_2D_Array(0,_,[]).

create_2D_Array(Row,Col,Array) :-
    Row > 0,
    length(L,Col),
    Row1 is Row - 1,
    create_2D_Array(Row1,Col,Array1),
    append([L],Array1,Array).

initialize_crossword([],_,_,[]) :- !.

initialize_crossword([H|Array],Curr_Num,Curr_Row,Crossword) :-
    initialize_row(H,Curr_Row,Curr_Num,1,New_num,NewList),
    Row is Curr_Row + 1,
    initialize_crossword(Array,New_num,Row,NewCrossword),
    append([NewList],NewCrossword,Crossword).

initialize_row([],_,Curr_num,_,Curr_num,[]) :- !.

initialize_row([_|List],Row,Curr_num,Curr_Col,New_num,[Curr_num|NewList]) :- 
    \+ black(Row,Curr_Col),
    Curr_num1 is Curr_num - 1,
    Col is Curr_Col + 1,
    initialize_row(List,Row,Curr_num1,Col,New_num,NewList).

initialize_row([_|List],Row,Curr_num,Curr_Col,New_num,['#'|NewList]) :-
    black(Row,Curr_Col),
    Col is Curr_Col + 1,
    initialize_row(List,Row,Curr_num,Col,New_num,NewList).

is_proper_word([],[]).

is_proper_word([H|L],[_|Word]) :-
    H =< 0,
    is_proper_word(L,Word).

is_proper_word([H|L],[H|Word]) :-
    is_proper_word(L,Word).

find_proper_word(L,[Word|_],Word) :-
    length(Word,L2),
    length(L,L1),
    L1 =:= L2,
    is_proper_word(L,Word).

find_proper_word(L,[H|Words],Word) :-
    length(L,L1),
    length(H,L2),
    L2 =< L1,
    find_proper_word(L,Words,Word).

initialize_words([],[]).

initialize_words([Word|NewWords],[H|Words]) :-
    nonvar(H),
    name(Word,H),
    initialize_words(NewWords,Words).

initialize_words([H|Words],[NewWord|NewWords]) :-
    nonvar(H),
    name(H,NewWord),
    initialize_words(Words,NewWords).

crossword(S) :-
    dimension(N),
    words(X),
    initialize_words(X,WordList),
    merge_sort(WordList,SortedWordList),
    create_2D_Array(N,N,Temp),
    initialize_crossword(Temp,0,1,Crossword),
    get_domains(Crossword,DomainList),
    fill_domains(DomainList,DomainList,SortedWordList,NewWordList,FilledDomainList),
    NewWordList = [],
    initialize_words(S,FilledDomainList),
    print_crossword(Crossword,FilledDomainList). %TO WRITE IT

fill_domains(NewDomainList,NewDomainList,WordList,WordList,NewDomainList).

fill_domains([H|DomainList],FullDomainList,WordList,NewWordList,FilledDomainList) :-
    is_full_domain(H),
    words(FullWordList),
    member(H,FullWordList),
    fill_domains(DomainList,FullDomainList,WordList,NewWordList,FilledDomainList).

fill_domains([H|_],FullDomainList,WordList,NewWordList,FilledDomainList) :-
    \+ is_full_domain(H),
    find_proper_word(H,WordList,Word),
    replace(FullDomainList,H,Word,TempDomainList),
    delete(Word,WordList,NWL),
    fill_crossings(TempDomainList,TempDomainList,Word,H,NWL,NewWordList2,[],NewDomainList), %TO WRITE IT
    fill_domains(NewDomainList,NewDomainList,NewWordList2,NewWordList,FilledDomainList).

fill_crossings([],_,_,_,WL,WL,SoFar,SoFar).
fill_crossings([H|DomainList],FullList,NewWord,OldDomain,WordList,NewWordList,SoFar,NewDomainList) :-
    has_crossing(H,OldDomain,X), 
    find_letter(X,OldDomain,NewWord,Letter), %TO WRITE IT
    replace(H,X,Letter,NewDomain),
    find_proper_word(NewDomain,WordList,NewWord1),
    delete(NewWord1,WordList,NWL),
    append(SoFar,[NewWord1],SoFar1),
    fill_crossings(DomainList,FullList,NewWord,OldDomain,NWL,NWL1,SoFar1,NewDomainList1),
    fill_crossings(NewDomainList1,NewDomainList1,NewWord1,NewDomain,NWL1,NewWordList,[],NewDomainList).

fill_crossings([H|DomainList],FullList,NewWord,OldDomain,WordList,NewWordList,SoFar,NewDomainList) :-
    \+ has_crossing(H,OldDomain,_),
    append(SoFar,[H],SoFar1),
    fill_crossings(DomainList,FullList,NewWord,OldDomain,WordList,NewWordList,SoFar1,NewDomainList).

has_crossing(List1,List2,X) :- member(X,List1), member(X,List2), X =< 0.  %I want the crossing 
                                                                        %to be a blank box

find_letter(X,[X|_],[H|_],H).
find_letter(X,[H1|OldDomain],[_|Word],Letter) :-
    H1 \= X,
    find_letter(X,OldDomain,Word,Letter).

is_full_domain([]).
is_full_domain([H|Domain]) :-
    is_character(H),
    is_full_domain(Domain).

get_domains(Crossword,DomainList) :-
    get_rows_domains(Crossword,DL1),
    transpose(Crossword,NewCrossword),
    get_rows_domains(NewCrossword,DL2),
    append(DL1,DL2,DomainList).

get_rows_domains([],[]).

get_rows_domains([H|Crossword],DomainList) :-
    get_row_domains(H,[],DL1),
    get_rows_domains(Crossword,DL2),
    append(DL1,DL2,DomainList).

get_row_domains([],SoFar,[Domain]) :-
    length(SoFar,LSF), LSF > 1,
    reverse(SoFar,Domain).

get_row_domains([],SoFar,[]) :-
    (length(SoFar,0) ; length(SoFar,1)).

get_row_domains(['#'|Row],SoFar,DomainList) :-
    length(SoFar,LSF),
    LSF > 1,
    reverse(SoFar,Domain),
    get_row_domains(Row,[],DL1),
    append([Domain],DL1,DomainList).

get_row_domains(['#'|Row],SoFar,DomainList) :-
    (length(SoFar,0) ; length(SoFar,1)),
    get_row_domains(Row,[],DomainList).

get_row_domains([H|Row],SoFar,DomainList) :-
    H \= '#',
    get_row_domains(Row,[H|SoFar],DomainList).