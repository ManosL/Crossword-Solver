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
 
flat_one_level([],[]).      %I made a seperate predicate of flatten because I need this.
flat_one_level([[]|List],NewList) :- flat_one_level(List,NewList).
flat_one_level([[H|L]|List],[H|NewList]) :- flat_one_level([L|List],NewList).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_character(X) :- X >= 97, X =< 122.
is_character(X) :- X >= 65, X =< 90.

%Simple merge sort
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
    X_Length < Y_Length,
    my_merge(List1,[Y|List2],SortedList).

my_merge([X|List1],[Y|List2],[Y|SortedList]) :-
    length(X,X_Length),
    length(Y,Y_Length),
    X_Length > Y_Length,
    my_merge([X|List1],List2,SortedList).

my_merge([X|List1],[Y|List2],[X|SortedList]) :- %When the two lists have equal length
    length(X,X_Length),                         %I check the containg lists of them which
    length(Y,Y_Length),                         %has the greater length(the reason is explained
    X_Length =:= Y_Length,                      %below).
    [HX|_] = X,
    [HY|_] = Y,
    length(HX,LHX),length(HY,LHY), LHX >= LHY,
    my_merge(List1,[Y|List2],SortedList).    

my_merge([X|List1],[Y|List2],[Y|SortedList]) :-
    length(X,X_Length),
    length(Y,Y_Length),
    X_Length =:= Y_Length,
    [HX|_] = X,
    [HY|_] = Y,
    length(HX,LHX),length(HY,LHY), LHX < LHY,
    my_merge([X|List1],List2,SortedList).

create_2D_Array(0,_,[]).

create_2D_Array(Row,Col,Array) :-
    Row > 0,
    length(L,Col),
    Row1 is Row - 1,
    create_2D_Array(Row1,Col,Array1),
    append([L],Array1,Array).

%I give to empty squares the values 0,-1,-2... because these 
%values represent an empty box, to the "black" squares
%I give the value '#'.
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

initialize_words([],[]).

initialize_words([Word|NewWords],[H|Words]) :- %If I want to get the words from list of ASCIIs
    nonvar(H),
    name(Word,H),
    initialize_words(NewWords,Words).

initialize_words([H|Words],[NewWord|NewWords]) :- %If I want to get the ASCIIS of each word
    nonvar(H),
    name(H,NewWord),
    initialize_words(Words,NewWords).

find_proper_word(L,[Word|_],Word) :-  %Finds a proper word of a domain
    length(Word,L2),
    length(L,L1),
    L1 =:= L2,
    is_proper_word(L,Word).

find_proper_word(L,[_|Words],Word) :-
    find_proper_word(L,Words,Word).

is_proper_word([],[]). %Checks if a word is proper

is_proper_word([H|L],[_|Word]) :- %If the domain has empty character is OK because there 
    H =< 0,                       %can be any character at any box.
    is_proper_word(L,Word).

is_proper_word([H|L],[H|Word]) :- %Because the word has only characters if the domain has 
    is_proper_word(L,Word).       %that character for now is proper word

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
print_crossword([]).  %Simple printing function of a 2D Array
print_crossword([H|Crossword]) :-
    print_row(H),nl,
    print_crossword(Crossword).

print_row([]).
print_row([H|Row]) :-
    H \= '#',
    name(X,[H]),write(' '), write(X), write(' '),
    print_row(Row).

print_row(['#'|Row]) :-
    write('###'),print_row(Row).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

crossword(S) :-
    dimension(N),              %getting the dimension of the crossword
    words(X),                  %getting the list of words that must be put to the crosword
    initialize_words(X,WordList),    %Configuring the list of words
    create_2D_Array(N,N,Temp),       
    initialize_crossword(Temp,0,1,Crossword),
    get_domains(Crossword,DomainList),       %Getting the domains of the crossword
    categorize_domains(DomainList,[],CategorizedDL),  %I categorize the domains like: [[N elems domains],[N1 elems domains],...]
    merge_sort(CategorizedDL,SortedCategorizedDL),    %I sort them by length of each category
    flat_one_level(SortedCategorizedDL,SortedDomainList), %I flatten by one element that list to get the sorted domains
    fill_domains(SortedDomainList,[],WordList,NewWordList,FilledDomainList), !, 
    NewWordList = [],                                     %Checking if I filled successfully the crossword
    configure_final_list(FilledDomainList,SortedDomainList,DomainList,FinalList), %I configure the unsorted domain list 
    initialize_words(S,FinalList),                                                %through the empty and filled sorted domain lists
    fill_crossword(Crossword,FinalList,NewCrossword),           %filling the crossword
    print_crossword(NewCrossword).                              %printing it

is_full_domain([]).           %Checks if a domain is full by checking if 
is_full_domain([H|Domain]) :- %it's element is character
    is_character(H),
    is_full_domain(Domain).

categorize_domains([],SoFar,SoFar).    %Base Case
categorize_domains([H|Domain],SoFar,Categorized) :- 
    categorize_domain(H,SoFar,SoFar1),              %I categorize the domain
    categorize_domains(Domain,SoFar1,Categorized).  %and continues to categorize the rest of them
    
categorize_domain(X,[],[[X]]).         %If I don't find a "category list" I will just make anew category
categorize_domain(X,[H|List],[H|NewList]) :- %If the Element doesn't fit in that category I put 
    [H1|_] = H,length(H1,LH1),length(X,LX),  %that category list into the new list/
    LX \= LH1,
    categorize_domain(X,List,NewList).
    
categorize_domain(X,[H|List],NewList) :-
    [H1|_] = H,length(H1,LH1),length(X,LX), %If the element fits in a category list
    LH1 =:= LX,                             %The new list is List but with the new domain with it
    append(H,[X],NewHead),
    append([NewHead],List,NewList).

get_domains(Crossword,DomainList) :-
    get_rows_domains(Crossword,DL1),  %Getting the horizontal domains
    transpose(Crossword,NewCrossword),
    get_rows_domains(NewCrossword,DL2), %Getting the vertical domains
    append(DL1,DL2,DomainList).
    
get_rows_domains([],[]).
get_rows_domains([H|Crossword],DomainList) :- %gets each row domains
    get_row_domains(H,[],DL1),
    get_rows_domains(Crossword,DL2),
    append(DL1,DL2,DomainList).
        
get_row_domains([],SoFar,[Domain]) :- %If the list is empty and the SoFar has more than 
    length(SoFar,LSF), LSF > 1,       %1 element can be considered as domain
    reverse(SoFar,Domain).
        
get_row_domains([],SoFar,[]) :-     %The domains have more than 1 element
    (length(SoFar,0) ; length(SoFar,1)).
        
get_row_domains(['#'|Row],SoFar,DomainList) :- %If I found a black box and SoFar got more than 1 
    length(SoFar,LSF),                         %element this can be considered as a domain
    LSF > 1,
    reverse(SoFar,Domain),
    get_row_domains(Row,[],DL1),
    append([Domain],DL1,DomainList).
        
get_row_domains(['#'|Row],SoFar,DomainList) :- %The opposite of above
    (length(SoFar,0) ; length(SoFar,1)),
    get_row_domains(Row,[],DomainList).
        
get_row_domains([H|Row],SoFar,DomainList) :-  %If I face an empty box I just change the SoFar
    H \= '#',                                 %and calling again the function.
    get_row_domains(Row,[H|SoFar],DomainList).
    

fill_domains([],SoFar,WordList,WordList,SoFar).       %Base Case
fill_domains([H|DomainList],SoFar,WordList,NewWordList,FilledDomainList) :- %If the domain is
    is_full_domain(H),                                                      %full I check if the word
    member(H,WordList),                                                     %which is created belongs to 
    delete(H,WordList,NWL),                                                 %Word list and if belongs I 
    append(SoFar,[H],SoFar1),                                               %go to the next domain
    fill_domains(DomainList,SoFar1,NWL,NewWordList,FilledDomainList).
    
fill_domains([H|DomainList],SoFar,WordList,NewWordList,FilledDomainList) :-
    \+ is_full_domain(H),                                           %If the domain isn't full
    find_proper_word(H,WordList,Word),                              %I find a proper word
    delete(Word,WordList,NWL),                                      %and I fill the crossings 
    fill_crossings([Word|DomainList],Word,H,[],NewList),            %who may exist
    append(SoFar,[Word],SoFar1),                                    %and I configure the SoFar list
    [_|NewList1] = NewList,                                         %and the list which may the recursion
    fill_domains(NewList1,SoFar1,NWL,NewWordList,FilledDomainList). %continues because the next domains
                                                                    %might be different because some crossings were filled

fill_crossings([],_,_,SoFar,SoFar).    %Base Case , returns the new domain list
fill_crossings([H|DomainList],NewWord,OldDomain,SoFar,NewDomainList) :- 
    has_crossing(H,OldDomain,X),                %Finds the letter(X) that is crossing with the OldDomain
    find_letter(X,OldDomain,NewWord,Letter),    %Finds the respective letter at new word of the old domain
    replace(H,X,Letter,NewDomain),
    append(SoFar,[NewDomain],SoFar1),           %put new domain at so Far List
    fill_crossings(DomainList,NewWord,OldDomain,SoFar1,NewDomainList).
    
fill_crossings([H|DomainList],NewWord,OldDomain,SoFar,NewDomainList) :- %If the domain don't have crossing with the OldDomain
    \+ has_crossing(H,OldDomain,_),  %I just put at SoFar List the Domain and continue the recursion
    append(SoFar,[H],SoFar1),
    fill_crossings(DomainList,NewWord,OldDomain,SoFar1,NewDomainList).
    
has_crossing(List1,List2,X) :- member(X,List1), member(X,List2), X =< 0.  %I want the crossing 
                                                                        %to be a blank box

find_letter(X,[X|_],[H|_],H).     %It's pretty simple
find_letter(X,[H1|OldDomain],[_|Word],Letter) :-
    H1 \= X,
    find_letter(X,OldDomain,Word,Letter).

fill_crossword(Crossword,WordList,NewCrossword) :-      %I explained the steps above(the same as get_domains)
    fill_rows(Crossword,WordList,NewWordList,Crossword1),
    transpose(Crossword1,Crossword2),
    fill_rows(Crossword2,NewWordList,_,Crossword3),
    transpose(Crossword3,NewCrossword).

fill_rows([],WordList,WordList,[]).    %It's pretty simple 
fill_rows([H|Rows],WordList,NewWordList,[FilledRow|NewList]) :-
    fill_row(H,WordList,NWL,[],FilledRow),
    fill_rows(Rows,NWL,NewWordList,NewList).

fill_row([],WordList,WordList,SoFar,FilledRow) :- %If SoFar has 0 or 1 elements and I reach the end of a row 
                                                  %I don't have a domain
    (length(SoFar,0);length(SoFar,1)),
    append([],SoFar,FilledRow).

fill_row([],[H|WordList],WordList,SoFar,FilledRow) :-  %The opposite as above
    length(SoFar,LSF), LSF > 1,
    append([],H,FilledRow).

fill_row(['#'|Row],WordList,NewWordList,SoFar,FilledRow) :- %The same as above with only difference
    (length(SoFar,0) ; length(SoFar,1)),                    %that I reached a black box and I give to SoFar
    append(SoFar,['#'],SF),                                 %an '#'.
    fill_row(Row,WordList,NewWordList,[],FL1),
    append(SF,FL1,FilledRow).

fill_row(['#'|Row],[H|WordList],NewWordList,SoFar,FilledRow) :-
    length(SoFar,LSF), LSF > 1,
    append(H,['#'],SF),
    fill_row(Row,WordList,NewWordList,[],FL1),
    append(SF,FL1,FilledRow).

fill_row([H|Row],WordList,NewWordList,SoFar,FilledRow) :- %If H is not a black box I continue the recursion
    H \= '#',
    append(SoFar,[H],SoFar1),
    fill_row(Row,WordList,NewWordList,SoFar1,FilledRow).

configure_final_list([],[],FinalList,FinalList). %I just know the old domain and the new and I replace it to original list
configure_final_list([H1|FilledDL],[H2|EmptyDL],EmptyOriginalDL,FinalList) :-
  replace(EmptyOriginalDL,H2,H1,OriginalDL1),
  configure_final_list(FilledDL,EmptyDL,OriginalDL1,FinalList).