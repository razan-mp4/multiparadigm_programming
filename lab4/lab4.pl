% База фактів
% Приклад чисельного ряду
numeric_series([10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 160, 170, 180, 190, 200]).

% Алфавіт (наприклад, 5 символів)
alphabet([a, b, c, d, e]).

% База правил
% Функція для генерації інтервалів
generate_intervals(N, Max, Intervals) :-
    findall(Interval,
            (between(1, N, I),
             Lower is (I - 1) * Max / N,
             Upper is I * Max / N,
             Interval = (Lower, Upper)),
            Intervals).

% Функція визначення символу для числа
assign_symbol(Number, Intervals, Alphabet, Symbol) :-
    nth1(Index, Intervals, (Lower, Upper)),
    Number > Lower, Number =< Upper,
    nth1(Index, Alphabet, Symbol).

% Мапінг чисельного ряду у лінгвістичний
map_to_symbols([], _, _, []).
map_to_symbols([H|T], Intervals, Alphabet, [Sym|Symbols]) :-
    assign_symbol(H, Intervals, Alphabet, Sym),
    map_to_symbols(T, Intervals, Alphabet, Symbols).

% Оновлення матриці для пари символів
update_matrix(Current, Next, Alphabet, Matrix, UpdatedMatrix) :-
    nth1(Row, Alphabet, Current),
    nth1(Col, Alphabet, Next),
    nth1(Row, Matrix, RowList),
    nth1(Col, RowList, Value),
    NewValue is Value + 1,
    replace(RowList, Col, NewValue, NewRowList),
    replace(Matrix, Row, NewRowList, UpdatedMatrix).

% Заміна елемента в списку
replace([_|T], 1, X, [X|T]).
replace([H|T], I, X, [H|R]) :-
    I > 1,
    NI is I - 1,
    replace(T, NI, X, R).

% Створення матриці з нулів
create_matrix(Rows, Cols, Matrix) :-
    findall(Row, (between(1, Rows, _), length(Row, Cols), maplist(=(0), Row)), Matrix).

% Побудова матриці переходів
build_transition_matrix([_], _, Matrix, Matrix).
build_transition_matrix([H, Next|T], Alphabet, TempMatrix, Matrix) :-
    update_matrix(H, Next, Alphabet, TempMatrix, UpdatedMatrix),
    build_transition_matrix([Next|T], Alphabet, UpdatedMatrix, Matrix).

% Друк матриці
print_matrix([]).
print_matrix([H|T]) :-
    write(H), nl,
    print_matrix(T).

% Виконання перетворення
perform_conversion :-
    numeric_series(Series),
    alphabet(Alphabet),
    length(Alphabet, N),
    max_list(Series, Max),
    generate_intervals(N, Max, Intervals),
    map_to_symbols(Series, Intervals, Alphabet, LinguisticSeries),
    write('Linguistic series: '), write(LinguisticSeries), nl,
    create_matrix(N, N, ZeroMatrix),
    build_transition_matrix(LinguisticSeries, Alphabet, ZeroMatrix, TransitionMatrix),
    write('Transition Matrix:'), nl,
    print_matrix(TransitionMatrix).

% Запуск
:- perform_conversion.
