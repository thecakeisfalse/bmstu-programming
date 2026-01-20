-module(test).
-export([main/0]).

ll1approx() ->
    {
        nfa,
        % symbols
        "ab",
        % initials
        [0],
        % finals
        [6],
        % transitions
        #{
            {4, 'b'} => {5},
            {6, 'a'} => {6},
            {6, 'b'} => {6},
            {4, 'a'} => {4},
            {1, 'a'} => {2},
            {2, 'b'} => {3},
            {5, 'b'} => {5, 3},
            {2, 'a'} => {4},
            {3, 'b'} => {6},
            {0, 'a'} => {1}
        }
    }.

lr0approx() ->
    {
        nfa,
        % symbols
        "ab",
        % initials
        [0],
        % finals
        [6],
        % transitions
        #{
            {3, 'b'} => {4},
            {6, 'a'} => {6},
            {6, 'b'} => {6},
            {3, 'a'} => {3},
            {1, 'a'} => {2},
            {2, 'b'} => {5},
            {4, 'b'} => {4, 5},
            {2, 'a'} => {3},
            {5, 'b'} => {6},
            {0, 'a'} => {1}
        }
    }.

dpda() ->
    {
        dpda,
        % symbols
        "ab",
        % stack symbols
        "AB",
        % initial
        'S',
        % finals
        ['F', 'F_a'],
        % transitionn
        #{
            {'S', 'a', nothing} => {'a', nothing},
            {'a', 'a', nothing} => {'aa', ['A']},
            {'aa', 'b', nothing} => {'b', nothing},
            {'b', 'b', nothing} => {'bb', nothing},
            {'bb', 'b', nothing} => {'bb', nothing},
            {'bb', 'a', nothing} => {'a', nothing},
            {'aa', 'a', 'A'} => {'m', ['B', 'B', 'B']},
            {'m', 'a', nothing} => {'m', ['B']},
            {'m', 'b', 'B'} => {'n', nothing},
            {'n', nothing, 'A'} => {'k', ['A']},
            {'n', nothing, eos} => {'F', [eos]},
            {'n', 'b', 'B'} => {'n', nothing},
            {'k', 'a', nothing} => {'ka', nothing},
            {'k', 'b', nothing} => {'k', nothing},
            {'ka', 'a', nothing} => {'ka', nothing},
            {'ka', 'b', 'A'} => {'kab', nothing},
            {'kab', 'a', 'A'} => {'ka', ['A']},
            {'kab', 'b', 'A'} => {'kab', ['A']},
            {'kab', nothing, eos} => {'F', [eos]},
            {'F', 'b', nothing} => {'F', nothing},
            {'F', 'a', nothing} => {'F_a', nothing},
            {'F_a', 'a', nothing} => {'F_a', nothing}
        }
    }.

check_fa(Word, Automaton) ->
    {_, _, Initials, Finals, _} = Automaton,
    States = check_fa(Word, Automaton, Initials),
    FinalsSet = sets:from_list(Finals),
    not sets:is_disjoint(States, FinalsSet).
check_fa([Word | End], Automaton, CurrentStates) ->
    {_, _, _, _, Transitions} = Automaton,
    NewStates = [
        NewState
     || State <- CurrentStates,
        Symbol <- [list_to_atom([Word])],
        NewStates <- [maps:get({State, Symbol}, Transitions, undefined)],
        NewStates =/= undefined,
        NewState <- tuple_to_list(NewStates)
    ],
    check_fa(End, Automaton, NewStates);
check_fa([], _, CurrentStates) ->
    sets:from_list(CurrentStates).

check_dpda(Word, Automaton) ->
    {_, _, _, Initial, Finals, _} = Automaton,
    Stack = [eos],
    NewState = check_dpda(Word, Automaton, Initial, Stack),
    sets:is_element(NewState, sets:from_list(Finals)).
check_dpda([Word | Last], Automaton, CurrentState, CurrentStack) ->
    {_, _, _, _, _, Transitions} = Automaton,
    {Ok, NewState, NewStack} = next_state(
        list_to_atom([Word]), CurrentState, CurrentStack, Transitions
    ),
    case Ok of
        ok -> check_dpda(Last, Automaton, NewState, NewStack);
        err -> err
    end;
check_dpda([], _, CurrentState, _) ->
    CurrentState.

next_transition(Word, Transitions, CurrentState, CurrentStack) ->
    State = CurrentState,
    Symbol = Word,
    K1 = {State, Symbol, nothing},
    K2 = {State, Symbol, lists:last(CurrentStack)},
    case Transitions of
        #{K1 := Value} ->
            {ok, Value, no};
        #{K2 := Value} ->
            {ok, Value, yes};
        #{} ->
            {err, {dont, care}, Symbol}
    end.

next_state(Symbol, CurrentState, CurrentStack, Transitions) ->
    {Ok, {NewState, Push}, ShouldPop} = next_transition(
        Symbol, Transitions, CurrentState, CurrentStack
    ),
    case Ok of
        ok ->
            DiffStack =
                case ShouldPop of
                    yes -> lists:sublist(CurrentStack, length(CurrentStack) - 1);
                    no -> CurrentStack
                end,
            NewStack =
                case Push of
                    nothing -> DiffStack;
                    _ -> DiffStack ++ Push
                end,
            {Ok2, State3, Stack3} = next_state(nothing, NewState, NewStack, Transitions),
            case Ok2 of
                ok -> {ok, State3, Stack3};
                err -> {ok, NewState, NewStack}
            end;
        err ->
            {err, err_state, err_stack}
    end.

check_ll1(Word) ->
    {Status, Suffix} = check_S(Word),
    case {Status, Suffix} of
        {true, []} -> true;
        _ -> false
    end.

check_S(Word) ->
    {Status1, Suffix1} = check_A(Word),
    case Status1 of
        true -> check_R1_A(Suffix1);
        _ -> {Status1, Suffix1}
    end.

check_A(Word) ->
    case Word of
        [$a, $a | Rest] -> check_A1(Rest);
        _ -> {false, Word}
    end.

check_A1(Word) ->
    case Word of
        [$b | _] ->
            case check_L(Word) of
                {true, Suffix1} ->
                    case check_A(Suffix1) of
                        {true, Suffix2} -> check_R(Suffix2);
                        {false, _} -> {false, Word}
                    end;
                {false, _} ->
                    {false, Word}
            end;
        [$a | _] ->
            case check_M(Word) of
                {true, Suffix1} ->
                    case Suffix1 of
                        [$b | Suffix2] -> check_R_B(Suffix2);
                        _ -> {false, Word}
                    end;
                {false, _} ->
                    {false, Word}
            end;
        _ ->
            {false, Word}
    end.

check_M(Word) ->
    case Word of
        [$a | Rest] -> check_M1(Rest);
        _ -> {false, Word}
    end.

check_M1(Word) ->
    case Word of
        [$b | Rest] ->
            {true, Rest};
        _ ->
            case check_M(Word) of
                {true, Suffix1} ->
                    case Suffix1 of
                        [$b | Rest] -> {true, Rest};
                        _ -> {false, Word}
                    end;
                {false, _} ->
                    {false, Word}
            end
    end.

check_L(Word) ->
    case Word of
        [$b | Rest] -> check_R_B(Rest);
        _ -> {false, Word}
    end.

check_R(Word) ->
    case check_R_A(Word) of
        {true, Suffix1} -> check_R_B(Suffix1);
        {false, _} -> {false, Word}
    end.

check_R_B(Word) ->
    case Word of
        [$b | Rest] -> check_R1_B(Rest);
        _ -> {false, Word}
    end.

check_R1_B(Word) ->
    case Word of
        [$b | Rest] -> check_R1_B(Rest);
        _ -> {true, Word}
    end.

check_R_A(Word) ->
    case Word of
        [$a | Rest] -> check_R1_A(Rest);
        _ -> {false, Word}
    end.

check_R1_A(Word) ->
    case Word of
        [$a | Rest] -> check_R1_A(Rest);
        _ -> {true, Word}
    end.

% Random string
regexp() ->
    {ok, Pattern} = re:compile("^(aabbb*)*$"),
    Pattern.

count_str(String, Sub) ->
    Len = length(Sub),
    count_str(String, Sub, Len).

count_str([], _, _) ->
    0;
count_str(String, Sub, Len) ->
    case string:slice(String, 0, Len) of
        Sub ->
            count_str(string:slice(String, 1), Sub, Len) + 1;
        _ ->
            count_str(string:slice(String, 1), Sub, Len)
    end.

check_set(Word) ->
    lists:any(fun(X) -> X end, [
        check_block(string:slice(Word, N), count_str(string:slice(Word, 0, N), "aa"))
     || N <- lists:seq(0, length(Word)),
        check_w1(string:slice(Word, 0, N))
    ]).

check_w1(Word) ->
    case re:run(Word, regexp()) of
        {match, _} -> true;
        _ -> false
    end.

check_block(Word, CountAA) ->
    K = string:str(Word, "b") - 1,
    if
        K > 2 andalso length(Word) >= 2 * K ->
            AB = string:slice(Word, 0, 2 * K),
            Expected = lists:duplicate(K, $a) ++ lists:duplicate(K, $b),
            case AB == Expected of
                true -> check_w2(string:slice(Word, 2 * K), CountAA);
                false -> false
            end;
        true ->
            false
    end.

check_w2(Word, CountAA) ->
    CountAB = count_str(Word, "ab"),
    CountAA == CountAB.

generate_word(random, Alphabet, MinLen, MaxLen) ->
    Length = MinLen + rand:uniform(MaxLen - MinLen) - 1,
    [
        lists:nth(rand:uniform(length(Alphabet)), Alphabet)
     || _ <- lists:seq(1, Length)
    ].

test(fuzz, 0, All, MissLL1Approx, MissLR0Approx, MissDpda, MissLL1) ->
    {All, MissLL1Approx, MissLR0Approx, MissDpda, MissLL1};
test(fuzz, TestCount, All, MissLL1Approx, MissLR0Approx, MissDpda, MissLL1) ->
    % Percent = rand:uniform(100),
    Word = generate_word(random, "ab", 3, 100),

    ValidLL1Approx = check_fa(Word, ll1approx()),
    ValidLR0Approx = check_fa(Word, lr0approx()),
    ValidDpda = check_dpda(Word, dpda()),
    ValidLL1 = check_ll1(Word),
    ValidSet = check_set(Word),

    MissLL1Approx_ =
        MissLL1Approx +
            % A -> B
            case (not ValidSet) or ValidLL1Approx of
                true ->
                    0;
                false ->
                    io:format("LL1 Approx missmatch: word = ~p~n", [Word]),
                    1
            end,

    MissLR0Approx_ =
        MissLR0Approx +
            case (not ValidSet) or ValidLR0Approx of
                true ->
                    0;
                false ->
                    io:format("LR0 Approx missmatch: word = ~p~n", [Word]),
                    1
            end,

    MissDpda_ =
        MissDpda +
            case ValidDpda == ValidSet of
                true ->
                    0;
                false ->
                    io:format("DPDA missmatch: word = ~p~n", [Word]),
                    1
            end,

    MissLL1_ =
        MissLL1 +
            case ValidLL1 == ValidSet of
                true ->
                    0;
                false ->
                    io:format("LL1 missmatch: word = ~p~n", [Word]),
                    1
            end,

    All_ =
        All andalso
            ((not ValidSet) or ValidLL1Approx) andalso
            ((not ValidSet) or ValidLR0Approx) andalso
            ValidSet == ValidLL1 andalso
            ValidSet == ValidDpda,

    test(
        fuzz,
        TestCount - 1,
        All_,
        MissLL1Approx_,
        MissLR0Approx_,
        MissDpda_,
        MissLL1_
    ).

test(fuzz, TestCount) ->
    {All, MissLL1Approx, MissLR0Approx, MissDpda, MissLL1} = test(
        fuzz, TestCount, true, 0, 0, 0, 0
    ),

    case All of
        true ->
            io:format("fuzz: passed~n");
        false ->
            (MissLL1Approx > 0) andalso
                io:format("MissLL1Approx: ~p~n", [MissLL1Approx]),
            (MissLR0Approx > 0) andalso
                io:format("MissLR0Approx: ~p~n", [MissLR0Approx]),
            (MissDpda > 0) andalso
                io:format("MissDpda: ~p~n", [MissDpda]),
            (MissLL1 > 0) andalso
                io:format("MissLL1: ~p~n", [MissLL1])
    end.

main() ->
    rand:seed(exs1024s, erlang:timestamp()),
    test(fuzz, 300000),
    init:stop().
