-module(test).
-export([main/0]).

nfa() ->
    {
        nfa,
        % symbols
        "ab",
        % initials
        [0],
        % finals
        [0, 7],
        % transitions
        #{
            {0, 'a'} => {1, 8},
            {0, 'b'} => {3},
            {1, 'b'} => {2},
            {2, 'a'} => {0},
            {3, 'a'} => {4},
            {4, 'b'} => {5},
            {5, 'a'} => {6},
            {6, 'b'} => {7},
            {7, 'a'} => {7, 8},
            {7, 'b'} => {3},
            {8, 'a'} => {5}
        }
    }.

dfa() ->
    {
        dfa,
        % symbols
        "ab",
        % initials,
        [0],
        % finals
        [0, 7, 8, 9, 10, 11, 12, 15, 16],
        % transitions
        #{
            {0, 'a'} => {1},
            {0, 'b'} => {3},
            {1, 'a'} => {5},
            {1, 'b'} => {2},
            {2, 'a'} => {0},
            {3, 'a'} => {4},
            {4, 'b'} => {5},
            {5, 'a'} => {6},
            {6, 'b'} => {7},
            {7, 'a'} => {8},
            {7, 'b'} => {3},
            {8, 'a'} => {9},
            {8, 'b'} => {3},
            {9, 'a'} => {10},
            {9, 'b'} => {3},
            {10, 'a'} => {10},
            {10, 'b'} => {11},
            {11, 'a'} => {12},
            {11, 'b'} => {3},
            {12, 'a'} => {9},
            {12, 'b'} => {13},
            {13, 'a'} => {14},
            {14, 'b'} => {15},
            {15, 'a'} => {16},
            {15, 'b'} => {3},
            {16, 'a'} => {9},
            {16, 'b'} => {11}
        }
    }.

afa() ->
    {
        afa,
        % prefix nfa
        {
            nfa,
            % symbols
            "ab",
            % initials
            [0],
            % finals
            [0, 1],
            % transitions
            #{
                {0, 'a'} => {1, 2},
                {0, 'b'} => {1},
                {1, 'a'} => {1},
                {1, 'b'} => {1},
                {2, 'b'} => {3},
                {3, 'a'} => {0}
            }
        },
        nfa()
    }.

regexp() ->
    {ok, Pattern} = re:compile("^(aba)*((aa|bab)aba*)*$"),
    Pattern.

extended_regexp() ->
    {ok, Pattern} = re:compile("^((?=.*a)(aba)*((aa|bab)aba*)*)?$"),
    Pattern.

check_regex(regex, Word) ->
    case re:run(Word, regexp()) of
        {match, _} -> true;
        _ -> false
    end;
check_regex(extregex, Word) ->
    case re:run(Word, extended_regexp()) of
        {match, _} -> true;
        _ -> false
    end.

% Random string
generate_word(random, Alphabet, MinLen, MaxLen) ->
    Length = MinLen + rand:uniform(MaxLen - MinLen) - 1,
    [
        lists:nth(rand:uniform(length(Alphabet)), Alphabet)
     || _ <- lists:seq(1, Length)
    ];
% Regex string
generate_word(regex, MaxAba, MaxBlocks, MaxABlock) ->
    CountAba = rand:uniform(MaxAba),
    AbaPrefix = [
        lists:concat(lists:duplicate(CountAba, "aba"))
    ],
    Blocks = [
        lists:concat(
            [
                lists:nth(rand:uniform(2), ["aa", "bab"]),
                "ab",
                lists:concat(lists:duplicate(rand:uniform(MaxABlock), "a"))
            ]
        )
     || _ <- lists:seq(0, rand:uniform(MaxBlocks))
    ],
    Result = lists:concat(AbaPrefix ++ Blocks),
    Result.

check(Word, {afa, Nfa1, Nfa2}) ->
    check(Word, Nfa1) andalso check(Word, Nfa2);
check(Word, Automaton) ->
    {_, _, Initials, Finals, _} = Automaton,
    States = check(Word, Automaton, Initials),
    FinalsSet = sets:from_list(Finals),
    not sets:is_disjoint(States, FinalsSet).

check([Word | End], Automaton, CurrentStates) ->
    {_, _, _, _, Transitions} = Automaton,
    NewStates = [
        NewState
     || State <- CurrentStates,
        Symbol <- [list_to_atom([Word])],
        NewStates <- [maps:get({State, Symbol}, Transitions, undefined)],
        NewStates =/= undefined,
        NewState <- tuple_to_list(NewStates)
    ],
    check(End, Automaton, NewStates);
check([], _, CurrentStates) ->
    sets:from_list(CurrentStates).

test(fuzz, 0, All, MissNfa, MissDfa, MissAfa, MissExtRegex) ->
    {All, MissNfa, MissDfa, MissAfa, MissExtRegex};
test(fuzz, TestCount, All, MissNfa, MissDfa, MissAfa, MissExtRegex) ->
    Percent = rand:uniform(100),
    Word =
        if
            Percent =< 10 -> generate_word(regex, 5, 5, 5);
            true -> generate_word(random, "ab", 3, 25)
        end,

    ResultRegex = check_regex(regex, Word),

    ResultNfa = check(Word, nfa()),
    ResultDfa = check(Word, dfa()),
    ResultAfa = check(Word, afa()),
    ResultExtRegex = check_regex(extregex, Word),

    % io:format("~p~n", [Word]),

    MissNfa_ =
        MissNfa +
            case ResultNfa == ResultRegex of
                true ->
                    0;
                false ->
                    io:format("Nfa missmatch: word = ~p~n", [Word]),
                    1
            end,

    MissDfa_ =
        MissDfa +
            case ResultDfa == ResultRegex of
                true ->
                    0;
                false ->
                    io:format("Dfa missmatch: word = ~p~n", [Word]),
                    1
            end,

    MissAfa_ =
        MissAfa +
            case ResultAfa == ResultRegex of
                true ->
                    0;
                false ->
                    io:format("Afa missmatch: word = ~p~n", [Word]),
                    1
            end,

    MissExtRegex_ =
        MissExtRegex +
            case ResultExtRegex == ResultRegex of
                true ->
                    0;
                false ->
                    io:format("ExtRegex missmatch: word = ~p~n", [Word]),
                    1
            end,

    All_ =
        All andalso
            ResultNfa == ResultRegex andalso
            ResultDfa == ResultRegex andalso
            ResultAfa == ResultRegex andalso
            ResultExtRegex == ResultRegex,

    test(
        fuzz,
        TestCount - 1,
        All_,
        MissNfa_,
        MissDfa_,
        MissAfa_,
        MissExtRegex_
    ).

test(fuzz, TestCount) ->
    {All, MissNfa, MissDfa, MissAfa, MissExtRegex} = test(fuzz, TestCount, true, 0, 0, 0, 0),

    case All of
        true ->
            io:format("fuzz: passed~n");
        false ->
            (MissNfa > 0) andalso
                io:format("MissNfa: ~p~n", [MissNfa]),
            (MissDfa > 0) andalso
                io:format("MissDfa: ~p~n", [MissDfa]),
            (MissAfa > 0) andalso
                io:format("MissAfa: ~p~n", [MissAfa]),
            (MissExtRegex > 0) andalso
                io:format("MissExtRegex: ~p~n", [MissExtRegex])
    end.

main() ->
    rand:seed(exs1024s, erlang:timestamp()),
    test(fuzz, 300000),
    init:stop().
