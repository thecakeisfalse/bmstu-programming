-module(test).
-export([main/0]).

read_lines(Filename, BufSize) ->
    {ok, File} = file:open(Filename, [read]),
    {ok, Txt} = file:read(File, BufSize),
    Trimmed = string:trim(Txt),
    string:split(Trimmed, "\n", all).

read_lines(Filename) ->
    read_lines(Filename, 1024 * 1024).

generate_random_string(MinLen, MaxLen, Alphabet) ->
    Length = MinLen + rand:uniform(MaxLen - MinLen) - 1,
    [
        lists:nth(rand:uniform(length(Alphabet)), Alphabet)
     || _ <- lists:seq(1, Length)
    ].

count(Char, String) ->
    length([C || C <- String, C == Char]).

%% SRS

read_srs(Filename) ->
    Lines = read_lines(Filename),
    Rules = [string:split(Line, " -> ", all) || Line <- Lines],
    [{lists:nth(1, Rule), lists:nth(2, Rule)} || Rule <- Rules].

alphabet(SRS) ->
    sets:from_list(
        lists:foldl(fun(Acc, X) -> Acc ++ X end, "", [L || {L, _} <- SRS])
    ).

alphabet(SRS_1, SRS_2) ->
    sets:to_list(
        sets:union(alphabet(SRS_1), alphabet(SRS_2))
    ).

% order(U, V) ->
%     case {length(U), U} >= {length(V), V} of
%         false -> {U, V};
%         true -> {V, U}
%     end.

find_all(Pattern, String) when length(Pattern) > length(String) -> [];
find_all(Pattern, String) ->
    [
        I
     || I <- lists:seq(1, length(String) - length(Pattern) + 1),
        string:substr(String, I, length(Pattern)) == Pattern
    ].

apply_rule(String, {Left, Right}) ->
    [
        string:substr(String, 1, I - 1) ++ Right ++
            string:substr(String, I + length(Left))
     || I <- find_all(Left, String)
    ].

unique(List) ->
    sets:to_list(sets:from_list(List)).

rewrite(SRS, String) ->
    unique(lists:concat([apply_rule(String, Rule) || Rule <- SRS])).

normal_forms(SRS, String, Lookup) ->
    NewStrings = sets:from_list(rewrite(SRS, String)),
    Intersection = sets:intersection(NewStrings, Lookup),

    case sets:is_empty(Intersection) of
        false ->
            Intersection;
        true ->
            case sets:is_empty(NewStrings) of
                true ->
                    sets:from_list([String]);
                false ->
                    UniqueNewStrings = sets:to_list(NewStrings),
                    sets:union([
                        normal_forms(SRS, NewString, Lookup)
                     || NewString <- UniqueNewStrings
                    ])
            end
    end.

normal_forms(SRS, String) ->
    normal_forms(SRS, String, sets:new()).

random_transform_with_steps(_, String, 0) ->
    String;
random_transform_with_steps(SRS, String, Steps) ->
    case rewrite(SRS, String) of
        [] ->
            String;
        NewStrings ->
            RandomNext = lists:nth(rand:uniform(length(NewStrings)), NewStrings),
            random_transform_with_steps(SRS, RandomNext, Steps - 1)
    end.

random_transform(SRS, String, MaxSteps) ->
    Steps = rand:uniform(MaxSteps),
    random_transform_with_steps(SRS, String, Steps).

can_transform(SRS, Small, Large) ->
    SmallForms = normal_forms(SRS, Small),
    LargeForms = normal_forms(SRS, Large, SmallForms),
    not sets:is_disjoint(SmallForms, LargeForms).

%% Invariants

invariant(1, Before, After) ->
    count('b', Before) >= count('b', After);
invariant(2, Before, After) ->
    count('r', Before) >= count('r', After);
invariant(3, Before, After) ->
    count('d', Before) =< count('d', After);
invariant(4, Before, After) ->
    case lists:last(Before) of
        'b' -> lists:last(After) == 'b';
        _ -> true
    end;
invariant(5, Before, After) ->
    case hd(Before) of
        'd' -> hd(After) == 'd';
        _ -> true
    end.

%% Tests

inspect(fuzz, Word, Transformed, AreEquiv) ->
    io:format("Original word: ~p~n", [Word]),
    io:format("After transformation: ~p~n", [Transformed]),
    io:format("Equivalent: ~p~n", [AreEquiv]);
inspect(meta, Word, Rewrites, Valid) ->
    io:format("Original word: ~p~n", [Word]),
    io:format("Transformations: ~p~n", [Rewrites]),
    io:format("Is invariant valid: ~p~n", [Valid]).

test(_, _, _, All, 0) ->
    All;
test(fuzz, OriginalSRS, NewSRS, All, N) ->
    Alphabet = alphabet(OriginalSRS, NewSRS),
    Word = generate_random_string(10, 30, Alphabet),
    Transformed = random_transform(OriginalSRS, Word, 10),
    AreEquiv = can_transform(NewSRS, Transformed, Word),
    inspect(fuzz, Word, Transformed, AreEquiv),
    test(fuzz, OriginalSRS, NewSRS, All andalso AreEquiv, N - 1);
test(meta, OriginalSRS, NewSRS, All, N) ->
    Alphabet = alphabet(OriginalSRS, NewSRS),
    Word = generate_random_string(10, 30, Alphabet),
    Rewrites = rewrite(NewSRS, Word),
    Valid = lists:all(fun({I, After}) -> invariant(I, Word, After) end, [
        {I, After}
     || I <- lists:seq(1, 5), After <- Rewrites
    ]),
    inspect(meta, Word, Rewrites, Valid),
    test(meta, OriginalSRS, NewSRS, All andalso Valid, N - 1).

test(fuzz, OriginalSRS, NewSRS, TestsCount) ->
    rand:seed(exs1024s, erlang:timestamp()),
    case test(fuzz, OriginalSRS, NewSRS, true, TestsCount) of
        true -> io:format("fuzz: SRS are equivalent~n");
        false -> io:format("fuzz: SRS are not equivalent~n")
    end;
test(meta, OriginalSRS, NewSRS, TestsCount) ->
    rand:seed(exs1024s, erlang:timestamp()),
    case test(meta, OriginalSRS, NewSRS, true, TestsCount) of
        true -> io:format("meta: SRS are equivalent~n");
        false -> io:format("meta: SRS are not equivalent~n")
    end.

main() ->
    OriginalSRS = read_srs("srs.txt"),
    NewSRS = read_srs("new-srs.txt"),
    Args = init:get_plain_arguments(),
    case Args of
        ["fuzz"] ->
            test(fuzz, OriginalSRS, NewSRS, 20);
        ["meta"] ->
            test(meta, OriginalSRS, NewSRS, 20);
        _ ->
            test(fuzz, OriginalSRS, NewSRS, 20),
            test(meta, OriginalSRS, NewSRS, 20)
    end,
    init:stop().

