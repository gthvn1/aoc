-module(day01).
-export([main/0, replace_english_digits/1]).

remove_non_digits(String) ->
    lists:filter(fun(Char) -> $0 =< Char andalso Char =< $9  end, String).

% Match string starting by a digit written in english
match_starting_digit(String) ->
    case String of
        [$o, $n, $e | Rest] -> [$1 | Rest];
        [$t, $w, $o | Rest] -> [$2 | Rest];
        [$t, $h, $r, $e, $e | Rest] -> [$3 | Rest];
        [$f, $o, $u, $r | Rest] -> [$4 | Rest];
        [$f, $i, $v, $e | Rest] -> [$5 | Rest];
        [$s, $i, $x | Rest] -> [$6 | Rest];
        [$s, $e, $v, $e, $n | Rest] -> [$7 | Rest];
        [$e, $i, $g, $h, $t | Rest] -> [$8 | Rest];
        [$n, $i, $n, $e | Rest] -> [$9 | Rest];
        [$z, $e, $r, $o | Rest] -> [$0 | Rest];
        _ -> String
    end.

replace_english_digits([]) -> [];
replace_english_digits(String) ->
    [Head | Rest] = match_starting_digit(String),
    % At this points Head is a digit or we don't find any digits.
    % So head is ok and we can recurse on the rest of the string.
    [Head | replace_english_digits(Rest)].

get_first_and_last([]) -> 0;
get_first_and_last([First]) -> (First - $0) * 10 + (First - $0);
get_first_and_last([First|Rest]) when is_list(Rest) ->
    Last = lists:last(Rest),
    (First - $0) * 10 + (Last - $0).

do_calibration(String) ->
    FilteredStr = remove_non_digits(String),
    get_first_and_last(FilteredStr).

part1(Input) ->
    CalibratedList = lists:map(fun(Str) -> do_calibration(Str) end, Input),
    lists:foldl (fun(X, Sum) -> X + Sum end, 0, CalibratedList).

part2(Input) ->
    ReplacedList = lists:map(fun(Str) -> replace_english_digits(Str) end, Input),
    CalibratedList = lists:map(fun(Str) -> do_calibration(Str) end, ReplacedList),
    lists:foldl (fun(X, Sum) -> X + Sum end, 0, CalibratedList).

main() ->
    io:format("Reading input file~n"),
    {ok, Binary} = file:read_file("input/day01"),
    RawInput = binary:bin_to_list(Binary),
    CalibrationValues = string:split(RawInput, "\n", all),
    P1 = part1(CalibrationValues),
    P2 = part2(CalibrationValues),
    io:format("Part 1: ~p~n", [P1]),
    io:format("Part 2: ~p~n", [P2]).