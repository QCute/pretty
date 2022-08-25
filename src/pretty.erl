%%%-------------------------------------------------------------------
%%% @doc
%%% pretty print
%%% @end
%%%-------------------------------------------------------------------
-module(pretty).
-compile(no_native).
-on_load(on_load/0).
%% API functions
-export([print/1, print/2]).
%%%===================================================================
%%% API
%%%===================================================================
-spec on_load() -> ok.
on_load() ->
    hook:hook(io_lib_pretty, print, 2, ?MODULE),
    ok.

%% @doc pretty print
-spec print(Term :: term()) -> io_lib:chars().
print(Term) ->
    format(Term, 1).

%% @doc the io_lib_pretty print callback
-spec print(Term :: term(), list() | function()) -> io_lib:chars().
print(Term, _) ->
    case erlang:process_info(self(), current_stacktrace) of
        {_, [_, {shell, _, _, _} | _]} ->
            format(Term, 1);
        _ ->
            io_lib_pretty:print(Term)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% format term
format(Map = #{}, _) when map_size(Map) == 0 ->
    color:blue("#{}");
format(Map, Depth) when is_map(Map) ->
    BeginPadding = lists:duplicate(Depth * 4, " "),
    EndPadding = lists:duplicate((Depth - 1) * 4, " "),
    StringList = [[format(Name, Depth), color:red(" => "), format(Term, Depth + 1)] || {Name, Term} <- maps:to_list(Map)],
    String = lists:concat([color:blue("#"), color:blue("{"), string:join(StringList, ", "), color:blue("}")]),
    case len(String, 0) < columns() of
        true ->
            String;
        false ->
            lists:concat([color:blue("#"), color:blue("{"), "\n", BeginPadding, string:join(StringList, ",\n" ++ BeginPadding), "\n", EndPadding, color:blue("}")])
    end;
format({}, _) ->
    color:blue("{}");
format(Tuple, Depth) when is_tuple(Tuple) ->
    BeginPadding = lists:duplicate(Depth * 4, " "),
    EndPadding = lists:duplicate((Depth - 1) * 4, " "),
    %% @doc read beam record
    Tag = element(1, Tuple),
    Tab = hd(lists:append([Tab || Tab <- ets:all(), ets:info(Tab, name) == shell_records], [undefined])),
    case catch ets:lookup(Tab, Tag) of
        [{_, {_, _, record, {_, Fields}}}] when length(Fields) == tuple_size(Tuple) - 1 ->
            StringList = lists:zipwith(fun(Name, Term) -> [atom_to_list(element(3, element(3, Name))), color:red(" = "), format(Term, Depth + 1)] end, Fields, tl(tuple_to_list(Tuple))),
            String = lists:concat([color:blue("#"), color:cyan(atom_to_list(Tag)), color:blue("{"), string:join(StringList, ", "), color:blue("}")]),
            case len(String, 0) < columns() of
                true ->
                    String;
                false ->
                    lists:concat([color:blue("#"), color:cyan(atom_to_list(Tag)), color:blue("{"), "\n", BeginPadding, string:join(StringList, ",\n" ++ BeginPadding), "\n", EndPadding, color:blue("}")])
            end;
        _ ->
            StringList = [format(Term, Depth + 1) || Term <- tuple_to_list(Tuple)],
            String = lists:concat([color:blue("{"), string:join(StringList, ", "), color:blue("}")]),
            case len(String, 0) < columns() of
                true ->
                    String;
                false ->
                    lists:concat([color:blue("{"), "\n", BeginPadding, string:join(StringList, ",\n" ++ BeginPadding), "\n", EndPadding, color:blue("}")])
            end
    end;
format([], _) ->
    color:green("[]");
format(List, Depth) when is_list(List) ->
    case io_lib:printable_unicode_list(List) of
        true ->
            [color:yellow("\""), color:yellow(List), color:yellow("\"")];
        false ->
            BeginPadding = lists:duplicate(Depth * 4, " "),
            EndPadding = lists:duplicate((Depth - 1) * 4, " "),
            StringList = [format(Term, Depth + 1) || Term <- List],
            String = lists:concat([color:green("["), string:join(StringList, ", "), color:green("]")]),
            case len(String, 0) < columns() of
                true ->
                    String;
                false ->
                    lists:concat([color:green("["), "\n", BeginPadding, string:join(StringList, ",\n" ++ BeginPadding), "\n", EndPadding, color:green("]")])
            end
    end;
format(Binary, _) when is_binary(Binary) ->
    case unicode:characters_to_list(Binary) of
        List when is_list(List) andalso byte_size(Binary) == length(List) ->
            case io_lib:printable_unicode_list(List) of
                true ->
                    ["<<", color:yellow("\""), color:yellow(List), color:yellow("\""), ">>"];
                false ->
                    ["<<", string:join([integer_to_list(Byte) || <<Byte>> <= Binary], ","), ">>"]
            end;
        List when is_list(List) ->
            case io_lib:printable_unicode_list(List) of
                true ->
                    ["<<", color:yellow("\""), color:yellow(List), color:yellow("\""), "/", color:magenta("utf8"), ">>"];
                false ->
                    ["<<", string:join([integer_to_list(Byte) || <<Byte>> <= Binary], ","), ">>"]
            end;
        _ ->
            ["<<", string:join([integer_to_list(Byte) || <<Byte>> <= Binary], ","), ">>"]
    end;
format(BitString, _) when is_bitstring(BitString) ->
    BinarySize = (bit_size(BitString) div 8),
    BitSize = (bit_size(BitString) rem 8),
    <<_:BinarySize, Rest:BitSize>> = BitString,
    ["<<", string:join([integer_to_list(Byte) || <<Byte>> <= BitString], ","), integer_to_list(Rest), ":", integer_to_list(BitSize), ">>"];
format(Integer, _) when is_integer(Integer) ->
    color:magenta(integer_to_list(Integer));
format(Float, _) when is_float(Float) ->
    %% loop check safety
    color:magenta(io_lib_format:fwrite_g(Float));
format(Atom, _) when is_atom(Atom) ->
    List = atom_to_list(Atom),
    %% is pure alpha number
    case lists:foldl(fun(C, start) -> ($a =< C andalso C =< $z) orelse C == $_; (C, F) -> (($a =< C andalso C =< $z) orelse ($0 =< C andalso C =< $9)) andalso F end, start, List) of
        true ->
            color:magenta(List);
        false ->
            color:magenta(["'", List, "'"])
    end;
format(Ref, _) when is_reference(Ref) ->
    color:magenta(ref_to_list(Ref));
format(Fun, _) when is_function(Fun) ->
    color:magenta(erlang:fun_to_list(Fun));
format(Pid, _) when is_pid(Pid) ->
    color:magenta(erlang:pid_to_list(Pid));
format(Port, _) when is_port(Port) ->
    color:magenta(erlang:port_to_list(Port)).

%% calculate string len
len([], Len) ->
    Len;
len([["\e[", _, _] | T], Len) ->
    len(T, Len);
len([String | T], Len) when is_list(String) ->
    len(T, len(String, Len));
len([C | T], Len) when is_integer(C), C > 16#FF, C < 16#D800; is_integer(C), C > 16#DFFF, C < 16#FFFE; is_integer(C), C > 16#FFFF, C =< 16#10FFFF ->
    len(T, Len + 2);
len([_ | T], Len) ->
    len(T, Len + 1).

%% get terminal columns
columns() ->
    columns(os:type()).
columns({unix, _}) ->
    element(2, io:columns());
columns({win32, _}) ->
    Data = os:cmd("PowerShell -NoLogo -NonInteractive -NoProfile -Command \"(get-host).ui.rawui.WindowSize | Select Width -ExpandProperty Width\""),
    list_to_integer(string:strip(string:strip(Data, 'both', $\n), 'both', $\r)).
