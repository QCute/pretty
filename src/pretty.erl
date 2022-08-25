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
-export([format/2]).
-ifndef(PRETTY_COLOR).
-define(PRETTY_COLOR, #{
    provider => color,
    arrow => red,
    equal => red,
    boolean => red,
    undefined => yellow,
    atom => magenta,
    integer => magenta,
    float => magenta,
    list => green,
    tuple => blue,
    record => blue,
    maps => blue,
    binary => yellow,
    function => magenta,
    pid => magenta,
    ref => magenta,
    port => magenta
}).
-endif.

%% @doc custom color provider configure
-type configure() :: #{
    provider => color,
    arrow => red,
    equal => red,
    boolean => red,
    undefined => yellow,
    atom => magenta,
    integer => magenta,
    float => magenta,
    list => green,
    tuple => blue,
    record => blue,
    maps => blue,
    binary => yellow,
    function => magenta,
    pid => magenta,
    ref => magenta,
    port => magenta
}.
-export_type([configure/0]).
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
    color(maps, "#{}");
format(Map, Depth) when is_map(Map) ->
    BeginPadding = lists:duplicate(Depth * 4, " "),
    EndPadding = lists:duplicate((Depth - 1) * 4, " "),
    StringList = [[format(Name, Depth), color(arrow, " => "), format(Term, Depth + 1)] || {Name, Term} <- maps:to_list(Map)],
    String = lists:concat([color(maps, "#"), color(maps, "{"), string:join(StringList, ", "), color(maps, "}")]),
    case len(String, 0) < columns() of
        true ->
            String;
        false ->
            lists:concat([color(maps, "#"), color(maps, "{"), "\n", BeginPadding, string:join(StringList, ",\n" ++ BeginPadding), "\n", EndPadding, color(maps, "}")])
    end;
format({}, _) ->
    color(tuple, "{}");
format(Tuple, Depth) when is_tuple(Tuple) ->
    BeginPadding = lists:duplicate(Depth * 4, " "),
    EndPadding = lists:duplicate((Depth - 1) * 4, " "),
    %% @doc read beam record
    Tag = element(1, Tuple),
    Tab = hd(lists:append([Tab || Tab <- ets:all(), ets:info(Tab, name) == shell_records], [undefined])),
    case catch ets:lookup(Tab, Tag) of
        [{_, {_, _, record, {_, Fields}}}] when length(Fields) == tuple_size(Tuple) - 1 ->
            Names = lists:map(fun CollectName({typed_record_field, Field, _}) -> CollectName(Field); CollectName({record_field, _, {_, _, Name}}) -> Name; CollectName({record_field, _, {_, _, Name}, _}) -> Name end, Fields),
            StringList = lists:zipwith(fun(Name, Term) -> [atom_to_list(Name), color(equal, " = "), format(Term, Depth + 1)] end, Names, tl(tuple_to_list(Tuple))),
            String = lists:concat([color(tuple, "#"), color(atom, atom_to_list(Tag)), color(tuple, "{"), string:join(StringList, ", "), color(tuple, "}")]),
            case len(String, 0) < columns() of
                true ->
                    String;
                false ->
                    lists:concat([color(tuple, "#"), color(atom, atom_to_list(Tag)), color(tuple, "{"), "\n", BeginPadding, string:join(StringList, ",\n" ++ BeginPadding), "\n", EndPadding, color(tuple, "}")])
            end;
        _ ->
            StringList = [format(Term, Depth + 1) || Term <- tuple_to_list(Tuple)],
            String = lists:concat([color(tuple, "{"), string:join(StringList, ", "), color(tuple, "}")]),
            case len(String, 0) < columns() of
                true ->
                    String;
                false ->
                    lists:concat([color(tuple, "{"), "\n", BeginPadding, string:join(StringList, ",\n" ++ BeginPadding), "\n", EndPadding, color(tuple, "}")])
            end
    end;
format([], _) ->
    color(list, "[]");
format(List, Depth) when is_list(List) ->
    case io_lib:printable_unicode_list(List) of
        true ->
            [color(binary, "\""), color(binary, List), color(binary, "\"")];
        false ->
            BeginPadding = lists:duplicate(Depth * 4, " "),
            EndPadding = lists:duplicate((Depth - 1) * 4, " "),
            StringList = [format(Term, Depth + 1) || Term <- List],
            String = lists:concat([color(list, "["), string:join(StringList, ", "), color(list, "]")]),
            case len(String, 0) < columns() of
                true ->
                    String;
                false ->
                    lists:concat([color(list, "["), "\n", BeginPadding, string:join(StringList, ",\n" ++ BeginPadding), "\n", EndPadding, color(list, "]")])
            end
    end;
format(Binary, _) when is_binary(Binary) ->
    case unicode:characters_to_list(Binary) of
        List when is_list(List) andalso byte_size(Binary) == length(List) ->
            case io_lib:printable_unicode_list(List) of
                true ->
                    ["<<", color(binary, "\""), color(binary, List), color(binary, "\""), ">>"];
                false ->
                    ["<<", string:join([integer_to_list(Byte) || <<Byte>> <= Binary], ","), ">>"]
            end;
        List when is_list(List) ->
            case io_lib:printable_unicode_list(List) of
                true ->
                    ["<<", color(binary, "\""), color(binary, List), color(binary, "\""), "/", color(atom, "utf8"), ">>"];
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
    color(integer, integer_to_list(Integer));
format(Float, _) when is_float(Float) ->
    %% loop check safety
    color(float, io_lib_format:fwrite_g(Float));
format(undefined, _) ->
    color(undefined, "undefined");
format(Atom, _) when is_boolean(Atom) ->
    List = atom_to_list(Atom),
    color(boolean, List);
format(Atom, _) when is_atom(Atom) ->
    List = atom_to_list(Atom),
    %% is pure alpha number
    case lists:foldl(fun(C, start) -> ($a =< C andalso C =< $z) orelse C == $_; (C, F) -> (($a =< C andalso C =< $z) orelse ($0 =< C andalso C =< $9) orelse C == $_) andalso F end, start, List) of
        true ->
            color(atom, List);
        false ->
            color(atom, ["'", List, "'"])
    end;
format(Fun, _) when is_function(Fun) ->
    color(function, erlang:fun_to_list(Fun));
format(Pid, _) when is_pid(Pid) ->
    color(pid, erlang:pid_to_list(Pid));
format(Ref, _) when is_reference(Ref) ->
    color(ref ,ref_to_list(Ref));
format(Port, _) when is_port(Port) ->
    color(port, erlang:port_to_list(Port)).

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

%% color
color(Type, Chars) ->
    case ?PRETTY_COLOR of
        #{provider := Provider, Type := Color} ->
            Provider:Color(Chars);
        _ ->
            Chars
    end.
