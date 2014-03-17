%%%-------------------------------------------------------------------
%%% @author Niclas Axelsson <niclas@burbasconsulting.com>
%%% @copyright (C) 2014, Niclas Axelsson
%%% @doc
%%% Test suite for erlstripe
%%% @end
%%% Created : 17 Mar 2014 by Niclas Axelsson <niclas@burbasconsulting.com>
%%%-------------------------------------------------------------------
-module(erlstripe_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-define(STRIPE_KEY, ""). %% Use your TEST-PRIVATE KEY HERE

%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
    application:set_env(erlstripe, stripe_key, ?STRIPE_KEY),
    application:start(inets),
    application:start(asn1),
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    [{timetrap,{seconds,30}}].

%%--------------------------------------------------------------------
%% @spec init_per_suite(Config0) ->
%%     Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> void() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_group(GroupName, Config0) ->
%%               void() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%% @end
%%--------------------------------------------------------------------
groups() ->
    [{customer, [], [
                     create_customer,
                     get_customer,
                     update_customer,
                     delete_customer
                    ]},
     {card, [], [
                 create_customer,
                 create_card,
                 update_card,
                 get_card_with_name,
                 delete_card,
                 delete_customer
                ]}
    ].


%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() ->
    [{group, customer}, {group, card}].

%%--------------------------------------------------------------------
%% @spec TestCase() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
my_test_case() ->
    [].

%%--------------------------------------------------------------------
%% @spec TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%% @end
%%--------------------------------------------------------------------
create_customer(Config) ->
    {ok, Result} = erlstripe:create_customer([{"description", "Test customer for erlstripe"},
                                              {"email", "test@erlstripe.io"}]),
    {save_config, [{custom_id, proplists:get_value(<<"id">>, Result)}]}.

get_customer(Config) ->
    {_, PropConfig} = proplists:get_value(saved_config, Config),
    Id = proplists:get_value(custom_id, PropConfig),
    {ok, _Res} = erlstripe:get_customer(Id),
    {save_config, [{custom_id, Id}]}.

update_customer(Config) ->
    io:format("~p~n", [Config]),
    {_, PropConfig} = proplists:get_value(saved_config, Config),
    Id = proplists:get_value(custom_id, PropConfig),
    {ok, Result} = erlstripe:update_customer(Id, [{"email", "test_update@erlstripe.io"}]),
    <<"test_update@erlstripe.io">> = proplists:get_value(<<"email">>, Result),
    {save_config, [{custom_id, Id}]}.

delete_customer(Config) ->
    {_, PropConfig} = proplists:get_value(saved_config, Config),
    Id = proplists:get_value(custom_id, PropConfig),
    {ok, _Res} = erlstripe:delete_customer(Id).


create_card(Config) ->
    {_, PropConfig} = proplists:get_value(saved_config, Config),
    Id = proplists:get_value(custom_id, PropConfig),
    {ok, CardProp} = erlstripe:create_card(Id, [{"card", [{"number", "4242424242424242"},
                                                          {"exp_month", "01"},
                                                          {"exp_year", "2015"},
                                                          {"cvc", "612"}]}]),
    {save_config, [{custom_id, Id}, {card_id, proplists:get_value(<<"id">>, CardProp)}]}.

get_card(Config) ->
    {_, PropConfig} = proplists:get_value(saved_config, Config),
    CardId = proplists:get_value(card_id, PropConfig),
    CustomerId = proplists:get_value(custom_id, PropConfig),
    {ok, _} = erlstripe:get_card(CustomerId, CardId),
    {save_config, [{custom_id, CustomerId}, {card_id, CardId}]}.

update_card(Config) ->
    {_, PropConfig} = proplists:get_value(saved_config, Config),
    CardId = proplists:get_value(card_id, PropConfig),
    CustomerId = proplists:get_value(custom_id, PropConfig),
    erlstripe:update_card(CustomerId, CardId, [{"name", "Erlstripe"}]),
    {save_config, [{custom_id, CustomerId}, {card_id, CardId}]}.

get_card_with_name(Config) ->
    io:format("~p~n", [Config]),
    {_, PropConfig} = proplists:get_value(saved_config, Config),
    CardId = proplists:get_value(card_id, PropConfig),
    CustomerId = proplists:get_value(custom_id, PropConfig),
    {ok, CardProp} = erlstripe:get_card(CustomerId, CardId),
    <<"Erlstripe">> = proplists:get_value(<<"name">>, CardProp),
    {save_config, [{custom_id, CustomerId}, {card_id, CardId}]}.

delete_card(Config) ->
    {_, PropConfig} = proplists:get_value(saved_config, Config),
    CardId = proplists:get_value(card_id, PropConfig),
    CustomerId = proplists:get_value(custom_id, PropConfig),
    {ok, _Res} = erlstripe:delete_card(CustomerId, CardId),
    {save_config, [{custom_id, CustomerId}]}.
