%%%-------------------------------------------------------------------
%%% @author Niclas Axelsson <niclas@burbasconsulting.com>
%%% @copyright (C) 2014, Niclas Axelsson
%%% @doc
%%% Module for handling stripe requests
%%% @end
%%% Created : 14 Mar 2014 by Niclas Axelsson <niclas@burbasconsulting.com>
%%%-------------------------------------------------------------------
-module(erlstripe).

%% Charges
-export([
         create_card_charge/3,
         create_card_charge/4,
         create_customer_charge/3,
         create_customer_charge/4,
         retrieve_charge/1,
         update_charge/3,
         refund_charge/1,
         refund_charge/2,
         capture_charge/1,
         capture_charge/2,
         list_charges/1
        ]).

%% Customers
-export([
         create_customer/1,
         get_customer/1,
         update_customer/2,
         delete_customer/1,
         get_customers/1
        ]).

%% Cards
-export([
         create_card/2,
         get_card/2,
         update_card/3,
         delete_card/2,
         list_cards/1
        ]).

%% Subscriptions
-export([
         create_new_subscription/3,
         retrieve_subscriptions/2,
         update_subscription/3,
         cancel_subscription/3,
         list_subscriptions/2
        ]).

%% Plans
-export([
         create_plan/6,
         retrieve_plan/1,
         update_plan/2,
         delete_plan/1,
         list_plans/1
        ]).

%% Coupons
-export([
         create_coupon/2,
         retrieve_coupon/1,
         delete_coupon/1,
         list_coupons/1
        ]).

%% Discounts
-export([
         delete_all_customer_discount/1,
         delete_subscription_discount/2
        ]).

%% Invoices
-export([
         retrieve_invoice/1,
         retrieve_invoice_line/2,
         create_invoice/2,
         pay_invoice/1,
         update_invoice/2,
         list_invoices/1,
         retrieve_upcoming_invoice/2
        ]).

%% Invoice items
-export([
         create_invoice_item/4,
         retrieve_invoice_item/1,
         update_invoice_item/2,
         delete_invoice_item/1,
         list_all_invoice_items/1
        ]).

%% Disputes
-export([
         update_dispute/2,
         close_dispute/1
        ]).

%% Transfers
-export([
         create_transfer/4,
         retrieve_transfer/1,
         update_transfer/2,
         cancel_transfer/1,
         list_transfers/1
        ]).

%% Recipients
-export([
         create_recipient/3,
         retrieve_recipient/1,
         update_recipient/2,
         delete_recipient/1,
         list_recipients/1
        ]).

%% Application fees
-export([
         retrieve_application_fee/1,
         refund_application_fee/2,
         list_application_fees/1
        ]).

%% Account
-export([retrieve_account_details/0]).

%% Balance
-export([
         retrieve_balance/0,
         retrieve_balance_transaction/1,
         list_balance_history/1
        ]).

%% Events
-export([
         retrieve_event/1,
         list_all_events/1
        ]).

-define(HTTP_TIMEOUT, 100000).
-define(BASE_URL, "https://api.stripe.com/v1").


%%%================================
%%% Charges
%%%================================

create_card_charge(Amount, Currency, Card) ->
    create_card_charge(Amount, Currency, Card, []).
create_card_charge(Amount, Currency, Card, OptionalParams) when is_list(Currency),
                                                                length(Currency) == 3 ->
    Params = [{"amount", Amount},
              {"currency", Currency},
              {"card", Card}|OptionalParams],
    make_request("/charges", post, Params).

create_customer_charge(Amount, Currency, CustomerId) ->
    create_customer_charge(Amount, Currency, CustomerId, []).
create_customer_charge(Amount, Currency, CustomerId, OptionalParams) when is_list(Currency),
                                                                          length(Currency) == 3 ->
    Params = [{"amount", Amount},
              {"currency", Currency},
              {"customer", CustomerId}|OptionalParams],
    make_request("/charges", post, Params).

retrieve_charge(ChargeId) ->
    make_request("/charges/" ++ uri_encode(ChargeId), get).

update_charge(ChargeId, Description, Metadata) ->
    Params = [{"description", Description},
              {"metadata", Metadata}],
    make_request("/charges/" ++ uri_encode(ChargeId), post, Params).


refund_charge(ChargeId) ->
    refund_charge(ChargeId, undefined).
refund_charge(ChargeId, Amount) ->
    Params = [{"amount", Amount} || Amount /= undefined],
    make_request("/charges/" ++ uri_encode(ChargeId), post, Params).

capture_charge(ChargeId) ->
    capture_charge(ChargeId, undefined).
capture_charge(ChargeId, Amount) ->
    Params = [{"amount", Amount} || Amount /= undefined],
    make_request("/charges/" ++ uri_encode(ChargeId) ++ "/capture", post, Params).

list_charges(Parameters) ->
    make_request("/charges", get, Parameters).


%%%================================
%%% Customers
%%%================================
create_customer(Parameters) ->
    make_request("/customers", post, Parameters).

get_customer(CustomerId) ->
    make_request("/customers/" ++ uri_encode(CustomerId), get).

update_customer(CustomerId, Parameters) ->
    make_request("/customers/" ++ uri_encode(CustomerId), post, Parameters).

delete_customer(CustomerId) ->
    make_request("/customers/" ++ uri_encode(CustomerId), delete).

get_customers(Parameters) ->
    make_request("/customers", get, Parameters).

%%%================================
%%% Cards
%%%================================
create_card(CustomerId, Parameters) ->
    make_request("/customers/" ++ uri_encode(CustomerId) ++ "/cards", post, Parameters).

get_card(CustomerId, CardId) ->
    make_request("/customers/" ++ uri_encode(CustomerId) ++ "/cards/" ++ uri_encode(CardId), get).

update_card(CustomerId, CardId, Parameters) ->
    make_request("/customers/" ++ uri_encode(CustomerId) ++ "/cards/" ++ uri_encode(CardId), post, Parameters).

delete_card(CustomerId, CardId) ->
    make_request("/customers/" ++ uri_encode(CustomerId) ++ "/cards/" ++ uri_encode(CardId), delete).

list_cards(CustomerId) ->
    make_request("/customers/" ++ uri_encode(CustomerId) ++ "/cards", get).

%%%================================
%%% Subscriptions
%%%================================
create_new_subscription(CustomerId, PlanId, Parameters) ->
    Params = [{"plan", PlanId}|Parameters],
    make_request("/customers/" ++ uri_encode(CustomerId) ++ "/subscriptions", post, Params).

retrieve_subscriptions(CustomerId, SubscriptionId) ->
    make_request("/customers/" ++ uri_encode(CustomerId) ++ "/subscriptions/" ++ uri_encode(SubscriptionId), get).

update_subscription(CustomerId, SubscriptionId, Parameters) ->
    make_request("/customers/" ++ uri_encode(CustomerId) ++ "/subscriptions/" ++ uri_encode(SubscriptionId), post, Parameters).

cancel_subscription(CustomerId, SubscriptionId, Parameters) ->
    make_request("/customers/" ++ uri_encode(CustomerId) ++ "/subscriptions/" ++ uri_encode(SubscriptionId), delete, Parameters).

list_subscriptions(CustomerId, Parameters) ->
    make_request("/customers/" ++ uri_encode(CustomerId) ++ "/subscriptions", get, Parameters).


%%%================================
%%% Plans
%%%================================
create_plan(Id, Amount, Currency, Interval, Name, Parameters) ->
    Params = [{"id", Id},
              {"amount", Amount},
              {"currency", Currency},
              {"interval", Interval},
              {"name", Name}|Parameters],
    make_request("/plans", post, Params).

retrieve_plan(PlanId) ->
    make_request("/plans/" ++ uri_encode(PlanId), get).

update_plan(PlanId, Parameters) ->
    make_request("/plans/" ++ uri_encode(PlanId), post, Parameters).

delete_plan(PlanId) ->
    make_request("/plans/" ++ uri_encode(PlanId), delete).

list_plans(Parameters) ->
    make_request("/plans", get, Parameters).


%%%================================
%%% Coupons
%%%================================
create_coupon(Duration, Parameters) ->
    make_request("/coupons", post, [{"duration", Duration}|Parameters]).

retrieve_coupon(CouponId) ->
    make_request("/coupons/" ++ uri_encode(CouponId), get).

delete_coupon(CouponId) ->
    make_request("/coupons/" ++ uri_encode(CouponId), delete).

list_coupons(Parameters) ->
    make_request("/coupons", get, Parameters).


%%%================================
%%% Discounts
%%%================================
delete_all_customer_discount(CustomerId) ->
    make_request("/customers/" ++ uri_encode(CustomerId) ++ "/discount", delete).

delete_subscription_discount(CustomerId, SubscriptionId) ->
    make_request("/customers/" ++ uri_encode(CustomerId) ++ "/subscriptions/" ++ uri_encode(SubscriptionId) ++ "/discount", delete).


%%%================================
%%% Invoices
%%%================================
retrieve_invoice(InvoiceId) ->
    make_request("/invoices/" ++ uri_encode(InvoiceId), get).

retrieve_invoice_line(InvoiceId, Parameters) ->
    make_request("/invoices/" ++ uri_encode(InvoiceId) ++ "/lines", get, Parameters).

create_invoice(CustomerId, Parameters) ->
    make_request("/invoices", post, [{"customer", CustomerId}|Parameters]).

pay_invoice(InvoiceId) ->
    make_request("/invoices/" ++ uri_encode(InvoiceId) ++ "/pay", post).

update_invoice(InvoiceId, Parameters) ->
    make_request("/invoices/" ++ uri_encode(InvoiceId), post, Parameters).

list_invoices(Parameters) ->
    make_request("/invoices", get, Parameters).

retrieve_upcoming_invoice(CustomerId, Parameters) ->
    make_request("/invoices/upcoming", post, [{"customer", CustomerId}|Parameters]).

%%%================================
%%% Invoice items
%%%================================
create_invoice_item(CustomerId, Amount, Currency, Parameters) ->
    make_request("/invoiceitems", post, [{"customer", CustomerId}, {"amount", Amount}, {"currency", Currency}|Parameters]).

retrieve_invoice_item(InvoiceItemId) ->
    make_request("/invoiceitems/" ++ uri_encode(InvoiceItemId), get).

update_invoice_item(InvoiceItemId, Parameters) ->
    make_request("/invoiceitems/" ++ uri_encode(InvoiceItemId), post, Parameters).

delete_invoice_item(InvoiceItemId) ->
    make_request("/invoiceitems/" ++ uri_encode(InvoiceItemId), delete).

list_all_invoice_items(Parameters) ->
    make_request("/invoiceitems", get, Parameters).


%%%================================
%%% Disputes
%%%================================
update_dispute(ChargeId, Parameters) ->
    make_request("/charges/" ++ uri_encode(ChargeId) ++ "/dispute", post, Parameters).

close_dispute(ChargeId) ->
    make_request("/charges/" ++ uri_encode(ChargeId) ++ "/dispute/close", post).


%%%================================
%%% Transfers
%%%================================
create_transfer(Amount, Currency, Recipient, Parameters) ->
    make_request("/transfers", post, [{"amount", Amount}, {"currency", Currency}, {"recipient", Recipient}|Parameters]).

retrieve_transfer(TransferId) ->
    make_request("/transfers/" ++ uri_encode(TransferId), get).

update_transfer(TransferId, Parameters) ->
    make_request("/transfers/" ++ uri_encode(TransferId), post, Parameters).

cancel_transfer(TransferId) ->
    make_request("/transfers/" ++ uri_encode(TransferId) ++ "/cancel", post).

list_transfers(Parameters) ->
    make_request("/transfers", get, Parameters).


%%%================================
%%% Recipients
%%%================================
create_recipient(Name, Type, Parameters) ->
    make_request("/recipients", post, [{"name", Name}, {"type", Type}|Parameters]).

retrieve_recipient(RecipientId) ->
    make_request("/recipients/" ++ uri_encode(RecipientId), get).

update_recipient(RecipientId, Parameters) ->
    make_request("/recipients/" ++ uri_encode(RecipientId), post, Parameters).

delete_recipient(RecipientId) ->
    make_request("/recipients/" ++ uri_encode(RecipientId), delete).

list_recipients(Parameters) ->
    make_request("/recipients", get, Parameters).

%%%================================
%%% Application fees
%%%================================
retrieve_application_fee(ApplicationFeeId) ->
    make_request("/application_fees/" ++ uri_encode(ApplicationFeeId), get).

refund_application_fee(ApplicationFeeId, Parameters) ->
    make_request("/application_fees/" ++ uri_encode(ApplicationFeeId) ++ "/refund", post, Parameters).

list_application_fees(Parameters) ->
    make_request("/application_fees", get, Parameters).


%%%================================
%%% Account
%%%================================
retrieve_account_details() ->
    make_request("/account", get).

%%%================================
%%% Balance
%%%================================
retrieve_balance() ->
    make_request("/balance", get).

retrieve_balance_transaction(TransactionId) ->
    make_request("/balance/history/" ++ uri_encode(TransactionId), get).

list_balance_history(Parameters) ->
    make_request("/balance/history", get, Parameters).


%%%================================
%%% Events
%%%================================
retrieve_event(EventId) ->
    make_request("/events/" ++ uri_encode(EventId), get).

list_all_events(Parameters) ->
    make_request("/events", get, Parameters).


%%%================================
%%% Tokens
%%%================================
%% TODO

%%%================================
%%% Internal functions
%%%================================

authorization() ->
    {ok, StripeKey} = application:get_env(erlstripe, stripe_key),
    {"Authorization", "Bearer " ++ StripeKey}.

handle_response({ok, {200, JSON}}) ->
    {ok, jsx:decode(list_to_binary(JSON))};
handle_response({ok, {400, JSON}}) ->
    {error, {bad_request, jsx:decode(list_to_binary(JSON))}};
handle_response({ok, {401, JSON}}) ->
    {error, {unauthorized, jsx:decode(list_to_binary(JSON))}};
handle_response({ok, {402, JSON}}) ->
    {error, {request_failed, jsx:decode(list_to_binary(JSON))}};
handle_response({ok, {404, JSON}}) ->
    {error, {not_found, jsx:decode(list_to_binary(JSON))}};
handle_response(Error) ->
    io:format("~p~n", [Error]),
    {error, stripe_error}.

make_request(Endpoint, Method) ->
    handle_response(
      httpc:request(
        Method,
        {
          ?BASE_URL ++ Endpoint,
          [authorization()]
        }, [{timeout, ?HTTP_TIMEOUT}], [{full_result, false}])
     ).

make_request(Endpoint, post, Parameters) ->
    handle_response(
      httpc:request(
        post,
        {
          ?BASE_URL ++ Endpoint,
          [authorization()],
          "application/x-www-form-urlencoded",
          url_encode(Parameters)
        }, [{timeout, ?HTTP_TIMEOUT}], [{full_result, false}])
     );
make_request(Endpoint, Method, Parameters) ->
    Params = url_encode(Parameters),
    make_request(Endpoint ++ "?" ++ Params, Method).


url_encode(List) ->
    string:join(url_encode_h(List), "&").

url_encode_h([]) -> [];
url_encode_h([{Key, [T|_] = Struct}|Tl]) when is_tuple(T) ->
    %% Encode struct
    [uri_encode_struct(Key, Struct)|url_encode_h(Tl)];
url_encode_h([{Key, Val}|Tl]) ->
    [uri_encode(Key) ++ "=" ++ uri_encode(Val) | url_encode_h(Tl)].

uri_encode_struct(Key, Struct) ->
    string:join(uri_encode_struct_aux(Key, Struct), "&").

uri_encode_struct_aux(_, []) -> [];
uri_encode_struct_aux(Key, [{SubKey, Val}|Tl]) ->
    [uri_encode(Key) ++ "[" ++ uri_encode(SubKey) ++ "]=" ++ uri_encode(Val)|uri_encode_struct_aux(Key, Tl)].

uri_encode(Val) when is_binary(Val) ->
    case binary_to_list(Val) of
        [Value] when is_integer(Value) ->
            uri_encode(Value);
        Value ->
            uri_encode(Value)
    end;
uri_encode(Val) when is_integer(Val) ->
    uri_encode(integer_to_list(Val));
uri_encode(Val) when is_atom(Val) ->
    uri_encode(atom_to_list(Val));
uri_encode(Val) when is_list(Val) ->
    http_uri:encode(Val).
