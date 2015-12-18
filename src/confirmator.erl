%%% @author Leonardo Rossi <leonardo.rossi@studenti.unipr.it>
%%% @copyright (C) 2015 Leonardo Rossi
%%%
%%% This software is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This software is distributed in the hope that it will be useful, but
%%% WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this software; if not, write to the Free Software Foundation,
%%% Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.
%%%
%%% @doc This module is used to confirm a object (e.g. confirm user by email).
%%% @end

-module(confirmator).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-behaviour(application).

%% API exports
-export([start/2, init/0, stop/1, register/2, register/3, confirm/3]).

%%% Macros ===========================================================
-define(BACKEND, (confirmator_config:backend())).
-define(TOKEN, (confirmator_config:token_generator())).

%%% Types ============================================================

-type id()      :: binary().
-type token()   :: binary().
-type appctx()  :: any().

%%====================================================================
%% API functions
%%====================================================================

-spec start(application:start_type(), term()) -> ok | {error, token()}.
start(_StartType, _StartArgs) ->
  ok.

-spec init() -> {ok, appctx()} | {error, term()}.
init() ->
  ?BACKEND:init().

-spec stop(appctx()) -> ok.
stop(AppCtx) ->
  ?BACKEND:stop(AppCtx).

-spec register(id(), appctx()) ->
  {ok, {appctx(), token()}} | {error, bad_token}.
register(Id, AppCtx) ->
  register(Id, ?TOKEN:generate(), AppCtx).

-spec register(id(), token(), appctx()) ->
  {ok, {appctx(), token()}} | {error, token()}.
register(Id, Token, AppCtx) ->
  case ?BACKEND:register(Id, erlang:md5(Token), AppCtx) of
    {ok, NewAppCtx} -> {ok, {NewAppCtx, Token}};
    {error, ErrorType} -> {error, ErrorType}
  end.

-spec confirm(id(), token(), appctx()) -> {boolean(), appctx()}.
confirm(Id, Token, AppCtx) ->
  ?BACKEND:confirm(Id, erlang:md5(Token), AppCtx).

%%====================================================================
%% Internal functions
%%====================================================================
