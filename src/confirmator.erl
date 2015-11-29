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

%% API exports
-export([register/2, register/3, confirm/3]).

%%% Macros ===========================================================
-define(BACKEND, (confirmator_config:backend())).
-define(TOKEN, (confirmator_config:token_generation())).

%%% Types ============================================================

-type identifier() :: binary().
-type token()      :: binary().
-type appctx()     :: any().

%%====================================================================
%% API functions
%%====================================================================

-spec register(identifier(), appctx()) ->
  {ok, appctx()} | {error, bad_token}.
register(Id, AppCtx) ->
  register(Id, ?TOKEN:generate(), AppCtx).

-spec register(identifier(), token(),
               appctx()) -> {ok, appctx()} | {error, bad_token}.
register(Id, Token AppCtx) ->
  ?BACKEND:register(Id, Token, AppCtx).

-spec confirm(identifier(), token(),
              appctx()) -> {boolean(), appctx()}.
confirm(Id, Token, AppCtx) ->
  ?BACKEND:register(Id, Token, AppCtx).

%%====================================================================
%% Internal functions
%%====================================================================
