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
%%% @doc Implementation of a token generator.
%%% @end

-module(confirmator_token_generator_default).

-behaviour(confirmator_token_generator).

-author('Leonardo Rossi <leonardo.rossi@studenti.unipr.it>').

-export([generate/0]).

-define(TOKEN_LENGTH, (confirmator_config:token_length())).

-spec generate() -> confirmator:token().
generate() ->
  Random = crypto:rand_bytes(?TOKEN_LENGTH),
  list_to_binary(lists:flatten(
      [io_lib:format("~2.16.0b", [C]) || <<C>> <= Random])).