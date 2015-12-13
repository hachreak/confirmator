confirmator
===========

[![Build Status](https://travis-ci.org/hachreak/confirmator.svg?branch=master)](https://travis-ci.org/hachreak/confirmator)

An OTP library to generate and verify tokens associated with a object.

Common use case: user registration.

1. After the user sent the registration form.
2. The system, through this module) save a temporary record in the database
   where it associates the new user with a **confirmation token** and send to
   the user an email with a confirmation link (containing the confirmation
   token).
3. The user will read the email and confirm heself by open the link.
4. The system will read the token from the url and compare with the token
   saved in the database. If they are equal, it means the user registration
   is terminated with success.

*Note*: the tokens will be usable only one time. After that, they will be
deleted for security.

*Note*: the registration and confirmation implicity hashify the token before
save it. It's done for security reasons.


Backend Implementation
----------------------

You can find an example of backend inside the `test` directory.

To see a "real" backend, you can see:

  - [confirmator_mongopool](https://github.com/hachreak/confirmator_mongopool):
    It implements the backend through
    [mongopool](https://github.com/hachreak/mongopool).


Configuration
-------------

The library makes no assumptions on how you want implement the persistence for
the tokens.

You need to specify the backend in the configuration file.

The token generator and the token length configuration are optional.
If you doesn't specify how to generate the tokens, the default
`confirmator_token.erl` will be used for generate tokens.
The same for the token length: if you doesn't specify nothing, they will be
`32` bytes.


Example of configuration:

```erlang
[
  {confirmator}, [
    {backend, YOUR_BACKEND},
    {token_generation, YOUR_TOKEN_GENERATOR},
    {token_length, 32}
  ]
]
```

Usage
-----

 - Start the `backend` application if it's necessary with:

```erlang
application:ensure_all_started(mybackend).
```

 - To start the `confirmator` application:

```erlang
{ok, AppCtx} = confirmator:init().
```

Now, you can easily start the confirmation of a object (e.g. a new user that
need to confirm heself by email).

 - Register the object: the application will save somewhere (it depends from
the backend installed) the id and the (automatically) generated token.

```erlang
{ok, {Token, NewAppCtx}} = confirmator:register(<<"my_object_id">>, AppCtx).
```

 - The confirmation of the object (e.g. after the user receive the
   confirmation email that contains a well defined confirmation link, he'll
   open the it, passing back the token to the system, now able to confirm him).

```erlang
case confirmator:confirm(<<"my_object_id">>, Token, NewAppCtx) of
  {true, NewNewAppCtx} -> ok;
  {false, NewNewAppCtx} -> no
end.
```

 - To stop the application:

```erlang
confirmator:stop(NewNewAppCtx).
```

Build
-----

    $ ./utils/rebar3 compile


Tests
-----

    $ ./utils/rebar3 eunit
