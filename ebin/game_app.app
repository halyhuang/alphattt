{application, game_app,
 [{description,"AlphaTTT game server"},
  {vsn, "1.00"},
  {modules,[game_app, game_sup, board, room]},
  {registered, []},
  {mod,{game_app,[]}},
  {env, []},
  {applications,[kernel, stdlib]}]}.
