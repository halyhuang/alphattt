{application, db_backup_app,
 [{description,"AlphaTTT db back up"},
  {vsn, "1.00"},
  {modules,[db_backup_app, db_backup_sup, db_backup]},
  {registered, []},
  {mod,{db_backup_app,[]}},
  {env, []},
  {applications,[kernel, stdlib]}]}.
