{application, ts,
             [{description,"erlang test suite"},
              {vsn,"0.0.1"},
              {modules,[ts_app, ts_sup, ts_server, ts_client_sup, ts_client]},
              {registerd,[kernel,stdlib]},
              {mod,{ts_app,[]}}]}.
