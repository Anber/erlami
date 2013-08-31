-type field() :: {atom(), string()}.

%% AMI 1.4 Specification
%% https://wiki.asterisk.org/wiki/display/AST/AMI+1.4+Specification

%% 4.2.1.2.2. Channels
-record(ami_ev_channel, {
    name                                :: nonempty_string(),
    uniqueid                            :: nonempty_string(),
    channel_state                       :: [0..10],
    channel_state_desc                  :: string(),
    caller_id_num                       :: string(),
    caller_id_name                      :: string(),
    connected_line_num                  :: string(),
    connected_line_name                 :: string(),
    account_code                        :: string(),
    context                             :: string(),
    exten                               :: string(),
    priority                            :: string(),
    chan_variable                       :: list(field())
}).

%% 4.2.1.2. Events
-record(ami_event, {
    %% 4.2.1.2.1. General Fields
    name                                :: atom(),
    action_id                           :: nonempty_string(),
    privilege = []                      :: list(atom()),

    channels = []                       :: list(#ami_ev_channel{}),

    %% TODO: 4.2.1.2.3. Bridges

    %% 4.2.1.2.4. Action Responses
    response                            :: 'success' | 'failure',
    event_list                          :: any(),
    message                             :: binary(),

    %% other fields
    fields = []                         :: list(field())
}).

-record(ami_response, {
    success                             :: boolean(),
    action_id                           :: nonempty_string(),
    message = ""                        :: string(),
    fields = []                         :: list(field())
}).

-record(ami_action, {
    name                                :: atom(),
    id = uuid:to_string(uuid:uuid1())   :: nonempty_string(),
    fields = []                         :: list(field())
}).
