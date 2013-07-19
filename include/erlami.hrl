-record(ami_event, {
    name,
    action_id,
    privilege = [],
    data = []
}).

-record(ami_response, {
    success,
    failure_type,
    uuid,
    message = ""
}).

-record(ami_request, {
    action,
    data = []
}).
