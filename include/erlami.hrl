-record(ami_event, {
    name,
    privilege = [],
    data = []
}).

-record(ami_response, {
    success,
    uuid,
    message = ""
}).

-record(ami_request, {
    action,
    data = []
}).
