-record(ami_event, {
    name,
    privilege = [],
    data = []
}).

-record(ami_response, {
    success,
    message = ""
}).
