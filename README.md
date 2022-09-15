# Example Erlang Cowboy API service 
Upload / Query example

## To start
Install rebar
Install Postgres and create a database (sys.config defaults name to 'erl')

Compile and start:
```
make
make run
```

Initialize DB via console:
```
persist:init_db().
```

## Usage

### Upload
Required fields: `sender_id (int)`, `receiver_id (int)`, `is_payable (bool)`, `file_type (str)`, `file (bin)`

Example POST:
```
curl -F 'sender_id=1' -F 'receiver_id=2' -F 'is_payable=true' -F 'file_type=test' -F 'file=@./README.md' http://localhost:8080/upload
```
Returns `content_id (int)`

### Query
Example GET content_id:
```
curl http://localhost:8080/content?content_id=1
```
Returns "application/octet-stream"

Example GET sender_id:
```
curl http://localhost:8080/content?sender_id=1
```
Returns "application/json" `{"array":[{"content_id":1,"receiver_id":2}`

Example GET sender_id, receiver_id:
```
curl http://localhost:8080/content?sender_id=1&receiver_id=2
```
Returns "application/json" `{"3":"true"}` (content_id, is_payable)
