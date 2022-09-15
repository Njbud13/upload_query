#!/bin/sh
curl -v -F 'sender_id='$1 -F 'receiver_id='$2 -F 'is_payable=true' -F 'file_type=test' -F 'file=@'$3 http://localhost:8080/upload
echo
