curl --request POST \
  --url https://<base_url>/API/v3/mailinglists/ML_cROpOLdqoh22Tyu/contacts \
  --header 'Content-Type: application/json' \
  --header 'X-API-TOKEN: <token>' \
  --data '{
  "firstName": "hello",
  "lastName": "world",
  "email": "hello@world.ch",
  "externalDataRef": "E",
  "language": "de"
 }'
