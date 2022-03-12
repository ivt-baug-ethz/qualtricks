curl --request POST \
  --url https://fra1.qualtrics.com/API/v3/mailinglists/ML_cROpOLdqoh22Tyu/contacts \
  --header 'Content-Type: application/json' \
  --header 'X-API-TOKEN: WmUIBKhzXPftK2g1MNEDcMtqqWyUKN3V4bXkIXgi' \
  --data '{
  "firstName": "hello",
  "lastName": "world",
  "email": "hello@world.ch",
  "externalDataRef": "E",
  "language": "de"
 }'
