let json =
  [|
    "{\n\
     \"firstName\": \"John\",\n\
     \"lastName\": \"Smith\",\n\
     \"isAlive\": true,\n\
     \"age\": 25,\n\
     \"address\": {\n\
     \"streetAddress\": \"21 2nd Street\",\n\
     \"city\": \"New York\",\n\
     \"state\": \"NY\",\n\
     \"postalCode\": \"10021-3100\"\n\
     },\n\
     \"phoneNumbers\": [\n\
     {\n\
     \"type\": \"home\",\n\
     \"number\": \"212 555-1234\"\n\
     },\n\
     {\n\
     \"type\": \"office\",\n\
     \"number\": \"646 555-4567\"\n\
     }\n\
     ],\n\
     \"children\": [],\n\
     \"spouse\": null\n\
     }\n";
    "{ \"face\": \"😂\" }";
    "{ \"face\": \"\\uD83D\\uDE02\" }";
  |]

let () =
  for i = 0 to Array.length json - 1 do
    ignore (JSON.from_string json.(i))
  done
