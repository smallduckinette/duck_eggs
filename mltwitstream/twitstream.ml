let callback e = 
  (
    try
      let json = Yojson.Safe.from_string e in
      match json with
          `Assoc elements -> 
            (
              match (List.assoc "text" elements) with
                  `String e -> prerr_endline e
                | _ -> raise Not_found
            )
        | _ -> raise Not_found
    with _ -> prerr_endline "Error reading message"
  );
  String.length e
    
let _ =
  let username = ref ""
  and password = ref ""
  and keywords = ref [] in
  Arg.parse 
    ["-u", Arg.Set_string username, "username";
     "-p", Arg.Set_string password, "password"]
    (fun e -> keywords := e::(!keywords))
    "twitstream [flags] [keywords]";
  
  let all_keywords = "track=" ^ (String.concat ":" (!keywords)) in
  let c = Curl.init() in
  Curl.setopt c (Curl.CURLOPT_USERPWD (!username ^ ":" ^ !password));
  Curl.setopt c (Curl.CURLOPT_URL "https://stream.twitter.com/1/statuses/filter.json");
  Curl.setopt c (Curl.CURLOPT_WRITEFUNCTION callback);
  Curl.setopt c (Curl.CURLOPT_POST true);
  Curl.setopt c (Curl.CURLOPT_POSTFIELDS all_keywords);
  Curl.setopt c (Curl.CURLOPT_POSTFIELDSIZE (String.length all_keywords));
  Curl.perform c;
  Curl.cleanup c
    
