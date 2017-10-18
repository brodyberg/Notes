module TestingNest where
    
import SimpleJSON (JValue(..))    
import PrettyJSON

jvalue = 
    (JArray [
        (JObject [
            ("Oathbreaker", (JString "Sink Into Sin I")), 
            ("Fallujah", (JString "Starlit Path")), 
            ("Agoraphobic Nosebleed", (JString "Not a Daughter")), 
            ("Lut", (JString "BoyToy"))
            ]),
        (JArray [
            (JObject [
                ("Mechanicum", JArray [
                    (JString "Thanatar Siege Automaton"),
                    (JString "Mars Pattern Reaver Titan")
                    ]),
                ("Vlka Fenryka", JArray [
                    (JString "Leman Russ"),
                    (JBool True)
                ])
            ])
        ])])

jvalue' = 
        (JObject [
            ("Oathbreaker", (JString "Sink Into Sin I")), 
            ("Fallujah", (JString "Starlit Path")), 
            ("Agoraphobic Nosebleed", (JString "Not a Daughter")), 
            ("Lut", (JString "BoyToy"))
            ])

jvalue'' = 
    (JObject [
        ("k", (JString "{v}"))
        ])

jvalue2 = 
    (JObject [
        ("k", (JString "{v}")), 
        ("k2", (JNumber 2))])

simplest = (JObject [("k", (JString "v"))])