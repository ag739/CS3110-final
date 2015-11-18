type p_type = Hardware | Software | Humanities

type pokecaml = {name: string; attacks: (string*int) list;
                 pokecaml_type: p_type; hp: int}

let camldex = []

let all_pokecaml= [{name = "Camlchu"; attacks= [("electrocute" ,8)];
                    pokecaml_type= Hardware; hp= 25};
                   {name = "Chiragzard"; attacks= [("piazza", 3)];
                    pokecaml_type= Humanities; hp= 25};
                   {name = "Immutabilitypuff"; attacks= [("pattern match", 10)];
                    pokecaml_type= Software; hp= 25}]