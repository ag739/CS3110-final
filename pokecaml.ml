
  type p_type = Hardware | Software | Humanities

  type pokecaml = {name: string; attacks: (string*int) list;
                   pokecaml_type: p_type; hp: int}

  let camldex = []

  let all_pokecaml= [{name = "Camlchu"; attacks= [("electrocute" ,8)];
                      pokecaml_type= Hardware; hp= 100};
                     {name = "Chiragzard"; attacks= [("piazza", 3)];
                      pokecaml_type= Humanities; hp= 100};
                     {name = "Immutabilitypuff"; attacks= [("pattern match", 10)
                     ; ("infinite recursion", 2)];
                      pokecaml_type= Software; hp= 100};
                      {name = "Recursee"; attacks= [("Base case", 8); ("Rec", 12);
                      ("Return", 7); ("Tail-recursion", 10)];
                      pokecaml_type = Software; hp= 100};
                      {name = "Deferredata"; attacks= [("Bind", 6); ("Upon", 4);
                      ("Return", 7); (">>=", 12)];
                      pokecaml_type = Hardware; hp= 100};
                      {name = "Proofle"; attacks= [("Induction", 10);
                      ("Equivalence", 8); ("Math", 7); ("Specify", 11)];
                      pokecaml_type = Humanities; hp= 100}]
