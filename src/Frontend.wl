BeginPackage["Notebook`Editor`TOC`", {
    "JerryI`Misc`Events`",
    "JerryI`Misc`Events`Promise`", 
    "JerryI`Notebook`", 
    "JerryI`WLX`",
    "JerryI`WLX`Importer`",
    "JerryI`WLX`WebUI`",  
    "JerryI`Notebook`AppExtensions`",
    "JerryI`Misc`WLJS`Transport`"
}]

Begin["`Private`"]

rootDir = $InputFileName // DirectoryName // ParentDirectory;

EventHandler[AppExtensions`AppEvents// EventClone, {
    "Loader:NewNotebook" ->  (Once[ attachListeners[#] ] &),
    "Loader:LoadNotebook" -> (Once[ attachListeners[#] ] &)
}];

vocabulary = <||>;

buildVocabulary[n_Notebook] := With[{outputCells = Select[n["Cells"], (#["Display"] == "markdown")&]},
    vocabulary[n] = makeEntity /@ outputCells // Flatten;
]

makeEntity[cell_] := With[{lines = StringSplit[cell["Data"], "\n"]},
    Map[Function[line, StringCases[line, (StartOfString~~">" | StartOfString)~~d:("#"..)~~WhitespaceCharacter~~str__:>heading[StringLength[d], str, cell] ] ], lines]
] 

debounce = AbsoluteTime[];

addEntity[n_Notebook, cell_] := With[{},
    If[cell["Display"] =!= "markdown", Return[] ];
    (*If[cell["Display"] =!= "markdown", Return[] ];
    With[{pos = Position[cell["Notebook", "Cells"], cell] // First // First},
        vocabulary[n] = Insert[vocabulary[n], makeEntity[cell], pos];
    ];*)
    If[AbsoluteTime[] - debounce < 3, Return[] ];

    debounce = AbsoluteTime[];
    Echo["TOK: added enttity"];
    buildVocabulary[n];
    EventFire[n, "UpdateTOC", vocabulary[n] ];
]

removeEntity[n_Notebook, cell_] := (
    If[cell["Display"] =!= "markdown", Return[] ];

    Echo["TOK: removed enttity"];
    vocabulary[n] = vocabulary[n] /. heading[_, _, cell] -> Nothing;
    EventFire[n, "UpdateTOC", vocabulary[n] ];
);

attachListeners[notebook_Notebook] := With[{e = notebook // EventClone},
    Echo["Attach event listeners to notebook from EXTENSION >> TOC"];
    EventHandler[e, {
        "OnBeforeLoad" -> Function[opts,
            Echo["TOC: Build vocabulary..."];
            buildVocabulary[notebook];
            notebook["TOC"] := vocabulary[notebook]; (* create a reference *)
        ],
        "New Cell" -> Function[cell, addEntity[notebook, cell] ], 
        "Remove Cell" -> Function[cell, removeEntity[notebook, cell] ]        
    }]; 
]

AppExtensions`TemplateInjection["AppSidebarBottom"] = ImportComponent[FileNameJoin[{rootDir, "template", "TOC.wlx"}] ];

End[]
EndPackage[]