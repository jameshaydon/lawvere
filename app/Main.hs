module Main where

import Lawvere.Decl
import Lawvere.Disp
import Lawvere.Eval
import Lawvere.Parse
import Protolude hiding (empty)
import qualified Text.Megaparsec as Mega

{-
example :: Decls
example =
  [ "uniq" === Cone [],
    -- "absurd" === CoCone [],
    -- "isEven"
    --   === Comp
    --     [ Tuple [Comp [], Proj "2"],
    --       Top "divRem",
    --       Cone
    --         [ "div" =: Proj "div",
    --           "zeroRemainder" =: Comp [Proj "rem", Tuple [Comp [], Proj "0", Top "eq"]]
    --         ],
    --       Distr "zeroRemainder",
    --       CoCone
    --         [ "true" =: Proj "div",
    --           "false" =: Comp [Lit (Str "Was not even"), Top "error", Top "absurd"]
    --         ]
    --     ],
    "map"
      === Comp
        [ Cone ["f" =: Proj "_1", "xs" =: Proj "_2"],
          Distr "xs",
          CoCone
            [ "empty" =: Comp [Top "uniq", Inj "empty"],
              "cons"
                =: Comp
                  [ Cone
                      [ "head"
                          =: Comp
                            [ Tuple [Proj "f", Comp [Proj "xs", Proj "head"]],
                              Top "app"
                            ],
                        "tail"
                          =: Comp
                            [ Tuple
                                [ Proj "f",
                                  Comp [Proj "xs", Proj "tail"]
                                ],
                              Top "map"
                            ]
                      ],
                    Inj "cons"
                  ]
            ]
        ],
    "length"
      === CoCone
        [ "empty" =: Lit (Int 0),
          "cons"
            =: Comp
              [ Tuple
                  [ Lit (Int 1),
                    Comp [Proj "tail", Top "length"]
                  ],
                Top "plus"
              ]
        ],
    "mapLength"
      === Comp
        [ Tuple
            [ EConst (Top "length"),
              Comp []
            ],
          Top "map"
        ],
    "mapPrint"
      === Comp
        [ Tuple
            [ EConst (Top "print"),
              Comp []
            ],
          Top "map"
        ],
    "cons1"
      === Comp
        [ Cone
            [ "head" =: Lit (Int 1),
              "tail" =: Comp []
            ],
          Inj "cons"
        ],
    "cons2"
      === Comp
        [ Cone
            [ "head" =: Lit (Int 2),
              "tail" =: Comp []
            ],
          Inj "cons"
        ],
    "list3"
      === Comp
        [ Inj "empty",
          Top "cons1",
          Top "cons2",
          Top "cons1"
        ],
    "list2"
      === Comp
        [ Inj "empty",
          Top "cons2",
          Top "cons1"
        ],
    "listOfLists"
      === Comp
        [ Inj "empty",
          Cone
            [ "head" =: Top "list3",
              "tail" =: Comp []
            ],
          Inj "cons",
          Cone
            [ "head" =: Top "list2",
              "tail" =: Comp []
            ],
          Inj "cons"
        ],
    "main"
      === Comp
        [ Top "listOfLists",
          Top "mapLength"
        ]
  ]
  where
    name === e = DAr name (TNamed "Int") e
-}

-- empty :: Val
-- empty = Tag "empty" (Rec mempty)

-- mkRec :: [(LcIdent, Val)] -> Val
-- mkRec xs = Rec (Map.fromList [(PLab i, x) | (i, x) <- xs])

-- cons :: Val -> Val -> Val
-- cons x xs = Tag "cons" (mkRec ["head" =: x, "tail" =: xs])

-- int :: Integer -> Val
-- int = Sca . Int

-- lis :: [Val] -> Val
-- lis = foldr cons empty

-- listOfLists :: Val
-- listOfLists = lis [lis [int 1, int 2], lis [int 2], lis [int 6, int 77]]

say :: Text -> IO ()
say = putStrLn

main :: IO ()
main = do
  basic <- readFile "examples/basic.law"
  case Mega.parse (parsed <* Mega.eof) "basic.law" basic of
    Left err -> putStr (Mega.errorBundlePretty err)
    Right (prog :: [Decl]) -> do
      putStrLn (render prog)
      let inp = Rec mempty
      v <- eval inp prog
      say "input:"
      say (render inp)
      say "-------"
      say "result:"
      putStrLn ("  " <> render v)
      putStrLn (mkJS prog)
