import Untype

main = mapM_ ( print . show . (eval []) . (read :: String -> Term) ) [
        "TmVar 0 0",
        "TmAbs \"x\" (TmVar 0 0)",
        "TmApp (TmAbs \"x\" (TmVar 0 0)) (TmAbs \"y\" (TmApp (TmVar 0 0) (TmVar 0 0)))",
        "TmAbs \"t\" (TmAbs \"f\" (TmVar 0 1))", -- tru
        "TmAbs \"f\" (TmAbs \"t\" (TmVar 0 1))", -- fls
        "TmApp (TmAbs \"l\" (TmVar 0 0)) (TmApp (TmAbs \"m\" (TmVar 0 1)) (TmAbs \"n\" (TmVar 0 2)))", -- test
        "TmApp (TmApp (TmAbs \"l\" (TmVar 0 0)) (TmApp (TmAbs \"m\" (TmVar 0 0)) (TmAbs \"n\" (TmVar 0 0)))) (TmAbs \"f\" (TmAbs \"t\" (TmVar 0 0)))" -- test fls
        ]
