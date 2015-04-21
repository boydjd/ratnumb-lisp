    (defun runTest1 ()
            (rnprint (quote (3 (2 5)) ) ))

    (defun runTest2 ()
            (rnprint 
                        (rnsub (quote (-2 (3 7))) (quote  (1 (2 3))) )))

(runTest1)
(runTest2)
