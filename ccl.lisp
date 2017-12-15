(print (+ 111 222))
(print *UNPROCESSED-COMMAND-LINE-ARGUMENTS*)

(open-shared-library "ccl.dll")
(setq answer
      (external-call "my_add2"
                     (:signed 32) 11 (:signed 32) 22
                     (:signed 32)))
(print answer)
(quit)
