(import "triangle" trunk :message "Import version 1.6")
(tag trunk "triangle-1.6" :message "Version 1.6")
(let ((branch (~ branches "triangle-1.6")))
  (copy trunk branch)
  (import "triangle1" branch)
  (tag branch "triangle-1.6-1")
  (import "triangle2" branch)
  (tag branch "triangle-1.6-2"))
