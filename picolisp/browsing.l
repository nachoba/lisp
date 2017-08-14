# Browsing
# --------
# PicoLisp provides some functions for inspecting pieces of data and code within the running system.
# The really basic tools are of course available: print, size

# (setq a '(a b c))
# (setq b  (1 2 3))
# (setq c '(a b c))

# (size a)  -> 3
# (print a) -> (a b c)


# There are more powerful tools: match, show

# (match a b) -> NIL
# (match a c) -> T

(setq a '(a b c))
(setq b  (1 2 3))
(setq c '(a b d))



