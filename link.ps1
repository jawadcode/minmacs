New-Item C:\Users\jawad\.emacs.d\init.el       -ItemType SymbolicLink -Target "$(Get-Location)/init.el"
New-Item C:\Users\jawad\.emacs.d\early-init.el -ItemType SymbolicLink -Target "$(Get-Location)/early-init.el"
