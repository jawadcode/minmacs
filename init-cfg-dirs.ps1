Remove-Item -Recurse -Force "$env:HOME\.emacs.d"

New-Item -ItemType Directory "$env:HOME\.emacs.d"
New-Item -ItemType Directory "$env:HOME\.emacs.d\codemacs"
New-Item -ItemType Directory "$env:HOME\.emacs.d\mathmacs"

New-Item "$env:HOME\.emacs.d\common" -ItemType SymbolicLink -Target "$(Get-Location)\common"

New-Item "$env:HOME\.emacs.d\codemacs\early-init.el" -ItemType SymbolicLink -Target "$(Get-Location)\codemacs\early-init.el"
New-Item "$env:HOME\.emacs.d\codemacs\init.el" -ItemType SymbolicLink -Target "$(Get-Location)\codemacs\init.el"

New-Item "$env:HOME\.emacs.d\mathmacs\early-init.el" -ItemType SymbolicLink -Target "$(Get-Location)\mathmacs\early-init.el"
New-Item "$env:HOME\.emacs.d\mathmacs\init.el" -ItemType SymbolicLink -Target "$(Get-Location)\mathmacs\init.el"
