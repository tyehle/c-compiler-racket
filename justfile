default:
    @just --list

rcc path:
    arch -x86_64 zsh -c 'racket rcc.rkt {{path}}'

partial-compile arg path:
    arch -x86_64 zsh -c 'racket rcc.rkt {{arg}} {{path}}'

lex path: (partial-compile "--lex" path)
parse path: (partial-compile "--parse" path)
codegen path: (partial-compile "--codegen" path)
assemble path: (partial-compile "--assemble" path)
