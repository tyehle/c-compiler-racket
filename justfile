default:
    @just --list

rcc path:
    arch -x86_64 zsh -c 'racket rcc.rkt {{path}}'

shell:
    arch -x86_64 zsh

partial-compile arg path:
    arch -x86_64 zsh -c 'racket rcc.rkt {{arg}} {{path}}'

lex path: (partial-compile "--lex" path)
parse path: (partial-compile "--parse" path)
validate path: (partial-compile "--validate" path)
tacky path: (partial-compile "--tacky" path)
codegen path: (partial-compile "--codegen" path)
assemble path: (partial-compile "--assemble" path)

test-latest chapter stage="run": (test chapter stage "--latest-only --failfast")

test chapter stage="run" extra_args="":
    #!/usr/bin/env zsh
    set -euo pipefail
    extra_credit='--bitwise --compound --increment --goto --switch'
    cmd="raco make rcc.rkt && ./writing-a-c-compiler-tests/test_compiler \
        --verbose {{extra_args}} $extra_credit \
        --chapter={{chapter}} --stage={{stage}} \
        ./rcc.rkt"
    if [[ "{{stage}}" == "run" ]]
    then
        set -x
        arch -x86_64 zsh -c "$cmd"
    else
        set -x
        eval "$cmd"
    fi
