default:
    @just --list

rcc path target="x86":
    racket rcc.rkt --target {{target}} {{path}}

partial-compile arg path target="x86":
    racket rcc.rkt --target {{target}} {{arg}} {{path}}

lex path: (partial-compile "--lex" path)
parse path: (partial-compile "--parse" path)
validate path: (partial-compile "--validate" path)
tacky path: (partial-compile "--tacky" path)
codegen path target="x86": (partial-compile "--codegen" path target)
assemble path target="x86": (partial-compile "--assemble" path target)

test-latest chapter stage="run" target="x86": (test chapter stage target "--latest-only --failfast")

test chapter stage="run" target="x86" extra_args="":
    #!/usr/bin/env zsh
    set -euxo pipefail
    extra_credit=(--bitwise --compound --increment --goto --switch)
    raco make rcc.rkt
    ./writing-a-c-compiler-tests/test_compiler \
        --verbose {{extra_args}} $extra_credit \
        --chapter={{chapter}} --stage={{stage}} \
        ./rcc.rkt -- --target {{target}}
