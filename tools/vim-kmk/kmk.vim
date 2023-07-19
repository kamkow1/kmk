if exists("b:current_syntax")
  finish
endif

setlocal iskeyword+=$

syn case match

syn region kmkLineComment start=/#/ end=/$/
syn keyword kmkKeyword function begin end set unset when
syn match kmkIdent "[a-zA-Z][a-zA-Z0-9]*"
syn region kmkString start=+"+ skip=+\\\\\|\\"\|\\$+ excludenl end=+"+ end=+$+ keepend contains=kmkEscape
syn match kmkEscape +\\[abfnrtv'"\\]+ contained

hi def link kmkLineComment Comment
hi def link kmkKeyword Keyword
hi def link kmkIdent Identifier
hi def link kmkString String
