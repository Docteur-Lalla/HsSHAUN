if exists("b:current_syntax")
  finish
endif

syn match number '\d\+'
syn match number '[-+]\d\+'
syn match number '\d\+\.\d\+'
syn match number '[-+]\d\+\.\d\+'
syn match number '\d\+[eE]\d\+'
syn match number '[-+]\d\+[eE]\d\+'
syn match number '\d\+[eE][+-]\d\+'
syn match number '[-+]\d\+[eE][+-]\d\+'
syn match number '\d\+\.\d\+[eE]\d\+'
syn match number '[-+]\d\+\.\d\+[eE]\d\+'
syn match number '\d\+\.\d\+[eE][+-]\d\+'
syn match number '[-+]\d\+\.\d\+[eE][+-]\d\+'

syn match label '\w\+\s*:'

syn region comments start="(" end=")" fold
syn region comments start='/\*' end='\*/' fold
syn region comments start='//' end='\n' fold
syn region string start="\"" end="\"" fold

syn region block start="{" end="}" fold transparent
syn region list start="\[" end="\]" fold transparent

hi def link number Constant
hi def link string Constant
hi def link block Delimiter
hi def link list Delimiter
hi def link label Label
hi def link comments Comment
