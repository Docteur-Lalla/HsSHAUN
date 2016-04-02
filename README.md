HsSHAUN
=======

HsSHAUN is a Haskell implementation of the SHAUN (Shaun HAtes Ugly Notations) object notation language created by Arthur Blanleuil and Pierre Le Luron. This language is a improvement of the JSON language featuring :

* Multiple comment delimiters : parentheses and C-style comments (simple and multiline)
* Unit for numbers (distance, angle, time, etc...)
* Object keys are symbols and not strings like in JSON

SHAUN recognizes five types of data :

* Numbers (every number is floating-point)
* Booleans
* Strings
* Heterogenous lists
* Object node

----------

HsSHAUN supplies Haskell with a full featured SHAUN interpreter allowing the user to retrieve, write and manipulate his datas. HsSHAUN is distributed under BSD 3-clauses license.
