ppl-accountability
==================

Compliance checker for PPL logs from the FI-WARE Data Handling Generic Enabler. Provides accountability feature.

FI-WARE SECURITY - DATA HANDLING GENERIC ENABLER - PPL LOG ANALYSER (ACCOUNTABILITY FEATURE)

Performs a compliance check for PPL logs in the current directory, against their associated 
sticky policies. The log files must have .log extensions; normally, the extensions are 
.mysql.log and .event.log.

COMPILATION:  		ghc loganalyser.hs

USAGE: 				./loganalyser piiId

Where piiId is an integer (the identifying number of the PII for which to perform the check)

The analyser reads all .log files in the current directory, sorts them, concatenates them,
parses them, and checks their compliance against the sticky policies which they contain.






This file last changed in June 2013.
