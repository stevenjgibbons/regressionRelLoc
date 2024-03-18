Directory containing example files for the first 6 North Korean nuclear tests  

The file *DPRK_clean_allCC.txt.good* was output using the code *onestatphaserel2abs* available from  

https://github.com/stevenjgibbons/onestatphaserel2abs

but initial runs using the code  *fixedSlovecsEventSolve* showed that the phase P at station YKA for event DPRK1 gives very high residuals for all interactions so we make a new input file *DPRK_clean_allCC.txt* by running  

```
comment_out_phases.sh
```
where the file *exclude.txt* contains the following

```
YKA   P   DPRK1
```

All lines including event DPRK1 for station YKA and phase P are then removed from the input file.
We just add extra lines to *exclude.txt* if there are other phases we do not want.  
