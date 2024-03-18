Files needed for the Finland example.  

The file *Finland_clean_allCC.txt.good* was output using the code *onestatphaserel2abs* available from  

https://github.com/stevenjgibbons/onestatphaserel2abs

but initial runs using the code  *fixedSlovecsEventSolve* showed that the phase S1 at station LP34 for event H20 gives very high residuals for all interactions so we make a new input file *Finland_clean_allCC.txt* by running  

```
comment_out_phases.sh
```
where the file *exclude.txt* contains the following

```
LP34   S1   H20
```

All lines including event H20 for station LP34 and phase S1 are then removed from the input file.
We just add extra lines to *exclude.txt* if there are other phases we do not want.  

