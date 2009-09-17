
for %%1 in (*.bmp) do bmptopnm %%1 | ppmquant 256 | ppmtogif > %%~n1.gif

