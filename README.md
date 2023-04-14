# Köppen-Geiger Climate Classification
An R function that generates a **Köppen-Geiger climate classification** in a raster format from rasters of monthly mean temperature and precipitations.

### Input
Monthly mean temperature (in Degrees Celsius) and precipitations (in millimetres), provided in a **raster format**. 

### Arguments
Classi.KG(*Temp*, *Prec*, *level* = 3)

- *Temp*: a list of the 12 rasters of temperature (ranked from January to December)
- *Prec*: a list of the 12 rasters of precipitations (ranked from January to December)
- *level*: 1, 2 or 3 (3 by default). Level of Köppen-Geiger climate classification.

### Output
A raster with each integer corresponding to a single climate class:

#### Level 1
- 100: A
- 200: B
- 300: C
- 400: D
- 500: E

#### Level 2
- 110: Af
- 120: Am
- 130: Aw
- 210: BW
- 220: BS
- 310: Cs
- 320: Cw
- 330: Cf
- 410: Ds
- 420: Dw
- 430: Df
- 510: ET
- 520: EF

#### Level 3
- 110: Af
- 120: Am
- 130: Aw
- 211: BWh
- 212: BWk
- 221: BSh
- 222: BSk
- 311: Csa
- 312: Csb
- 313: Csc
- 321: Cwa
- 322: Cwb
- 323: Cwc
- 331: Cfa
- 332: Cfb
- 333: Cfc
- 411: Dsa
- 412: Dsb
- 413: Dsc
- 414: Dsd
- 421: Dwa
- 422: Dwb
- 423: Dwc
- 424: Dwd
- 431: Dfa
- 432: Dfb
- 433: Dfc
- 434: Dfd
- 510: ET
- 520: EF

### Version of the Köppen-Geiger classification
This function uses the Köppen-Geiger classification provided by Peel *et al.* (2007) : https://hess.copernicus.org/articles/11/1633/2007/
