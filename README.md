# Urban bird assemblages are shaped by the regional pool
R scripts supporting the study "Urban bird assemblages are shaped by the regional pool" by C. Monchy &amp; A. Rodrigues

### Data processsing
##### 1_Data_Extraction.R
    - Input : data downloaded from eBird database (checklists + observations) between 2015 and 2021
    - Filtering data according criteria : complete checklists, geographical area in America, protocol (stationnary and transect), duration (15min and 5h), transect distance (less than 5km).
    - Output : first round of selected data + list of "valid" observers (with at least 10 complete checklists across the continent)
    
2_Data_Split.R
    • Input : données pour toutes les amériques (non-disponible) 
    • Sépare les données par écorégion
    • Filtre des listes (sampling) selon : nbre d’observateurs (≤2) dont un « valide », période de l’année
    • Filtre des observations (ebd) selon : l’espèce (voir acceptedTaxa.txt)
    • Output : 2_Splitting (283 éco-régions sur 297)
3_Calculation_Urban-Rich.R
    - Input : 2_Splitting
    • Filtrage des listes selon : nbre d’observateur (1seul)
    • Calcul du taux moyen d’urbanisation avec des buffers de 1 et 3km autour du point de la liste et classification
    • Filtrage des listes avec taux d’urbanisation à 1km == NA (≠0) (en mer, ou lac)
    • Calcul du score à partir de la richesse des listes
    • Output : 3_Calculation/Urban_Calc
3bis_Filtering_season.R
    • Input : 3_Calculation/Urban_Calc
    • Filtrage sur la période en fonction du centroïde de l’écorégion
    • Output : 3_Calculation et 3_Calculation/bis (236 écorégions)
4_Filtering_ScoreDurationDistance.R
    • Input : 3_Calculation et 3_Calculation/bis
    • Filtrage des listes et observations selon : la durée (15min et 1h), la distance (≤2km), le score des obs
    • Définition du milieu « Urban », « NonUrban » et NotClassified »
    • Output : 4_Filtering et 4_Filtering/bis (226 écorégions)
5_Selection_UrbanNurban.R
    • Input : 4_Filtering et 4_Filtering/bis
    • Calcul de la distance à la côte, de l’altitude et appairage 
    • Output 5_Selecting : appairage des listes (urbaines et non-urbaines) pour 138 écorégions (dont 69 écorégions avec moins de 20 paires dans /tooSMALL) 5_Selecting
