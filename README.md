hw01
================
Brendan Zimmer
9/22/2021

\#Goes to <https://github.com/brendanzim11/stats433-homework>

\#Library

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)
```

\#download text file from Bridges of all states in 2020

``` r
df = read.delim("2020HwyBridgesDelimitedAllStates.txt", header = TRUE, sep = ",")
head(df)
```

    ##   STATE_CODE_001 STRUCTURE_NUMBER_008 RECORD_TYPE_005A ROUTE_PREFIX_005B
    ## 1             01      00000000000S702                1                 6
    ## 2             01      00000000000S703                1                 6
    ## 3             01      0000000000M0022                1                 8
    ## 4             01      000000883039900                1                 4
    ## 5             01      000001014002450                1                 3
    ## 6             01      000001331700710                1                 6
    ##   SERVICE_LEVEL_005C ROUTE_NUMBER_005D DIRECTION_005E HIGHWAY_DISTRICT_002
    ## 1                  1             04007              0                   09
    ## 2                  1             04003              0                   09
    ## 3                  1             00000              0                   00
    ## 4                  1             00088              0                   02
    ## 5                  1             00101              0                   02
    ## 6                  2             00000              0                   02
    ##   COUNTY_CODE_003 PLACE_CODE_004        FEATURES_DESC_006A
    ## 1              53              0           'PERDIDO CREEK'
    ## 2              53           3004           'PERDIDO CREEK'
    ## 3             113          19000             'UCHEE CREEK'
    ## 4              59              0 'LITTLE BEAR CR. DAM SPW'
    ## 5              79              0         'TENNESSEE RIVER'
    ## 6              33              0         'TENNESSEE RIVER'
    ##   CRITICAL_FACILITY_006B FACILITY_CARRIED_007              LOCATION_009
    ## 1                          'IRR BIA RTE 4007' '11.4KM NW OF ATMORE  AL'
    ## 2                          'IRR BIA RTE 4003'      'ON LYNN MCGEE ROAD'
    ## 3                         '101ST AIRBORNE RD' '3.4 KM S OF SUNSHINE RD'
    ## 4                                 'CO. RD 88'      'LITTLE BEAR CR DAM'
    ## 5                                    'SR 101'          'ON WHEELER DAM'
    ## 6                                    'Res Rd'           'ON WILSON DAM'
    ##   MIN_VERT_CLR_010 KILOPOINT_011 BASE_HWY_NETWORK_012 LRS_INV_ROUTE_013A
    ## 1            99.99           1.5                    0         0000000000
    ## 2            99.99           1.1                    0         0000000000
    ## 3            99.99             0                    0                   
    ## 4            99.99             0                    0                   
    ## 5             4.52        39.429                    0                   
    ## 6            99.99             0                    0                   
    ##   SUBROUTE_NO_013B  LAT_016 LONG_017 DETOUR_KILOS_019 TOLL_020 MAINTENANCE_021
    ## 1                0 31061094 87341348                3        3              62
    ## 2                0 31062020 87340890                3        3              62
    ## 3               NA 32174330  -845837               18        3              74
    ## 4               NA 34270600 87581200               16        3              67
    ## 5               NA 34481800 87225400               42        3              67
    ## 6               NA 34480000 87373000                6        3              67
    ##   OWNER_022 FUNCTIONAL_CLASS_026 YEAR_BUILT_027 TRAFFIC_LANES_ON_028A
    ## 1        62                    9           1999                     2
    ## 2        62                    9           2002                     2
    ## 3        74                    9           1942                     2
    ## 4        67                    8           1974                     2
    ## 5        67                    6           1937                     2
    ## 6        67                   17           1924                     2
    ##   TRAFFIC_LANES_UND_028B ADT_029 YEAR_ADT_030 DESIGN_LOAD_031 APPR_WIDTH_MT_032
    ## 1                      0      50         2019               5               9.7
    ## 2                      0     159         2019               5               6.5
    ## 3                      0     375         2017               4              10.4
    ## 4                      0     430         2017               5              11.6
    ## 5                      0    5520         2017               4               7.9
    ## 6                      2    3620         2018               3               7.9
    ##   MEDIAN_CODE_033 DEGREES_SKEW_034 STRUCTURE_FLARED_035 RAILINGS_036A
    ## 1               0               30                    0             1
    ## 2               0                0                    0             1
    ## 3               0                0                    0             0
    ## 4               0               99                    0             1
    ## 5               0                0                    0             1
    ## 6               0                0                    0             1
    ##   TRANSITIONS_036B APPR_RAIL_036C APPR_RAIL_END_036D HISTORY_037 NAVIGATION_038
    ## 1                1              1                  1           5              0
    ## 2                1              1                  1           5              0
    ## 3                0              1                  1           5              0
    ## 4                1              1                  1           5              N
    ## 5                0              0                  0           4              1
    ## 6                1              1                  1           1              1
    ##   NAV_VERT_CLR_MT_039 NAV_HORR_CLR_MT_040 OPEN_CLOSED_POSTED_041
    ## 1                   0                   0                      A
    ## 2                   0                   0                      A
    ## 3                   0                   0                      A
    ## 4                   0                   0                      A
    ## 5                17.9                33.5                      A
    ## 6                17.3                33.5                      A
    ##   SERVICE_ON_042A SERVICE_UND_042B STRUCTURE_KIND_043A STRUCTURE_TYPE_043B
    ## 1               1                5                   5                   5
    ## 2               1                5                   5                   1
    ## 3               1                5                   1                   4
    ## 4               1                9                   5                   5
    ## 5               5                5                   3                  10
    ## 6               5                6                   4                   3
    ##   APPR_KIND_044A APPR_TYPE_044B MAIN_UNIT_SPANS_045 APPR_SPANS_046
    ## 1              0              0                   1              0
    ## 2              0              0                   1              0
    ## 3              0              0                   3              0
    ## 4              0              0                   5              0
    ## 5              3              2                   2            143
    ## 6              1             20                   8             78
    ##   HORR_CLR_MT_047 MAX_SPAN_LEN_MT_048 STRUCTURE_LEN_MT_049 LEFT_CURB_MT_050A
    ## 1             9.7                14.7                 15.0               0.0
    ## 2             9.7                12.9                 13.6               0.3
    ## 3             7.3                18.3                 43.0               0.6
    ## 4             8.6                14.0                 65.5               0.0
    ## 5             6.0                53.3               1981.2               0.9
    ## 6             6.0                46.9               1512.4               1.7
    ##   RIGHT_CURB_MT_050B ROADWAY_WIDTH_MT_051 DECK_WIDTH_MT_052
    ## 1                0.0                  9.7               9.7
    ## 2                0.3                  9.1               9.8
    ## 3                0.6                  7.3                 9
    ## 4                0.0                  8.7              10.5
    ## 5                0.2                  6.1               7.8
    ## 6                0.5                  6.1               8.3
    ##   VERT_CLR_OVER_MT_053 VERT_CLR_UND_REF_054A VERT_CLR_UND_054B LAT_UND_REF_055A
    ## 1                99.99                     N                 0                N
    ## 2                99.99                     N                 0                N
    ## 3                99.99                     N                 0                N
    ## 4                99.99                     N                 0                N
    ## 5                 4.52                     N                 0                N
    ## 6                99.99                     H             12.19                H
    ##   LAT_UND_MT_055B LEFT_LAT_UND_MT_056 DECK_COND_058 SUPERSTRUCTURE_COND_059
    ## 1               0                   0             8                       8
    ## 2               0                   0             8                       8
    ## 3               0                   0             5                       5
    ## 4               0                   0             7                       7
    ## 5               0                   0             5                       6
    ## 6               6                   0             5                       5
    ##   SUBSTRUCTURE_COND_060 CHANNEL_COND_061 CULVERT_COND_062 OPR_RATING_METH_063
    ## 1                     7                6                N                   2
    ## 2                     7                7                N                   2
    ## 3                     6                6                N                   2
    ## 4                     7                7                N                   1
    ## 5                     5                9                N                   1
    ## 6                     5                8                N                   1
    ##   OPERATING_RATING_064 INV_RATING_METH_065 INVENTORY_RATING_066
    ## 1                 44.5                   2                 32.7
    ## 2                 84.8                   2                 35.1
    ## 3                 51.7                   2                 37.2
    ## 4                 44.2                   1                 26.6
    ## 5                   40                   1                 23.9
    ## 6                 58.4                   1                   35
    ##   STRUCTURAL_EVAL_067 DECK_GEOMETRY_EVAL_068 UNDCLRENCE_EVAL_069
    ## 1                   7                      7                   N
    ## 2                   7                      6                   N
    ## 3                   5                      5                   N
    ## 4                   6                      5                   N
    ## 5                   5                      2                   N
    ## 6                   5                      2                   9
    ##   POSTING_EVAL_070 WATERWAY_EVAL_071 APPR_ROAD_EVAL_072 WORK_PROPOSED_075A
    ## 1                5                 8                  8                   
    ## 2                5                 7                  8                   
    ## 3                5                 8                  6                 38
    ## 4                5                 9                  6                   
    ## 5                5                 8                  8                 31
    ## 6                5                 9                  3                 31
    ##   WORK_DONE_BY_075B IMP_LEN_MT_076 DATE_OF_INSPECT_090 INSPECT_FREQ_MONTHS_091
    ## 1                NA              0                 219                      24
    ## 2                NA              0                 219                      24
    ## 3                 1           52.8                1019                      24
    ## 4                NA                                618                      24
    ## 5                 1         1981.2                 619                      24
    ## 6                 1         1512.4                 618                      24
    ##   FRACTURE_092A UNDWATER_LOOK_SEE_092B SPEC_INSPECT_092C
    ## 1           N                      N                 N  
    ## 2           N                      N                 N  
    ## 3           N                      Y60               N  
    ## 4           N                      N                 N  
    ## 5           Y24                    N                 N  
    ## 6           Y24                    N                 N  
    ##   FRACTURE_LAST_DATE_093A UNDWATER_LAST_DATE_093B SPEC_LAST_DATE_093C
    ## 1                                                                    
    ## 2                                                                    
    ## 3                                            1017                    
    ## 4                                                                    
    ## 5                    0619                                            
    ## 6                    0618                                            
    ##   BRIDGE_IMP_COST_094 ROADWAY_IMP_COST_095 TOTAL_IMP_COST_096 YEAR_OF_IMP_097
    ## 1                   0                    0                  0            2019
    ## 2                   0                    0                  0            2019
    ## 3                   1                    1                  2            2019
    ## 4                  NA                   NA                 NA                
    ## 5               50000                 2000              60000                
    ## 6               35000                 2000              45000                
    ##   OTHER_STATE_CODE_098A OTHER_STATE_PCNT_098B OTHR_STATE_STRUC_NO_099
    ## 1                                           0                        
    ## 2                                           0                        
    ## 3                                                                    
    ## 4                                                                    
    ## 5                                                                    
    ## 6                                                                    
    ##   STRAHNET_HIGHWAY_100 PARALLEL_STRUCTURE_101 TRAFFIC_DIRECTION_102
    ## 1                    0                      N                     2
    ## 2                    0                      N                     2
    ## 3                    0                      N                     2
    ## 4                    0                      N                     2
    ## 5                    0                      N                     2
    ## 6                    0                      N                     2
    ##   TEMP_STRUCTURE_103 HIGHWAY_SYSTEM_104 FEDERAL_LANDS_105
    ## 1                                     0                 1
    ## 2                                     0                 1
    ## 3                                     0                 0
    ## 4                                     0                 0
    ## 5                                     0                 0
    ## 6                                     0                 0
    ##   YEAR_RECONSTRUCTED_106 DECK_STRUCTURE_TYPE_107 SURFACE_TYPE_108A
    ## 1                      0                       2                 6
    ## 2                      0                       2                 6
    ## 3                      0                       1                 1
    ## 4                      0                       1                 0
    ## 5                   1962                       1                 0
    ## 6                   1958                       1                 5
    ##   MEMBRANE_TYPE_108B DECK_PROTECTION_108C PERCENT_ADT_TRUCK_109
    ## 1                  0                    1                     1
    ## 2                  0                    8                     1
    ## 3                  0                    0                    35
    ## 4                  0                    0                     5
    ## 5                  0                    0                    10
    ## 6                  0                    0                     8
    ##   NATIONAL_NETWORK_110 PIER_PROTECTION_111 BRIDGE_LEN_IND_112
    ## 1                    0                                      Y
    ## 2                    0                                      Y
    ## 3                    0                                      Y
    ## 4                    0                                      Y
    ## 5                    0                   1                  Y
    ## 6                    0                   1                  Y
    ##   SCOUR_CRITICAL_113 FUTURE_ADT_114 YEAR_OF_FUTURE_ADT_115 MIN_NAV_CLR_MT_116
    ## 1                  8             59                   2039                  0
    ## 2                  8            200                   2039                  0
    ## 3                  5            400                   2039                   
    ## 4                  5            600                   2037                   
    ## 5                  5           9000                   2037                   
    ## 6                  5           2500                   2038                   
    ##   FED_AGENCY SUBMITTED_BY BRIDGE_CONDITION LOWEST_RATING DECK_AREA
    ## 1          Y           62                G             7     145.5
    ## 2          Y           62                G             7    133.28
    ## 3          Y           74                F             5       387
    ## 4          Y           67                G             7    687.75
    ## 5          Y           67                F             5  15453.36
    ## 6          Y           67                F             5  12552.92

\#Selected columns that I found interesting and thought would make a
good graph

``` r
new_df <- df %>% select(STATE_CODE_001, MAINTENANCE_021, DESIGN_LOAD_031, RAILINGS_036A, YEAR_RECONSTRUCTED_106, BRIDGE_CONDITION, YEAR_BUILT_027)

head(new_df)
```

    ##   STATE_CODE_001 MAINTENANCE_021 DESIGN_LOAD_031 RAILINGS_036A
    ## 1             01              62               5             1
    ## 2             01              62               5             1
    ## 3             01              74               4             0
    ## 4             01              67               5             1
    ## 5             01              67               4             1
    ## 6             01              67               3             1
    ##   YEAR_RECONSTRUCTED_106 BRIDGE_CONDITION YEAR_BUILT_027
    ## 1                      0                G           1999
    ## 2                      0                G           2002
    ## 3                      0                F           1942
    ## 4                      0                G           1974
    ## 5                   1962                F           1937
    ## 6                   1958                F           1924

\#(Need to condense) BRIDGE\_CONDITION is the current condition of the
bridge with G being ‘Good’, F being “Fair”, P being ‘Poor’ and replaces
them with 3,2,1

``` r
numdf <- as.data.frame(new_df)
numdf$BRIDGE_CONDITION <- gsub('G', '3', numdf$BRIDGE_CONDITION)
numdf$BRIDGE_CONDITION <- gsub('P', '1', numdf$BRIDGE_CONDITION)
numdf$BRIDGE_CONDITION <- gsub('F', '2', numdf$BRIDGE_CONDITION)
```

\#Filtered further to only be states Wisconsin and Illinois for possible
comparison

``` r
numdf <- numdf %>% filter(STATE_CODE_001 == "55" | STATE_CODE_001 == "17")
```

\#Changed values in dataset to numeric and deleted outliers that were
disrupting the graph

``` r
numdf <- as.data.frame(apply(numdf, 2, as.numeric))
```

    ## Warning in apply(numdf, 2, as.numeric): NAs introduced by coercion

    ## Warning in apply(numdf, 2, as.numeric): NAs introduced by coercion

    ## Warning in apply(numdf, 2, as.numeric): NAs introduced by coercion

``` r
numdf<-numdf[!(numdf$YEAR_BUILT_027=="2" | numdf$YEAR_BUILT_027=="3" | numdf$YEAR_BUILT_027=="9"),]
```

\#Plot of maintenance and year built, this one I found the most
interesting and really enjoyed. It shows a clear increase in maintenance
in the 1960s after they were built. This makes me wonder if older
bridges may be more sturdy or less used, or even destroyed. There was an
outlier for the maintence being in the 6000 range, which added a skew to
the graph, but not majorly.

``` r
numdf %>% ggplot(aes(y=MAINTENANCE_021, x=YEAR_BUILT_027)) +
  geom_bar(stat="identity") + ggtitle("Maintenance vs Year Built in 2020")
```

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

\#I think this was a great project, but I have found places that I will
need to improve on for large projects
