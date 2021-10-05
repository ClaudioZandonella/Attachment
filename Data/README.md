## Data README

Dataset used in the analysis is saved in `data_CR.rds`. The dataset is composed by 854 observations on 39 variables (note that variables names are partially in Italian):

- `ID` - factor variable indicating children unique ID (854 levels)
- `ìd_classe` - factor variable indicating classroom unique ID (50 levels)
- `classe` - numeric variable indicating children school grade (from 3 to 6)
- `genere` - factor variable indicating children gender (`"F"` for females and `"M"` for males)
- `age_year` - numeric variable indicating children age in years
- `externalizing_sum` - numeric variable indicating children externalizing problems total score
- `internalizing_sum` - numeric variable indicating children internalizing problems total score
- `o_ecr\*m` (from 1 to 12) - set of numeric variables indicating children response to ECR items regarding mother attachment (“*madre*” in Italian)
- `o_ecr\*p` (from 1 to 12) - set of numeric variables indicating children response to ECR items regarding father attachment (“*padre*” in Italian)
- `Avm` - numeric variable indicating children *Avoidance* score regarding mother attachment (“*madre*” in Italian)
- `Avp` - numeric variable indicating children *Avoidance* score regarding father attachment (“*padre*” in Italian)
- `Anxm` - numeric variable indicating children *Anxiety* score regarding mother attachment (“*madre*” in Italian)
- `Anxp` - numeric variable indicating children *Anxiety* score regarding father attachment (“*padre*” in Italian)
