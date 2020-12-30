
<!-- README.md is generated from README.Rmd. Please edit that file -->

# myStarship

The goal of myStarship is to give each student a unique dataset for an
assessment about linear mixed models. This is still a work in progress
and I have not yet trialled it with students.

![](https://hips.hearstapps.com/pop.h-cdn.co/assets/16/26/4000x2000/landscape-1467144815-starshipenterprise.jpg)
Image source: Smithsonian’s National Air and Space Museum

## Installation

You can install this package from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("elb0/myStarship")
```

## Example

This package assumes students have a numeric ID that is 8, 9 or 10
digits long, no letters or special characters.

`get_my_starship()` creates two objects in the environment.

1)  `ship_name` is a character vector with the ship’s name. It has the
    form “SS \_\_\_\_\_” where the blank is a randomly generated
    nonsense word.  
2)  `personnel_data` is a randomly generated dataset with information
    about the crew of the ship.

The same ship and crew will be generated as long as the same ID is used,
even across different sessions and machines.

``` r
library(myStarship)
library(tidyverse)

get_my_starship(1000000098)

ship_name
#> [1] "SS Pleeplull"

glimpse(personnel_data)
#> Rows: 3,012
#> Columns: 12
#> $ rank               <chr> "Captain", "Captain", "Captain", "Captain", "Capta…
#> $ position           <chr> "Captain", "Captain", "Captain", "Captain", "Capta…
#> $ division           <chr> "Command", "Command", "Command", "Command", "Comma…
#> $ sub_division       <chr> "Command", "Command", "Command", "Command", "Comma…
#> $ gender             <chr> "Masculine", "Masculine", "Masculine", "Masculine"…
#> $ name               <chr> "Christopher Padilla", "Christopher Padilla", "Chr…
#> $ duty_shift         <chr> "Alpha", "Alpha", "Alpha", "Alpha", "Alpha", "Alph…
#> $ shift_team         <chr> "Team 1", "Team 1", "Team 1", "Team 1", "Team 1", …
#> $ starfleet_gpa      <dbl> 6.99, 6.99, 6.99, 6.99, 6.99, 6.99, 6.99, 6.99, 6.…
#> $ perseverance_score <dbl> 7.83, 7.83, 7.83, 7.83, 7.83, 7.83, 7.83, 7.83, 7.…
#> $ week               <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 1, 2, 3, 4,…
#> $ productivity       <dbl> 28.87447, 30.85021, 30.55453, 30.06952, 32.65455, …
```

# The ‘context’

This data is Star Trek ‘inspired’ but probably horrifically wrong to
fans less ‘casual’ than me.

## Draft preamble for students

You are the Chief Science Officer of the SS \_\_\_\_ (e.g. SS
Pleeplull). You have data about the productivity of the crew over a 12
week period after a shore leave (a holiday break for the crew). For each
member of the crew you also have data on their `rank` within Starfleet,
their role on the ship (`position`), which of the three main divisions
(`division`) they are in (Command, Operations, Science), as well as
their sub-division (`sub_division`, e.g. Engineering is a sub-division
of Operations). You also know their `gender` (Feminine, Masculine,
Non-binary), `name`, which duty shift (`duty_shift`) they are assigned
to (there are four 8-hour shifts covering each 24 hour period, Alpha,
Beta, Delta and Gamma), what team within that shift they are assigned to
(`shift_team`, Team 1 to 6, or sometimes fewer), what their GPA upon
graduating from Starfleet Academy was (`starfleet_gpa`, 0-10 scale, 10
being the best grade), their perseverance score (`perseverance_score`)
from their most recent psych assessment (0-10 scale, 10 being high
perseverance). `week` indicates the weeks since the shore leave (1 to
12) and their `productivity` score for each week is recorded.

Your goal is to better understand productivity aboard your ship.

# Outstanding questions/ideas

  - How to autograde assessments is still something I am working on, but
    very motivated to solve because I’d like to do this with my 600
    third years.
      - I haven’t done this yet, but I think providing students with a
        personalized mark scheme should be pretty easy: at simplest,
        provide an .Rmd template that students can add their student ID
        to and knit, at most complicated, use a mail merge workflow.
  - I’m still extremely new to making packages so there probably soooo
    many ways to make this more light weight and not rely so heavily on
    the tidyverse…but this works well for my purposes.
  - Originally I’d thought about having the student provide their name
    as an input to the function and having them represented in the data,
    but then I added gender, and I also didn’t want them to be miffed if
    they were randomly assigned a bad score for any of the numeric
    variables.
  - I think there is still an eeny-weeny chance that a student will end
    up being the Science Officer on the SS \[swear word\], so I will be
    warning studnets that the names are auto-generated…

# Credits

The ‘initials’, ‘finals’ and ‘vowels’ components used to make starship
names are pulled from the gibberish database:
<https://github.com/greghaskins/gibberish>.

Thank you to my lovely sister, Meggie Bolton, for consulting on
tailoring the gibberish word components and letting me tell her all
about making this.
