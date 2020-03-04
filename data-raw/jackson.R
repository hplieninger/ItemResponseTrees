library("dplyr")

df1 <- read.csv(url("https://ndownloader.figshare.com/files/100890"),
                stringsAsFactors = FALSE, na.strings = c("NULL", "0"))

rev_items <- c("q5", "q15", "q25", "q35", "q45", "q1", "q11", "q21", "q31",
               "q7", "q17", "q27", "q37", "q8", "q18", "q9", "q19", "q29")

E <- c("q0", "q5", "q10", "q15", "q20", "q25", "q30", "q35", "q40", "q45")
# N <- c("q1", "q6", "q11", "q16", "q21", "q26", "q31", "q36", "q41", "q46")
A <- c("q1", "q6", "q11", "q16", "q21", "q26", "q31", "q36", "q41", "q46")
# A <- c("q2", "q7", "q12", "q17", "q22", "q27", "q32", "q37", "q42", "q47")
C <- c("q2", "q7", "q12", "q17", "q22", "q27", "q32", "q37", "q42", "q47")
# C <- c("q3", "q8", "q13", "q18", "q23", "q28", "q33", "q38", "q43", "q48")
N <- c("q3", "q8", "q13", "q18", "q23", "q28", "q33", "q38", "q43", "q48")
O <- c("q4", "q9", "q14", "q19", "q24", "q29", "q34", "q39", "q44", "q49")

df2 <- df1 %>%
    mutate_at(vars(starts_with("q")), na_if, y = 0) %>%
    mutate_at(rev_items, ~6 - .x) %>%
    mutate(gender = factor(gender, levels = 1:3, labels = c("male", "female", "other"), exclude = 0)) %>%
    labelled::set_variable_labels(
        "q0" =  "Am the life of the party.",
        "q1" =  "Feel little concern for others.",
        "q2" =  "Am always prepared.",
        "q3" =  "Get stressed out easily.",
        "q4" =  "Have a rich vocabulary.",
        "q5" =  "Don't talk a lot.",
        "q6" =  "Am interested in people.",
        "q7" =  "Leave my belongings around.",
        "q8" =  "Am relaxed most of the time.",
        "q9" =  "Have difficulty understanding abstract ideas.",
        "q10" = "Feel comfortable around people.",
        "q11" = "Insult people.",
        "q12" = "Pay attention to details.",
        "q13" = "Worry about things.",
        "q14" = "Have a vivid imagination.",
        "q15" = "Keep in the background.",
        "q16" = "Sympathize with others' feelings.",
        "q17" = "Make a mess of things.",
        "q18" = "Seldom feel blue.",
        "q19" = "Am not interested in abstract ideas.",
        "q20" = "Start conversations.",
        "q21" = "Am not interested in other people's problems.",
        "q22" = "Get chores done right away.",
        "q23" = "Am easily disturbed.",
        "q24" = "Have excellent ideas.",
        "q25" = "Have little to say.",
        "q26" = "Have a soft heart.",
        "q27" = "Often forget to put things back in their proper place.",
        "q28" = "Get upset easily.",
        "q29" = "Do not have a good imagination.",
        "q30" = "Talk to a lot of different people at parties.",
        "q31" = "Am not really interested in others.",
        "q32" = "Like order.",
        "q33" = "Change my mood a lot.",
        "q34" = "Am quick to understand things.",
        "q35" = "Don't like to draw attention to myself.",
        "q36" = "Take time out for others.",
        "q37" = "Shirk my duties.",
        "q38" = "Have frequent mood swings.",
        "q39" = "Use difficult words.",
        "q40" = "Don't mind being the center of attention.",
        "q41" = "Feel others' emotions.",
        "q42" = "Follow a schedule.",
        "q43" = "Get irritated easily.",
        "q44" = "Spend time reflecting on things.",
        "q45" = "Am quiet around strangers.",
        "q46" = "Make people feel at ease.",
        "q47" = "Am exacting in my work.",
        "q48" = "Often feel blue.",
        "q49" = "Am full of ideas."
    )

names(df2)[names(df2) %in% E] <- paste0("E", 1:10)
names(df2)[names(df2) %in% N] <- paste0("N", 1:10)
names(df2)[names(df2) %in% A] <- paste0("A", 1:10)
names(df2)[names(df2) %in% C] <- paste0("C", 1:10)
names(df2)[names(df2) %in% O] <- paste0("O", 1:10)

jackson <- as_tibble(df2)

usethis::use_data(jackson, overwrite = TRUE, version = 2)
