# Elements to cut from dataset 
#   Because they represent just part of a larger element  
behaviors_cut_partial <- c("Male1 Half-bow Left",
                           "Bird2 Half-bow Left",
                           "Male1 BowLeft",
                           "Bird2 BowLeft",
                           "Male1 TafLF_Off",
                           "Male1 Metronome_Right",
                           "Bird2 TafLF_Off")

# Elements to cut from dataset
#   Because they represent movement tracking only
behaviors_cut_movement <- c("Female Looking Away",
                            "Female Tracking Male", 
                            "Female ResponseToALAD", 
                            "FemaleSwitch",       
                            "Female Movement", 
                            "Male1 On Log",
                            "Male1 Off Log")

# Elements to cut from dataset because they are used for tracking
behaviors_cut_tracking <- c("Start", 
                            "End", 
                            "Female On Log", 
                            "Female Off Log")

# Elements to cut from dataset
#   Because they do not directly constitute dance display behaviors
behaviors_cut_other <- c("Male1 Other Behavior Vocalization", 
                         "Male1 Other Behavior Gardening")

behaviorCodes_original <- c("Male1 On Log No Display"             = "Zro",
                            "Male1 ALAD"                          = "ALAD",
                            "Male1 SLAD"                          = "SLAD",
                            "Male1 BowRight"                      = "Bow",
                            "Male1 Half-bow Right"                = "HafB",
                            "Male1 HeadDownBowing"                = "HdBw",
                            "Male1 Metronome_Left"                = "Metr",
                            "Male1 Switch"                        = "Swtc",
                            "Male1 NeckTwist"                     = "Neck",
                            "Male1 TafLF_On"                      = "Taf",
                            "Male1 Mixed Element"                 = "Mix",
                            "Male1 Other Behavior Wing Flash"     = "Othr",
                            "Male1 Other Behavior Unspecified"    = "Othr",
                            "Male1 Other Behavior Unknown"        = "Othr",
                            "Bird2 On-log NO display"             = "B2Zr",
                            "Bird2 ALAD"                          = "B2AL",
                            "Bird2 SLAD"                          = "B2SL",
                            "Bird2 BowRight"                      = "B2Bw",
                            "Bird2 Half-bow Right"                = "B2Hf",
                            "Bird2 HeadDownBowing"                = "B2Hd",
                            "Bird2 NeckTwist"                     = "B2Nk",
                            "Bird2 TafLF_On"                      = "B2Tf",
                            "Bird2 Mixed Element"                 = "B2Mx",
                            "Attempted Copulation"                = "AttC",
                            "Copulation"                          = "Cop")

behaviorCodes_short <- c("Zro"  = "A",
                         "ALAD" = "B",
                         "SLAD" = "C",
                         "Bow"  = "D",
                         "HafB" = "E",
                         "HdBw" = "F",
                         "Metr" = "G",
                         "Swtc" = "H",
                         "Neck" = "I",
                         "Taf"  = "J",
                         "Mix"  = "K",
                         "Othr" = "L",
                         "B2Zr" = "M",
                         "B2AL" = "N",
                         "B2SL" = "O",
                         "B2Bw" = "P",
                         "B2Hf" = "Q",
                         "B2Hd" = "R",
                         "B2Nk" = "S",
                         "B2Tf" = "T",
                         "B2Mx" = "U",
                         "AttC" = "V",
                         "Cop"  = "W")