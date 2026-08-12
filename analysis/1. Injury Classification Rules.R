
# Injury Classification Rules
# This is my rulebook for turning the messy free-text injury reasons on the NBA
# reports into tidy categories the rest of my pipeline can use. I keep it in its
# own file and source it from both "2. Clean Injury Reports.R" and "4. Assemble
# Player-Game Panel.R" so the exact same rules are applied everywhere.
#
# The helpers I define here:
#   classify_injury(reason_lower)    -> my 24 injury categories, from a reason string
#   is_injury_absence_flag(category) -> TRUE if it's a genuine injury (not rest/illness)
#   is_structural_flag(category)     -> TRUE for the serious structural injuries
#   injury_body_region(category)     -> the anatomical region, so I can match reaggravations
#   injury_mechanism(category)       -> my three load-cause tiers (the negative-control split)

# classify_injury(): my main rule — I take a lowercased reason string and return
# whichever category matches first.
classify_injury <- function(reason_lower) {
  # First I strip the "injury/illness -" / "injury -" prefix that the NBA prepends
  # to every reason. I got caught out here: without this, the word "illness" in the
  # prefix fires my illness rule on genuine injuries like
  # "Injury/Illness - Left Ankle; Stress Fracture".
  reason_lower <- str_trim(str_remove(
    reason_lower,
    "^(injury/illness|injury|illness)\\s*-\\s*"
  ))
  
  case_when(
    
    # ── Non-injury absences ──────────────────────────────────────────────────
    str_detect(reason_lower,
               "g league|two-way|on assignment|two way") ~ "g_league",
    
    str_detect(reason_lower,
               "\\brest\\b|load management|maintenance") ~ "load_management",
    
    str_detect(reason_lower,
               "trade pending|league suspension|team suspension|ineligible|coach.s decision|personal reasons|family reasons") ~ "roster_move",
    
    str_detect(reason_lower,
               "not with team") ~ "not_with_team",
    
    str_detect(reason_lower,
               "return to competition reconditioning|reconditioning") ~ "reconditioning",
    
    str_detect(reason_lower,
               "health and safety protocols") ~ "illness",
    
    str_detect(reason_lower,
               "illness|non-covid|non covid|flu|influenza|flu-like|respiratory|gastrointestinal|gastroenteritis|stomach|virus|viral|asthma|migraine|headache|sinus|congestion|pharyngitis|strep|covid|dental|tooth|wisdom tooth|appendectomy|pneumothorax|deep vein thrombosis|dvt") ~ "illness",
    
    # ── Structural — most specific first ────────────────────────────────────
    str_detect(reason_lower,
               "\\bacl\\b|anterior cruciate|\\bpcl\\b|posterior cruciate") ~ "acl_pcl",
    
    str_detect(reason_lower,
               "achilles.*(tear|rupture|torn|repair|surgery|reconstruct|rehab)") ~ "achilles",
    
    str_detect(reason_lower,
               "meniscu|meniscal|meniscectomy|meniscotomy") ~ "meniscus",

    # ── Bone stress— I have to put this BEFORE my acute
    # fracture rule. Stress fractures/reactions and lower-limb/spine/foot bony
    # injuries come from accumulated LOAD, so I want them separate from impact
    # fractures (hand, nose, rib). I catch the explicit stress keywords first, then
    # any fracture at a load-bearing site.
    str_detect(reason_lower,
               "stress fracture|stress reaction|stress injury|bone stress|bone bruise|bone contusion|bone spur") ~ "bone_stress",
    str_detect(reason_lower,
               "(navicular|metatarsal|tibia|tibial|fibula|lower leg|\\bshin\\b|femoral|femur|\\bfoot\\b|\\bankle\\b|calcaneus|talus|lumbar|lower back|low back|sacral|sacrum|pelvi|iliac|\\bhip\\b).*fracture") ~ "bone_stress",

    # ── Acute (impact) fracture — upper body / face / ribs; basically random with
    # respect to load. By this point every load-site fracture is already caught
    # above, so anything still saying "...fracture" I treat as an impact fracture
    # (hand, wrist, nose, jaw, rib, etc.).
    str_detect(reason_lower,
               "fracture|hairline fracture|non.displaced fracture|scaphoid|metacarpal.*fracture|orbital|nasal|facial|jaw|sternum|scapula|clavicle") ~ "fracture",

    str_detect(reason_lower,
               "dislocation|dislocated|subluxation|shoulder.*dislocated") ~ "dislocation",
    
    str_detect(reason_lower,
               "\\bucl\\b|mcl.*tear|mcl.*sprain|labral.*tear|labrum.*tear|labral.*repair|rotator cuff.*tear|rotator cuff.*torn|lisfranc.*tear|wrist.*ligament.*tear|thumb.*ligament.*tear|central slip.*tear|extensor.*tear|extensor.*rupture|patellar.*tendon.*rupture|patellar.*tendon.*torn|adductor.*torn|adductor.*tear|oblique.*torn|oblique.*tear|thumb.*ucl|scapholunate|trochlea.*tear|trochlea.*cartilage") ~ "ligament_tear",
    
    # ── Soft tissue 
    str_detect(reason_lower,
               "achilles|achillies") ~ "calf_achilles_complex",
    
    str_detect(reason_lower,
               "hamstring") ~ "hamstring",
    
    str_detect(reason_lower,
               "\\bcalf\\b|soleus|lower leg.*strain|lower leg.*muscle|popliteus|peroneal.*strain|peroneal.*soreness|tibialis.*strain|tibialis.*tendon|posterior tibialis|periostitis|shin.*stress|shin.*fracture|shin.*soreness|shin.*contusion|fibula.*soreness") ~ "calf_achilles_complex",
    
    str_detect(reason_lower,
               "\\bquad\\b|quadricep|quadriceps|thigh.*strain|thigh.*contusion|thigh.*soreness|patellar.*tendin|patellofemoral|patellar.*contusion|patella.*tendin|patella.*femoral|quad.*tendon|quadricep.*tendon") ~ "quad_thigh",
    
    str_detect(reason_lower,
               "groin|adductor|hip flexor|hip.*flexor|sports hernia|hernia|oblique|abdominal.*strain|abdominal.*tendinopathy|core.*muscle|pelvic.*core|lower abdominal|rectus abdomin|abdominal|sacroiliac|si joint|sacral|sacrum|tailbone|glut|gluteal|gluteus|ischial") ~ "groin_adductor",
    
    str_detect(reason_lower,
               "ankle.*sprain|ankle.*spain|ankle.*sprained|high ankle.*sprain|ankle.*high ankle|syndesmosis") ~ "ankle_sprain",
    
    str_detect(reason_lower,
               "ankle.*tendinopathy|ankle.*tendinitis|ankle.*peroneal|ankle.*stress|ankle.*surgery|ankle.*soreness|ankle.*contusion|ankle.*effusion|ankle.*inflammation|ankle.*discomfort|ankle.*bursitis|ankle.*bone bruise|ankle.*strain|ankle.*injury|peroneal.*tendinopathy") ~ "ankle_other",
    
    str_detect(reason_lower,
               "plantar|foot.*soreness|foot.*sore|foot.*sprain|foot.*strain|foot.*surgery|foot.*contusion|foot.*fracture|foot.*stress|foot.*inflammation|foot.*injury|foot.*pain|midfoot|mid.foot|mid foot|metatarsal|sesamoid|navicular|lisfranc|\\btoe\\b|great toe|big toe|turf toe|heel.*soreness|heel.*pain|heel.*contusion|heel.*bursitis|heel.*strain|heel.*inflammation|retrocalcaneal|enthesopathy|tenosynovitis|neuroma") ~ "foot",
    
    str_detect(reason_lower,
               "\\bknee\\b|\\bknees\\b|chondromalacia|synovitis|bursitis.*knee|knee.*effusion|knee.*swelling|knee.*contusion|knee.*bone bruise|knee.*soreness|knee.*sore|knee.*tendin|knee.*tendon|knee.*inflammation|knee.*strain|knee.*sprain|knee.*surgery|knee.*injury|knee.*management|knee.*recovery|knee.*hyperextension|knee.*arthroscop|knee.*procedure|knee.*stress|patellofemoral|patellar.*tendin|it band") ~ "knee_soft",
    
    str_detect(reason_lower,
               "\\bback\\b|low back|lower back|lumbar|thoracic|disc.*herniation|disc.*bulge|disc.*protrusion|disc.*surgery|disc.*irritation|disc.*annular|annular fissure|spine|spinal|radiculopathy|sciatica|sciatic|facet|lumbosacral|cervicothoracic") ~ "back_spine",
    
    str_detect(reason_lower,
               "shoulder|rotator cuff|acromioclavicular|ac joint") ~ "shoulder",
    
    str_detect(reason_lower,
               "\\belbow\\b|\\bwrist\\b|forearm|ulnar.*collateral|ecu tendon|flexor carpi|olecranon|bicep|tricep|radius|ulna|styloid") ~ "elbow_wrist",
    
    str_detect(reason_lower,
               "\\bhand\\b|\\bfinger\\b|\\bthumb\\b|metacarpal|phalanx|mallet finger|mcp joint|dip joint|pip joint|mp joint|scaphoid|distal radius|carpal") ~ "hand_finger",
    
    str_detect(reason_lower,
               "\\bhip\\b|hip.*impingement|hip.*contusion|hip.*bursitis|hip.*soreness|hip.*strain|hip.*pointer|hip.*sprain|hip.*synovitis|pelvis|pelvic") ~ "hip",
    
    str_detect(reason_lower,
               "\\bneck\\b|cervical|stinger|whiplash") ~ "neck_cervical",
    
    str_detect(reason_lower,
               "\\brib\\b|ribs|ribcage|costochondral|costal cartilage|sternum|chest.*contusion") ~ "rib",
    
    str_detect(reason_lower,
               "concussion|post.traumatic head|post-traumatic|head.*contusion|head.*laceration|head.*injury") ~ "concussion",
    
    str_detect(reason_lower,
               "\\beye\\b|corneal|orbital|nasal|\\bnose\\b|facial|face.*fracture|face.*contusion|face.*mask|facemask|jaw|mouth|lip|laceration|\\bcut\\b|abrasion|iritis|retinal") ~ "eye_face",
    
    str_detect(reason_lower,
               "personal reasons|family reasons|personal") ~ "personal",
    
    TRUE ~ "unspecified"
  )
}

# is_injury_absence_flag(): TRUE when a category is a genuine injury. I use this to
# throw out the "absences" that aren't injuries at all — rest, illness, G-League
# assignments, roster moves — so they never get counted as injury events.
is_injury_absence_flag <- function(injury_category) {
  !injury_category %in% c(
    "load_management", "illness", "g_league", "roster_move",
    "personal", "reconditioning", "not_with_team", "unspecified"
  )
}

# is_structural_flag(): TRUE for the high-severity structural injuries I want to be
# able to flag separately (the ACLs, Achilles, fractures and the like).
is_structural_flag <- function(injury_category) {
  injury_category %in% c(
    "acl_pcl", "achilles", "meniscus",
    "fracture", "bone_stress", "dislocation", "ligament_tear"   # bone_stress = stress fractures
  )
}

# injury_body_region(): I collapse the 24 categories down to a handful of anatomical
# regions. I need this so I can tell when a later absence is a reaggravation of the
# SAME body part rather than a brand-new injury somewhere else.
injury_body_region <- function(injury_category) {
  case_when(
    injury_category %in% c("foot", "ankle_sprain", "ankle_other") ~ "foot_ankle",
    injury_category %in% c("knee_soft", "acl_pcl", "meniscus")     ~ "knee",
    injury_category %in% c("calf_achilles_complex", "achilles")    ~ "lower_leg",
    injury_category %in% c("hamstring", "quad_thigh")              ~ "thigh",
    injury_category %in% c("hip", "groin_adductor")                ~ "hip_groin",
    injury_category %in% c("back_spine")                           ~ "back",
    injury_category %in% c("shoulder", "elbow_wrist",
                           "hand_finger", "ligament_tear")         ~ "upper_limb",
    injury_category %in% c("rib", "neck_cervical", "concussion",
                           "eye_face")                             ~ "other_injury",
    TRUE                                                            ~ NA_character_
  )
}
# NOTE: this function is superseded by injury_region_ioc() below and is kept only so
# older code still runs. It used to map fracture / bone_stress / dislocation to a
# "structural" bucket, which is a category error: those are pathology types, not body
# regions, so a hand fracture and a foot stress fracture both landed in the same
# "region" and were then treated as recurrences of one another. Anything that needs a
# body region should call injury_region_ioc() on the raw reason string instead.

# injury_mechanism(): this is the split my whole negative-control argument rests on.
# I group each category by its likely CAUSE into THREE tiers, ordered by how much I'd
# expect accumulated LOAD to drive it:
#
#   "soft_tissue"    Muscle/tendon OVERUSE (strains, tendinopathy). If my workload
#                    hypothesis is right, load should drive these most directly.
#   "traumatic_load" Structural injuries that still have a plausible load pathway:
#                    fatigue degrades landing/cutting mechanics (ACL, ankle sprain,
#                    meniscus) and repetitive bone loading causes stress fractures.
#   "random_impact"  Contact/collision injuries that are basically INDEPENDENT of how
#                    much a player has been loaded — a jammed finger, a poke in the
#                    eye, a broken nose, a concussion. This is my CLEAN NEGATIVE
#                    CONTROL: if workload still predicts these, that tells me I'm
#                    picking up exposure/selection, not real causation.
#   "other"          Non-injury (illness, load management) / unspecified.
injury_mechanism <- function(injury_category) {          # takes one of my 24 category labels
  case_when(                                             # first rule that matches wins
    injury_category %in% c("hamstring", "calf_achilles_complex", "quad_thigh",
                           "groin_adductor", "knee_soft", "achilles",
                           "back_spine", "foot", "hip", "ankle_other")
                                                  ~ "soft_tissue",     # overuse muscle/tendon
    injury_category %in% c("acl_pcl", "meniscus", "ligament_tear",
                           "ankle_sprain", "bone_stress")
                                                  ~ "traumatic_load",  # structural but load-linked
    injury_category %in% c("fracture", "dislocation", "concussion", "eye_face",
                           "rib", "hand_finger", "elbow_wrist", "neck_cervical",
                           "shoulder")
                                                  ~ "random_impact",   # contact — the clean negative control
    TRUE                                          ~ "other"            # non-injury / unspec
  )
}

# classify on separate axes, following the IOC consensus statement
# (Bahr R, Clarsen B, Derman W, et al. BJSM 2020;54:372-389), which recommends
# recording body region, tissue type, mechanism and severity independently:
#
#   injury_side()            -> left / right / bilateral       (needed for SIC-2.0)
#   injury_region_ioc()      -> IOC body region (their Table 4)
#   injury_tissue_ioc()      -> IOC tissue type (muscle/tendon, ligament, bone, ...)
#   injury_mechanism_ioc()   -> repetitive / acute_noncontact / acute_impact
#   injury_severity_ioc()    -> IOC time-loss severity bins
#
# Nearly every reason reads "Injury/Illness - <SITE>; <PATHOLOGY>", so site and pathology are already separate
# fields and 93% carry a side. I parse them rather than throw that structure away.
#
# On mechanism: the IOC explicitly tells you NOT to use a simple sudden/gradual
# dichotomy, and to record whether the mechanism is acute, repetitive, or a mix. I
# keep three tiers because my negative-control argument needs them, but they are now
# defined on mechanism rather than on a hand-made list of body parts.

# Pull the SITE half of the reason (everything before the semicolon).
injury_site_raw <- function(reason) {
  # The reports write "Injury/Illness - ..." with capitals; the strip has to be
  # case-insensitive or it silently never fires.
  s <- str_trim(str_remove(reason, regex("^\\s*(injury/illness|injury|illness)\\s*-\\s*",
                                         ignore_case = TRUE)))
  str_trim(str_remove(s, ";.*$"))
}

# Pull the PATHOLOGY half (everything after the semicolon).
injury_nature_raw <- function(reason) {
  str_trim(str_remove(str_extract(reason, ";.*$"), "^;"))
}

# Side matters for recurrence: a left hamstring strain after a right one is NOT the
# same injury coming back. SIC-2.0 uses side explicitly, so I extract it.
injury_side <- function(reason) {
  s <- str_to_lower(injury_site_raw(reason))
  case_when(
    str_detect(s, "bilateral|both")            ~ "bilateral",
    str_detect(s, "\\bleft\\b|\\bl\\b")        ~ "left",
    str_detect(s, "\\bright\\b|\\br\\b")       ~ "right",
    TRUE                                       ~ "unspecified"
  )
}

# IOC body region (their Table 4). 
injury_region_ioc <- function(reason) {
  s <- str_to_lower(injury_site_raw(reason))
  case_when(
    str_detect(s, "concussion|\\bhead\\b|skull|scalp|\\beye\\b|orbital|oribital|retinal|\\bnose\\b|nasal|facial|\\bface\\b|jaw|mouth|lip|dental|tooth|ear\\b") ~ "head",
    str_detect(s, "\\bneck\\b|cervical|stinger|whiplash")                                   ~ "neck",
    str_detect(s, "shoulder|rotator cuff|acromioclavicular|\\bac joint\\b|clavicle|scapula|labrum|labral") ~ "shoulder",
    str_detect(s, "upper arm|humerus|bicep|tricep")                                          ~ "upper_arm",
    str_detect(s, "\\belbow\\b|olecranon|\\bucl\\b|ulnar collateral")                         ~ "elbow",
    str_detect(s, "forearm|radius|ulna\\b")                                                  ~ "forearm",
    str_detect(s, "\\bwrist\\b|carpal\\b|scaphoid|styloid")                                  ~ "wrist",
    str_detect(s, "\\bhand\\b|\\bfinger\\b|\\bthumb\\b|thunb|pinky|metacarpal|phalanx|knuckle|mallet|mp joint|mcp") ~ "hand",
    str_detect(s, "\\bchest\\b|\\brib\\b|ribs|sternum|costo|pectoral")                       ~ "chest",
    str_detect(s, "thoracic|midback|mid back|upper back")                                    ~ "thoracic_spine",
    str_detect(s, "lumbar|low back|lower back|\\bback\\b|sacral|sacrum|sacroiliac|si joint|coccyx|tailbone|spine|spinal|\\bdisc\\b|sciatic|facet") ~ "lumbosacral",
    str_detect(s, "abdom|oblique|hernia|core\\b")                                            ~ "abdomen",
    str_detect(s, "\\bhip\\b|groin|adductor|pelvi|iliac|glut|ischial|femoroacetabular")       ~ "hip_groin",
    str_detect(s, "thigh|hamstring|quad|femur|femoral")                                      ~ "thigh",
    str_detect(s, "\\bknee\\b|patella|patellar|meniscus|meniscal|\\bacl\\b|\\bpcl\\b|\\bmcl\\b|\\blcl\\b|cruciate") ~ "knee",
    str_detect(s, "\\bcalf\\b|achilles|achillies|soleus|lower leg|\\bshins?\\b|tibia|fibula|peroneal") ~ "lower_leg",
    str_detect(s, "\\bankle\\b|syndesmosis|malleol")                                          ~ "ankle",
    str_detect(s, "\\bfoot\\b|\\btoe\\b|heel|plantar|metatarsal|navicular|calcaneus|talus|retrocalcaneal|midfoot|mid-foot|sesamoid|lisfranc") ~ "foot",
    TRUE                                                                                      ~ "unspecified"
  )
}

# IOC tissue type. I read this off the pathology half where I can; where the pathology
# is only a management word ("surgery", "injury recovery") the tissue is genuinely not
# recorded and I say so rather than guess.
injury_tissue_ioc <- function(reason) {
  p <- str_to_lower(injury_nature_raw(reason))
  s <- str_to_lower(injury_site_raw(reason))
  both <- paste(s, p)
  case_when(
    str_detect(both, "concussion|head injury|post.traumatic head")                    ~ "nervous",
    str_detect(p, "stress fracture|stress reaction|stress injury|bone stress|bone bruise|bone contusion|periostitis|fracture|avulsion|bone spur|shin splint") ~ "bone",
    str_detect(p, "strain|tendinopathy|tendinitis|tendonitis|tendinosis|tendon|muscle|fasci|spasm|tightness|cramp|rupture") ~ "muscle_tendon",
    str_detect(p, "sprain|ligament|\\bacl\\b|\\bpcl\\b|\\bmcl\\b|\\blcl\\b|\\bucl\\b|cruciate|syndesmosis|instability|dislocat|subluxation|capsul") ~ "ligament_capsule",
    str_detect(p, "meniscus|meniscal|cartilage|chondral|chondromalacia|labrum|labral|bursitis|synovitis|effusion|impingement|arthritis|arthro") ~ "cartilage_synovium_bursa",
    str_detect(p, "contusion|contused|laceration|abrasion|\\bcut\\b|bruise")           ~ "superficial",
    str_detect(p, "nerve|neuroma|radiculopathy|sciatic|stinger|neuropathy")            ~ "nervous",
    str_detect(p, "sore|soreness|discomfort|inflammation|irritation|pain|swelling|stiffness|fatigue") ~ "non_specific",
    str_detect(p, "surgery|surgical|repair|reconstruct|recovery|management|rehab|procedure|injection|splint|brace|precaution") ~ "not_recorded",
    TRUE                                                                               ~ "not_recorded"
  )
}

# Mechanism, in the IOC's terms. The consensus says to record whether the mechanism is
# acute (one identifiable energy-transfer event), repetitive (accumulated low-energy
# loading), or mixed. That is exactly the axis my negative control needs, so my three
# tiers are now cut on mechanism instead of on body part:
#
#   repetitive       overuse pathology (tendinopathy, stress injury, chronic soreness).
#                    If workload causes injury, this is where it should show up.
#   acute_noncontact a discrete event, but one that fatigue can plausibly set up:
#                    strains, sprains, non-contact tears. The "mixed" bucket in IOC
#                    terms — an acute event on a cumulatively loaded tissue.
#   acute_impact     external energy transfer: contusions, lacerations, concussion,
#                    fractures and dislocations at non-load-bearing sites. Workload
#                    should NOT cause these, so this is my negative control.

injury_mechanism_ioc <- function(reason) {
  p      <- str_to_lower(injury_nature_raw(reason))
  region <- injury_region_ioc(reason)
  tissue <- injury_tissue_ioc(reason)
  impact_region <- region %in% c("head", "hand", "wrist", "forearm", "elbow",
                                 "chest", "neck", "shoulder", "upper_arm")
  case_when(
    str_detect(p, "tendinopathy|tendinitis|tendonitis|tendinosis|stress fracture|stress reaction|stress injury|fatigue fracture|bone stress|jones fracture|periostitis|shin splint|plantar fasc|bursitis|impingement|chondromalacia|apophysitis|tenosynovitis") ~ "repetitive",
    # Non-specific chronic symptoms (soreness, tightness, spasm) read as accumulated load
    str_detect(p, "sore|soreness|tightness|stiffness|spasm|cramp|discomfort|inflammation|irritation|fatigue") ~ "repetitive",
    # BONY INJURY (fracture / bone bruise), split by whether the site makes a collision
    str_detect(p, "fracture|bone bruise|bone contusion") &  impact_region              ~ "acute_impact",
    str_detect(p, "fracture|bone bruise|bone contusion") & !impact_region              ~ "acute_noncontact",
    # Clear external energy transfer. Soft-tissue contusions and lacerations stay here
    # wherever they are: a corked thigh is by definition a blow.
    str_detect(p, "contusion|contused|laceration|abrasion|\\bcut\\b|bruise|concussion") ~ "acute_impact",
    str_detect(p, "dislocat|subluxation")                                               ~ "acute_impact",
    # Discrete musculoskeletal events that fatigue can predispose
    str_detect(p, "strain|sprain|tear|torn|rupture|ligament|meniscus|meniscal|cartilage|labral|labrum") ~ "acute_noncontact",
    TRUE                                                                                ~ "unclassified"
  )
}

# IOC severity bins. The consensus defines severity as days unavailable and suggests
# 1-7 / 8-28 / >28 day bands. My clock is games rather than days, so I convert using
# the NBA's roughly 3.5 games per week and say so.
injury_severity_ioc <- function(games_missed) {
  days <- games_missed * (7 / 3.5)
  case_when(
    is.na(games_missed) ~ NA_character_,
    days <= 7           ~ "minimal (1-7 days)",
    days <= 28          ~ "moderate (8-28 days)",
    TRUE                ~ "severe (>28 days)"
  )
}
