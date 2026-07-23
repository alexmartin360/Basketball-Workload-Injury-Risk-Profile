# =============================================================================
# Injury Classification Rules
# =============================================================================
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
# =============================================================================

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

    # ── Bone stress (an OVERUSE bony injury) — I have to put this BEFORE my acute
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
    
    # ── Soft tissue ─────────────────────────────────────────────────────────
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
    injury_category %in% c("fracture", "bone_stress", "dislocation") ~ "structural",
    injury_category %in% c("rib", "neck_cervical", "concussion",
                           "eye_face")                             ~ "other_injury",
    TRUE                                                            ~ NA_character_
  )
}

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