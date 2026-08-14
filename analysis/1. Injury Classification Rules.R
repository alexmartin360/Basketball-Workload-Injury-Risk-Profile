
# 1. Injury Classification Rules
# Turns the free-text injury reasons on the NBA reports into tidy categories.
# Sourced by files 2 and 4 so the same rules apply everywhere.
#
# classify_injury()      -> 24 injury categories, used to flag genuine injury absences
# injury_*_ioc()         -> the IOC axes (region / tissue / mechanism / severity),
#                           recorded independently as Bahr et al. (BJSM 2020) recommend
# injury_side()          -> left / right / bilateral, needed to match recurrences

# The NBA prefixes every reason with "Injury/Illness -", so it has to be stripped
# before matching or the word "illness" fires on genuine injuries.
strip_prefix <- function(x) {
  str_trim(str_remove(x, regex("^\\s*(injury/illness|injury|illness)\\s*-\\s*",
                               ignore_case = TRUE)))
}

# Site is everything before the semicolon, pathology everything after:
# the reports read "Injury/Illness - <SITE>; <PATHOLOGY>".
injury_site_raw   <- function(reason) str_trim(str_remove(strip_prefix(reason), ";.*$"))
injury_nature_raw <- function(reason) str_trim(str_remove(str_extract(reason, ";.*$"), "^;"))

classify_injury <- function(reason_lower) {
  reason_lower <- strip_prefix(reason_lower)

  case_when(
    # ── Non-injury absences ──
    str_detect(reason_lower, "g league|two-way|on assignment|two way") ~ "g_league",
    str_detect(reason_lower, "\\brest\\b|load management|maintenance") ~ "load_management",
    str_detect(reason_lower, "trade pending|league suspension|team suspension|ineligible|coach.s decision|personal reasons|family reasons") ~ "roster_move",
    str_detect(reason_lower, "not with team") ~ "not_with_team",
    str_detect(reason_lower, "return to competition reconditioning|reconditioning") ~ "reconditioning",
    str_detect(reason_lower, "health and safety protocols") ~ "illness",
    str_detect(reason_lower, "illness|non-covid|non covid|flu|influenza|flu-like|respiratory|gastrointestinal|gastroenteritis|stomach|virus|viral|asthma|migraine|headache|sinus|congestion|pharyngitis|strep|covid|dental|tooth|wisdom tooth|appendectomy|pneumothorax|deep vein thrombosis|dvt") ~ "illness",

    # ── Structural, most specific first ──
    str_detect(reason_lower, "\\bacl\\b|anterior cruciate|\\bpcl\\b|posterior cruciate") ~ "acl_pcl",
    str_detect(reason_lower, "achilles.*(tear|rupture|torn|repair|surgery|reconstruct|rehab)") ~ "achilles",
    str_detect(reason_lower, "meniscu|meniscal|meniscectomy|meniscotomy") ~ "meniscus",

    # Bone stress must precede the acute-fracture rule: stress fractures and fractures
    # at load-bearing sites come from accumulated load, not from a collision.
    str_detect(reason_lower, "stress fracture|stress reaction|stress injury|bone stress|bone bruise|bone contusion|bone spur") ~ "bone_stress",
    str_detect(reason_lower, "(navicular|metatarsal|tibia|tibial|fibula|lower leg|\\bshin\\b|femoral|femur|\\bfoot\\b|\\bankle\\b|calcaneus|talus|lumbar|lower back|low back|sacral|sacrum|pelvi|iliac|\\bhip\\b).*fracture") ~ "bone_stress",
    str_detect(reason_lower, "fracture|hairline fracture|non.displaced fracture|scaphoid|metacarpal.*fracture|orbital|nasal|facial|jaw|sternum|scapula|clavicle") ~ "fracture",
    str_detect(reason_lower, "dislocation|dislocated|subluxation|shoulder.*dislocated") ~ "dislocation",
    str_detect(reason_lower, "\\bucl\\b|mcl.*tear|mcl.*sprain|labral.*tear|labrum.*tear|labral.*repair|rotator cuff.*tear|rotator cuff.*torn|lisfranc.*tear|wrist.*ligament.*tear|thumb.*ligament.*tear|central slip.*tear|extensor.*tear|extensor.*rupture|patellar.*tendon.*rupture|patellar.*tendon.*torn|adductor.*torn|adductor.*tear|oblique.*torn|oblique.*tear|thumb.*ucl|scapholunate|trochlea.*tear|trochlea.*cartilage") ~ "ligament_tear",

    # ── Soft tissue ──
    str_detect(reason_lower, "achilles|achillies") ~ "calf_achilles_complex",
    str_detect(reason_lower, "hamstring") ~ "hamstring",
    str_detect(reason_lower, "\\bcalf\\b|soleus|lower leg.*strain|lower leg.*muscle|popliteus|peroneal.*strain|peroneal.*soreness|tibialis.*strain|tibialis.*tendon|posterior tibialis|periostitis|shin.*stress|shin.*fracture|shin.*soreness|shin.*contusion|fibula.*soreness") ~ "calf_achilles_complex",
    str_detect(reason_lower, "\\bquad\\b|quadricep|quadriceps|thigh.*strain|thigh.*contusion|thigh.*soreness|patellar.*tendin|patellofemoral|patellar.*contusion|patella.*tendin|patella.*femoral|quad.*tendon|quadricep.*tendon") ~ "quad_thigh",
    str_detect(reason_lower, "groin|adductor|hip flexor|hip.*flexor|sports hernia|hernia|oblique|abdominal.*strain|abdominal.*tendinopathy|core.*muscle|pelvic.*core|lower abdominal|rectus abdomin|abdominal|sacroiliac|si joint|sacral|sacrum|tailbone|glut|gluteal|gluteus|ischial") ~ "groin_adductor",
    str_detect(reason_lower, "ankle.*sprain|ankle.*spain|ankle.*sprained|high ankle.*sprain|ankle.*high ankle|syndesmosis") ~ "ankle_sprain",
    str_detect(reason_lower, "ankle.*tendinopathy|ankle.*tendinitis|ankle.*peroneal|ankle.*stress|ankle.*surgery|ankle.*soreness|ankle.*contusion|ankle.*effusion|ankle.*inflammation|ankle.*discomfort|ankle.*bursitis|ankle.*bone bruise|ankle.*strain|ankle.*injury|peroneal.*tendinopathy") ~ "ankle_other",
    str_detect(reason_lower, "plantar|foot.*soreness|foot.*sore|foot.*sprain|foot.*strain|foot.*surgery|foot.*contusion|foot.*fracture|foot.*stress|foot.*inflammation|foot.*injury|foot.*pain|midfoot|mid.foot|mid foot|metatarsal|sesamoid|navicular|lisfranc|\\btoe\\b|great toe|big toe|turf toe|heel.*soreness|heel.*pain|heel.*contusion|heel.*bursitis|heel.*strain|heel.*inflammation|retrocalcaneal|enthesopathy|tenosynovitis|neuroma") ~ "foot",
    str_detect(reason_lower, "\\bknee\\b|\\bknees\\b|chondromalacia|synovitis|bursitis.*knee|knee.*effusion|knee.*swelling|knee.*contusion|knee.*bone bruise|knee.*soreness|knee.*sore|knee.*tendin|knee.*tendon|knee.*inflammation|knee.*strain|knee.*sprain|knee.*surgery|knee.*injury|knee.*management|knee.*recovery|knee.*hyperextension|knee.*arthroscop|knee.*procedure|knee.*stress|patellofemoral|patellar.*tendin|it band") ~ "knee_soft",
    str_detect(reason_lower, "\\bback\\b|low back|lower back|lumbar|thoracic|disc.*herniation|disc.*bulge|disc.*protrusion|disc.*surgery|disc.*irritation|disc.*annular|annular fissure|spine|spinal|radiculopathy|sciatica|sciatic|facet|lumbosacral|cervicothoracic") ~ "back_spine",
    str_detect(reason_lower, "shoulder|rotator cuff|acromioclavicular|ac joint") ~ "shoulder",
    str_detect(reason_lower, "\\belbow\\b|\\bwrist\\b|forearm|ulnar.*collateral|ecu tendon|flexor carpi|olecranon|bicep|tricep|radius|ulna|styloid") ~ "elbow_wrist",
    str_detect(reason_lower, "\\bhand\\b|\\bfinger\\b|\\bthumb\\b|metacarpal|phalanx|mallet finger|mcp joint|dip joint|pip joint|mp joint|scaphoid|distal radius|carpal") ~ "hand_finger",
    str_detect(reason_lower, "\\bhip\\b|hip.*impingement|hip.*contusion|hip.*bursitis|hip.*soreness|hip.*strain|hip.*pointer|hip.*sprain|hip.*synovitis|pelvis|pelvic") ~ "hip",
    str_detect(reason_lower, "\\bneck\\b|cervical|stinger|whiplash") ~ "neck_cervical",
    str_detect(reason_lower, "\\brib\\b|ribs|ribcage|costochondral|costal cartilage|sternum|chest.*contusion") ~ "rib",
    str_detect(reason_lower, "concussion|post.traumatic head|post-traumatic|head.*contusion|head.*laceration|head.*injury") ~ "concussion",
    str_detect(reason_lower, "\\beye\\b|corneal|orbital|nasal|\\bnose\\b|facial|face.*fracture|face.*contusion|face.*mask|facemask|jaw|mouth|lip|laceration|\\bcut\\b|abrasion|iritis|retinal") ~ "eye_face",
    str_detect(reason_lower, "personal reasons|family reasons|personal") ~ "personal",
    TRUE ~ "unspecified"
  )
}

# A genuine injury, so rest / illness / G-League / roster moves never count as events.
is_injury_absence_flag <- function(injury_category) {
  !injury_category %in% c("load_management", "illness", "g_league", "roster_move",
                          "personal", "reconditioning", "not_with_team", "unspecified")
}

is_structural_flag <- function(injury_category) {
  injury_category %in% c("acl_pcl", "achilles", "meniscus",
                         "fracture", "bone_stress", "dislocation", "ligament_tear")
}

# Side matters for recurrence: a left hamstring strain after a right one is a
# different injury, and SIC-2.0 uses side explicitly.
injury_side <- function(reason) {
  s <- str_to_lower(injury_site_raw(reason))
  case_when(
    str_detect(s, "bilateral|both")      ~ "bilateral",
    str_detect(s, "\\bleft\\b|\\bl\\b")  ~ "left",
    str_detect(s, "\\bright\\b|\\br\\b") ~ "right",
    TRUE                                 ~ "unspecified"
  )
}

# IOC body region (their Table 4), read off the site half of the reason.
injury_region_ioc <- function(reason) {
  s <- str_to_lower(injury_site_raw(reason))
  case_when(
    str_detect(s, "concussion|\\bhead\\b|skull|scalp|\\beye\\b|orbital|oribital|retinal|\\bnose\\b|nasal|facial|\\bface\\b|jaw|mouth|lip|dental|tooth|ear\\b") ~ "head",
    str_detect(s, "\\bneck\\b|cervical|stinger|whiplash")                              ~ "neck",
    str_detect(s, "shoulder|rotator cuff|acromioclavicular|\\bac joint\\b|clavicle|scapula|labrum|labral") ~ "shoulder",
    str_detect(s, "upper arm|humerus|bicep|tricep")                                    ~ "upper_arm",
    str_detect(s, "\\belbow\\b|olecranon|\\bucl\\b|ulnar collateral")                   ~ "elbow",
    str_detect(s, "forearm|radius|ulna\\b")                                            ~ "forearm",
    str_detect(s, "\\bwrist\\b|carpal\\b|scaphoid|styloid")                            ~ "wrist",
    str_detect(s, "\\bhand\\b|\\bfinger\\b|\\bthumb\\b|thunb|pinky|metacarpal|phalanx|knuckle|mallet|mp joint|mcp") ~ "hand",
    str_detect(s, "\\bchest\\b|\\brib\\b|ribs|sternum|costo|pectoral")                  ~ "chest",
    str_detect(s, "thoracic|midback|mid back|upper back")                              ~ "thoracic_spine",
    str_detect(s, "lumbar|low back|lower back|\\bback\\b|sacral|sacrum|sacroiliac|si joint|coccyx|tailbone|spine|spinal|\\bdisc\\b|sciatic|facet") ~ "lumbosacral",
    str_detect(s, "abdom|oblique|hernia|core\\b")                                      ~ "abdomen",
    str_detect(s, "\\bhip\\b|groin|adductor|pelvi|iliac|glut|ischial|femoroacetabular") ~ "hip_groin",
    str_detect(s, "thigh|hamstring|quad|femur|femoral")                                ~ "thigh",
    str_detect(s, "\\bknee\\b|patella|patellar|meniscus|meniscal|\\bacl\\b|\\bpcl\\b|\\bmcl\\b|\\blcl\\b|cruciate") ~ "knee",
    str_detect(s, "\\bcalf\\b|achilles|achillies|soleus|lower leg|\\bshins?\\b|tibia|fibula|peroneal") ~ "lower_leg",
    str_detect(s, "\\bankle\\b|syndesmosis|malleol")                                   ~ "ankle",
    str_detect(s, "\\bfoot\\b|\\btoe\\b|heel|plantar|metatarsal|navicular|calcaneus|talus|retrocalcaneal|midfoot|mid-foot|sesamoid|lisfranc") ~ "foot",
    TRUE                                                                               ~ "unspecified"
  )
}

# IOC tissue type, read off the pathology half. Where the pathology is only a
# management word ("surgery", "recovery") the tissue is genuinely not recorded.
injury_tissue_ioc <- function(reason) {
  p    <- str_to_lower(injury_nature_raw(reason))
  both <- paste(str_to_lower(injury_site_raw(reason)), p)
  case_when(
    str_detect(both, "concussion|head injury|post.traumatic head")                     ~ "nervous",
    str_detect(p, "stress fracture|stress reaction|stress injury|bone stress|bone bruise|bone contusion|periostitis|fracture|avulsion|bone spur|shin splint") ~ "bone",
    str_detect(p, "strain|tendinopathy|tendinitis|tendonitis|tendinosis|tendon|muscle|fasci|spasm|tightness|cramp|rupture") ~ "muscle_tendon",
    str_detect(p, "sprain|ligament|\\bacl\\b|\\bpcl\\b|\\bmcl\\b|\\blcl\\b|\\bucl\\b|cruciate|syndesmosis|instability|dislocat|subluxation|capsul") ~ "ligament_capsule",
    str_detect(p, "meniscus|meniscal|cartilage|chondral|chondromalacia|labrum|labral|bursitis|synovitis|effusion|impingement|arthritis|arthro") ~ "cartilage_synovium_bursa",
    str_detect(p, "contusion|contused|laceration|abrasion|\\bcut\\b|bruise")           ~ "superficial",
    str_detect(p, "nerve|neuroma|radiculopathy|sciatic|stinger|neuropathy")            ~ "nervous",
    str_detect(p, "sore|soreness|discomfort|inflammation|irritation|pain|swelling|stiffness|fatigue") ~ "non_specific",
    TRUE                                                                               ~ "not_recorded"
  )
}

# Mechanism on the IOC axis, which is what the negative control needs:
# repetitive (accumulated load) / acute non-contact (fatigue can set it up) /
# acute impact (external energy transfer, so load cannot cause it).
injury_mechanism_ioc <- function(reason) {
  p <- str_to_lower(injury_nature_raw(reason))
  impact_region <- injury_region_ioc(reason) %in%
    c("head", "hand", "wrist", "forearm", "elbow", "chest", "neck", "shoulder", "upper_arm")
  case_when(
    str_detect(p, "tendinopathy|tendinitis|tendonitis|tendinosis|stress fracture|stress reaction|stress injury|fatigue fracture|bone stress|jones fracture|periostitis|shin splint|plantar fasc|bursitis|impingement|chondromalacia|apophysitis|tenosynovitis") ~ "repetitive",
    str_detect(p, "sore|soreness|tightness|stiffness|spasm|cramp|discomfort|inflammation|irritation|fatigue") ~ "repetitive",
    # A fracture splits on whether the site makes a collision the likely cause.
    str_detect(p, "fracture|bone bruise|bone contusion") &  impact_region ~ "acute_impact",
    str_detect(p, "fracture|bone bruise|bone contusion") & !impact_region ~ "acute_noncontact",
    str_detect(p, "contusion|contused|laceration|abrasion|\\bcut\\b|bruise|concussion") ~ "acute_impact",
    str_detect(p, "dislocat|subluxation")                                               ~ "acute_impact",
    str_detect(p, "strain|sprain|tear|torn|rupture|ligament|meniscus|meniscal|cartilage|labral|labrum") ~ "acute_noncontact",
    TRUE                                                                                ~ "unclassified"
  )
}

# IOC time-loss severity bands. My clock is games, so I convert at the NBA's
# roughly 3.5 games per week.
injury_severity_ioc <- function(games_missed) {
  days <- games_missed * (7 / 3.5)
  case_when(
    is.na(games_missed) ~ NA_character_,
    days <= 7           ~ "minimal (1-7 days)",
    days <= 28          ~ "moderate (8-28 days)",
    TRUE                ~ "severe (>28 days)"
  )
}
