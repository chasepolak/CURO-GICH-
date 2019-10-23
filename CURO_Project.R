library(tidyverse)
library(readxl)
###Read in Data
commerce_issues <- read_excel("commerce issues.xlsx")
commerce_owner <- read_excel("commerce owner.xlsx")
commerce_proprty <- read_excel("commerce proprty.xlsx")
Gainesville_issues_and_info <- read_excel("Gainesville issues and info.xlsx")
hartwell_issues_and_owner_correct_version <- read_excel("hartwell issues and owner correct version.xlsx")
millen_info_with_parcel_number <- read_excel("millen info with parcel number.xlsx")
millen_issues <- read_excel("millen issues.xlsx")
millen_owner <- read_excel("millen_owner.xlsx")
monroe_info <- read_excel("monroe info.xlsx")
monroe_issues <- read_excel("monroe issues.xlsx")
warrenton_information <- read_excel("warrenton information.xlsx")
warrenton_issues <- read_excel("warrenton issues.xlsx")
Cochran_Issues <- read_excel("Cochran Issues.xlsx")
Cochran_info <- read_excel("Cochran info.xlsx")
### Combine Commerce Data
commerce_owner_property<-commerce_owner%>%
  left_join(commerce_proprty)
commerce_issues_info<-commerce_issues%>%
  left_join(commerce_owner_property)
commerce_info_issues<-commerce_issues_info%>%
  mutate("primary_city"="Commerce",
         realkey=as.character(realkey))
### Combine Gainesville data
Gainesville_issues_and_info <- read_excel("Gainesville issues and info.xlsx")
gainesville_info_issues<-Gainesville_issues_and_info%>%
  mutate("primary_city"="gainesville")
###Combine hartwell
hartwell_info_issues<-hartwell_issues_and_owner_correct_version
hartwell_info_issues<-hartwell_info_issues%>%
  mutate("primary_city"="hartwell")
###Combine millen
millen_info<-millen_info_with_parcel_number%>%
  left_join(millen_owner)
millen_info_issues<-millen_issues%>%
  left_join(millen_info)
millen_info_issues<-millen_info_issues%>%
  mutate("primary_city"="millen")
###Combine monroe
monroe_info_issues<-monroe_issues%>%
  left_join(monroe_info)
monroe_info_issues<-monroe_info_issues%>%
  mutate("primary_city"="monroe")
###Combine cochran
cochran_info_issues<-Cochran_Issues%>%
  left_join(Cochran_info)
cochran_info_issues<-cochran_info_issues%>%
  mutate("primary_city"="cochran")
###Combine Warrenton, need help on fizxing excel
warrenton_info_issues<-warrenton_issues%>%
  left_join(warrenton_information)
<<<<<<< HEAD

###eliminate all data from property and owner, except own address,name, city, state, homestead variable, eliminate old curo projects
#change variable names to match. seperate one for each survey, finish the crosswalk file

 ### merge all files together
=======
###merge all data
full_data_combine<-millen_info_issues%>%
  full_join(gainesville_info_issues)%>%
  full_join(hartwell_info_issues)%>%
  full_join(cochran_info_issues)%>%
  full_join(commerce_info_issues,by=c("Parcel_No", "address", "condition", "long", "lat", "ownaddress", "owncity", "ownstate", "primary_city", "homeexempt", "reviewed_by", "date", "search_type", "search_add", "parcel_num_add", "search_parcel", "parcel_num_parcel", "address_parcel", "address_ver", "address_rev","property_type", "multifamily_units", "other_elaborate", "gen_prop_info.occupied", "gen_prop_info.unoccupied", "gen_prop_info.for_sale", "gen_prop_info.unknown", "num_of_stories", "foundation", "found_cracked_num", "found_partreplace_num", "found_compreplace_num", "found_total", "exterior.ext_good", "exterior.ext_repaint", "exterior.ext_cracked", "exterior.ext_needs_replace", "exterior.ext_chimney", "exterior.ext_nosiding", "exterior.ext_notvis", "ext_repaint_num", "ext_cracked_num", "ext_needs_replace_num", "ext_chimney_num", "ext_nosiding_num", "ext_total", "windows_doors.window_good", "windows_doors.window_repaint", "windows_doors.window_crackedpanes", "windows_doors.window_minreplace", "windows_doors.window_majreplace", "window_repaint_num", "window_crackedpanes_num", "window_minreplace_num", "window_majreplace_num", "window_total", "stairs_rails", "stairs_repaint_num", "stairs_cracked_num", "stairs_majorrepair_num", "stairs_total", "roofing.roof_good", "roofing.roof_gutters", "roofing.roof_shingles", "roofing.roof_reroof_part", "roofing.roof_reroof_tot", "roofing.roof_newstructure", "roof_gutters_num", "roof_shingles_num", "roof_reroof_part_num", "roof_reroof_tot_num", "roof_newstructure_num", "roof_total", "points_total", "lot_assess.lot_satis", "lot_assess.lot_weeds", "lot_assess.lot_missingcover", "lot_assess.lot_trees", "lot_assess.lot_inop_vehicle", "lot_assess.lot_junk", "lot_assess.lot_porchstorage", "lot_assess.lot_graffiti", "comments", "photo1", "photo2", "photo3", "photo4", "start", "end", "meta.instanceID", "X_id", "X_uuid", "X_submission_time", "X_tags", "X_notes", "X_version", "X_duration", "X_submitted_by", "X_total_media", "X_media_count", "X_media_all_received", "X_xform_id", "prop_type_name", "genprop", "found_good", "found_partreplace", "found_compreplace", "found_cracked", "found_text", "exterior", "windows", "stairs_good", "stairs_cracked", "stairs_majorrepair", "stairs_repaint", "stairs_text", "roof", "lot", "geometry", "ownkey", "cond_factor", "textbox", "FIRSTNAME", "MIDDLE", "ownzip"))%>%
  full_join(monroe_info_issues)

full_data_bind<-bind_rows(millen_info_issues, gainesville_info_issues,
                          hartwell_info_issues,cochran_info_issues,
                          commerce_info_issues, monroe_info_issues)
 
>>>>>>> 0eb6496029281c5bba991d3faad21705ee5d5c3e
full_data_combine<-merge(gainesville_info_issues,millen_info_issues,hartwell_info_issues,cochran_info_issues,millen_info_issues,by=c("primary_city"))
