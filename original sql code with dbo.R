"SELECT distinct
	facility.name
    ,dfwf.Latitude_Meas as latitude
	,dfwf.Longitude_Meas as longitude
	,event.event_type_id,
    event_type_lut.description
	,SPECIES_LUT.Description as species_description
	,run_lut.Description as run_description
	, STOCK_LUT.Description as stock_description
	,origin_lut.Description Origin_desc
	,MARK_TYPE_CLIP_LUT.Description
  FROM hatcheries.dbo.EVENT_ADULT
     JOIN hatcheries.dbo.event ON event.event_id = event_adult.event_id
     JOIN hatcheries.dbo.brood ON event.brood_id = brood.brood_id
     JOIN hatcheries.dbo.facility ON facility.facility_id = event.facility_id
     JOIN hatcheries.dbo.event_type_lut ON event_type_lut.event_type_id = event.event_type_id
	  join STOCK_LUT on STOCK_LUT.STOCK_Id = brood.stock_id
  join RUN_LUT on run_lut.RUN_Id = brood.run_id
  join SPECIES_LUT on SPECIES_LUT.SPECIES_Id = brood.species_id
  join ORIGIN_LUT on origin_lut.ORIGIN_Id = BROOD.ORIGIN_Id
  join MARK_TYPE_CLIP_LUT on MARK_TYPE_CLIP_LUT.MARK_TYPE_CLIP_Id = EVENT_ADULT.MARK_TYPE_CLIP1_ID
  join fishProgram.WALOCs.facility waf on waf.facility_id = EVENT.facility_id
  join fishProgram.WALOCs.vw_WALOCS_DFW_Facility dfwf on dfwf.WALOC_Id = waf.WALOC_Id
  where name not like 'OLY%'
  and brood_year >= 2010
  and dfwf.Hatchery_Region_Group like '%Columbia%'
  and dfwf.Region = 5
	 
	 and (event_adult.male_num > 0 OR event_adult.female_num > 0 OR event_adult.adult_num > 0 OR event_adult.jack_num > 0 OR event_adult.nvf_num > 0) AND event.deleted_flag = 'false' AND (event.event_type_id in (3))"