/* scripts for Industrial Lands net supply */

/* Step 1. Bring MySQL data into Sockeye */
    -- Modelsrv3 MySQL database
    /*
        CREATE TABLE Sandbox_mjj.exclude_fiprcl(fiprcl_id nchar(18));

        INSERT INTO Sandbox_mjj.exclude_fiprcl(fiprcl_id)
        SELECT fiprcl_id FROM psrc_2023_parcel_baseyear.parcels_all GROUP BY fiprcl_id HAVING count(*) >1;

        DROP TABLE sandbox_mjj.prclbldg_values_2023;

        CREATE TABLE sandbox_mjj.prclbldg_values_2023 (fiprcl_id nchar(18) PRIMARY KEY NOT NULL, gross_sqft decimal(18,2), impval bigint(20), flag char(1), land_use_type_id int(11), x_coord_sp decimal(11,2), y_coord_sp decimal(11,2));

        INSERT INTO sandbox_mjj.prclbldg_values_2023 (fiprcl_id, gross_sqft, impval, flag, land_use_type_id, x_coord_sp, y_coord_sp)
        SELECT p.fiprcl_id, CAST(p.gross_sqft AS DECIMAL(18,2)), sum(b.improvement_value) AS impval, ' ' AS flag, p.land_use_type_id, round(p.x_coord_sp,2), round(p.y_coord_sp,2)
        FROM psrc_2023_parcel_baseyear.parcels_all AS p 
        JOIN psrc_2023_parcel_baseyear.buildings_all_ili AS b ON p.fiprcl_id=b.fiprcl_id
        WHERE NOT EXISTS (SELECT 1 FROM Sandbox_mjj.exclude_fiprcl AS e WHERE e.fiprcl_id=p.fiprcl_id) 
        GROUP BY p.fiprcl_id LIMIT 5;

        UPDATE sandbox_mjj.prclbldg_values_2023
        SET flag=CASE WHEN impval IS NULL OR impval=0 OR gross_sqft=0 THEN 'v' WHEN impval/gross_sqft < 0.00162 THEN 'v' WHEN impval/gross_sqft < 8.1 THEN 'r' ELSE '' END;

        INSERT INTO sandbox_mjj.prclbldg_values_2023 (fiprcl_id, gross_sqft, impval, flag, land_use_type_id, x_coord_sp, y_coord_sp)
        SELECT p.fiprcl_id, CAST(p.gross_sqft AS DECIMAL(18,2)), 0.00 AS impval, 'v' AS flag, p.land_use_type_id, round(p.x_coord_sp,2), round(p.y_coord_sp,2)
        FROM psrc_2023_parcel_baseyear.parcels_all AS p
        WHERE NOT EXISTS (SELECT 1 FROM psrc_2023_parcel_baseyear.buildings_all_ili AS b WHERE b.fiprcl_id=p.fiprcl_id)
        AND NOT EXISTS (SELECT 1 FROM Sandbox_mjj.exclude_fiprcl AS e WHERE e.fiprcl_id=p.fiprcl_id);

        SELECT * FROM sandbox_mjj.prclbldg_values_2023
        INTO OUTFILE 'by23_prcl_bldg.csv';
    */
    -- import file into MSSQL via flat file import tool.

/* Step 2. Create a net acreage layer by erasing the largest_waterbodies layer, and all other exclusions (wetlands, CAI tiers, public use) from ElmerGeo.dbo.parcels_urbansim_2018 in ArcGIS */
    --Faster than using OGC queries to intersect such detailed geometry
    --import via ogr2ogr into Sockeye

    UPDATE Sandbox.Mike.prcl23_netx
    SET Shape = Shape.MakeValid();
    GO

/* Step 3. Create primary table with necessary fields */
    -- Faster to import all parcels, spatially update, and then drop those which don't match, than to selectively insert those which do match (go figure)
    DROP TABLE IF EXISTS Sandbox.Mike.ilx_indprcl23;
    GO
    CREATE TABLE Sandbox.Mike.ilx_indprcl23(fiprcl_id nchar(20) PRIMARY KEY NOT NULL, 
                                            ind_type nvarchar(25), 
                                            county_id smallint, 
                                            mic nvarchar(40), 
                                            impval bigint, 
                                            gross_sqft decimal(18,2),  
                                            land_use_type_id smallint,
                                            flag nchar(3), 
                                            value_ratio decimal(11,2),
                                            urban nchar(3),
                                            CentroidShape geometry);
    GO 
    INSERT INTO Sandbox.Mike.ilx_indprcl23(fiprcl_id, impval, gross_sqft, land_use_type_id, flag, CentroidShape)
    SELECT p.fiprcl_id, p.impval, p.gross_sqft, p.land_use_type_id, p.flag, 
           geometry::STGeomFromText('POINT(' + CAST(p.x_coord_sp AS VARCHAR(20)) + ' ' + CAST(p.y_coord_sp AS VARCHAR(20)) + ')', 2285) AS CentroidShape
    FROM Sandbox.Mike.by23_prcl_bldg AS p;
    GO
    CREATE SPATIAL INDEX ilxs_all_ind_parcels ON Sandbox.Mike.ilx_indprcl23(CentroidShape)
    USING GEOMETRY_AUTO_GRID WITH (BOUNDING_BOX = (xmin = 1111000, ymin = -92400, xmax = 1520420, ymax = 476385));
    GO

    --Spatial join with the 2023 Industrial Lands Inventory
    UPDATE x
    SET x.ind_type=CASE WHEN i.ind_type IS NULL OR i.ind_type IN('Aviation Operations','Military Industrial','Limited Industrial') THEN '' ELSE i.ind_type END
    FROM Sandbox.Mike.ilx_indprcl23 AS x JOIN Sandbox.Mike.ili20231221 AS i ON x.CentroidShape.STIntersects(i.Shape)=1
    WHERE x.ind_type IS NULL;
    GO
    DELETE FROM Sandbox.Mike.ilx_indprcl23 WHERE ind_type='' OR ind_type IS NULL;
    GO

    UPDATE x 
    SET x.value_ratio=CASE WHEN gross_sqft > 0 THEN x.impval/x.gross_sqft ELSE 0 END 
    FROM Sandbox.Mike.ilx_indprcl23 AS x;

    --Add vacant and redevelopment flags
    UPDATE Sandbox.Mike.ilx_indprcl23
    SET flag='v' WHERE impval IS NULL OR impval < 0.01257 AND flag IS NULL;
    GO
    UPDATE Sandbox.Mike.ilx_indprcl23
    SET flag='v' WHERE impval/gross_sqft < 0.01257 AND gross_sqft<>0 AND flag IS NULL;    
    GO
    UPDATE Sandbox.Mike.ilx_indprcl23
    SET flag='r' WHERE impval/gross_sqft < 6.28 AND gross_sqft<>0 AND flag IS NULL;        
    GO

    --Attach geographic assignments
    UPDATE x 
    SET x.county_id=CAST(county_fip AS smallint)
    FROM Sandbox.Mike.ilx_indprcl23 AS x JOIN ElmerGeo.dbo.county_background_evw AS c ON x.CentroidShape.STIntersects(c.Shape)=1
    WHERE c.county_fip IN('033','035','053','061');
    GO
    UPDATE x 
    SET x.mic=m.mic
    FROM Sandbox.Mike.ilx_indprcl23 AS x JOIN ElmerGeo.dbo.micen_evw AS m ON x.CentroidShape.STIntersects(m.Shape)=1;    
    GO
    UPDATE x 
    SET x.urban=CASE WHEN r.class_desc='Metro' AND r.Juris<>'Bremerton' THEN 'Y' ELSE 'N' END
    FROM Sandbox.Mike.ilx_indprcl23 AS x JOIN ElmerGeo.dbo.psrc_revion_evw AS r ON x.CentroidShape.STIntersects(r.Shape)=1;    
    GO  

--QC
SELECT count(*) FROM Sandbox.Mike.ilx_indprcl23 AS a WHERE NOT EXISTS (SELECT 1 FROM Mike.by23_prcl_bldg AS p WHERE a.fiprcl_id=p.fiprcl_id)
 
/* Step 5. 2023 net supply queries */

    --Replicates 2015 method of market factor and ROI set-asides through a multiplier

    WITH cte AS (SELECT x.mic, x.flag, round(sum(p.Shape.STArea() * (CASE WHEN x.urban='Y' THEN .88 ELSE .85 END))/43560,2) AS acres 
                FROM Sandbox.Mike.ilx_indprcl23 AS x JOIN Sandbox.Mike.prcl23_netx AS p ON x.fiprcl_id=p.fiprcl_id 
                WHERE x.mic<>'' AND x.land_use_type_id NOT IN(2,7,8,19,22,23,29) --AND x.flag IN('v', 'r')
                GROUP BY x.flag, x.mic)
    SELECT * FROM cte PIVOT (max(acres) FOR flag IN([v], [r])) AS pv;

    WITH cte AS (SELECT x.county_id, x.flag, round(sum(p.Shape.STArea() * (CASE WHEN x.urban='Y' THEN .88 ELSE .85 END))/43560,2) AS acres 
                FROM Sandbox.Mike.ilx_indprcl23 AS x JOIN Sandbox.Mike.prcl23_netx AS p ON x.fiprcl_id=p.fiprcl_id 
                WHERE ((x.mic<>'' AND x.land_use_type_id=6) OR x.land_use_type_id NOT IN(2,6,7,8,19,22,23,29)) AND x.flag IN('v', 'r')
                GROUP BY x.flag, x.county_id) 
    SELECT * FROM cte PIVOT (max(acres) FOR flag IN([v], [r])) AS pv;

    WITH cte AS (SELECT x.ind_type, x.flag, round(sum(p.Shape.STArea() * (CASE WHEN x.urban='Y' THEN .88 ELSE .85 END))/43560,2) AS acres 
                FROM Sandbox.Mike.ilx_indprcl23 AS x JOIN Sandbox.Mike.prcl23_netx AS p ON x.fiprcl_id=p.fiprcl_id
                WHERE ((x.mic<>'' AND x.land_use_type_id=6) OR x.land_use_type_id NOT IN(2,6,7,8,19,22,23,29)) AND x.flag IN('v', 'r')
                GROUP BY x.flag, x.ind_type)
    SELECT * FROM cte PIVOT (max(acres) FOR flag IN([v], [r])) AS pv;

    /* -- For comparison estimates from 2015 CAI files 
   --import combined net supply geometry with exclusions erased into Sockeye via ogr2ogr as ili_2013_net 

    UPDATE Sandbox.Mike.ili_2013_net SET Shape=Shape.MakeValid(); */

    WITH cte AS(SELECT i.net_flag, c.county_nm, 
                round(sum(c.Shape.STDifference(e.Shape).STIntersection(i.Shape).STArea() * CASE WHEN rg.class_desc='Metro' THEN .88 ELSE .85 END/43560),2) AS acres
                FROM ElmerGeo.dbo.county_background_evw AS c 
                LEFT JOIN Sandbox.Mike.ili_2013_net AS i ON 1=1 
                JOIN Sandbox.Mike.ili_2013_net_exclude AS e ON 1=1
                LEFT JOIN ElmerGeo.dbo.regional_geographies_evw AS rg ON 1=1
                WHERE c.county_fip IN('033','035','053','061')
                GROUP BY i.net_flag, c.county_nm)
    SELECT * FROM cte PIVOT (max(acres) FOR net_flag IN([vacant], [redevelopable])) AS p;

    WITH cte AS(SELECT i.net_flag, m.mic, 
                round(sum(m.Shape.STIntersection(i.Shape).STArea() * CASE WHEN rg.class_desc='Metro' THEN .88 ELSE .85 END/43560),2) AS acres
                FROM ElmerGeo.dbo.micen_evw AS m 
                LEFT JOIN Sandbox.Mike.ili_2013_net AS i ON 1=1 
                JOIN Sandbox.Mike.ili_2013_net_exclude AS e ON 1=1
                LEFT JOIN ElmerGeo.dbo.regional_geographies_evw AS rg ON 1=1
                GROUP BY i.net_flag, m.mic)
    SELECT * FROM cte PIVOT (max(acres) FOR net_flag IN([vacant], [redevelopable])) AS p;

    --query 2013 CAI estimates

    WITH cte AS(SELECT i.net_flag, c.county_nm,     
                round(sum(c.Shape.STIntersection(i.Shape).STArea()* CASE WHEN rg.class_desc='Metro' THEN .88 ELSE .85 END)/43560,2) AS acres
                FROM ElmerGeo.dbo.county_background_evw AS c 
                JOIN Sandbox.Mike.ili_2013_net AS i ON 1=1
                LEFT JOIN ElmerGeo.dbo.regional_geographies_evw AS rg ON 1=1
                WHERE c.county_fip IN('033','035','053','061')
                GROUP BY i.net_flag, c.county_nm)
    SELECT * FROM cte PIVOT (max(acres) FOR net_flag IN([vacant], [redevelopable])) AS p;

    WITH cte AS(SELECT i.net_flag, rg.class_desc,     
                round(sum(rg.Shape.STIntersection(i.Shape()).STArea())/43560 * .9,2) AS acres
                FROM Sandbox.Mike.ili_2013_net AS i
                JOIN ElmerGeo.dbo.regional_geographies_evw AS rg ON 1=1
                GROUP BY i.net_flag, rg.class_desc)
    SELECT * FROM cte PIVOT (max(acres) FOR net_flag IN([vacant], [redevelopable])) AS p;

    WITH cte AS(SELECT i.net_flag, m.mic, 
                round(sum(m.Shape.STIntersection(i.Shape).STDifference(w.Shape).STArea())/43560 * .88,2) AS acres
                FROM ElmerGeo.dbo.micen_evw AS m LEFT JOIN Sandbox.Mike.ili_2013_net AS i ON 1=1 LEFT JOIN ElmerGeo.dbo.largest_waterbodies_evw as w ON 1=1
                GROUP BY i.net_flag, m.mic)
    SELECT * FROM cte PIVOT (max(acres) FOR net_flag IN([vacant], [redevelopable])) AS p;
