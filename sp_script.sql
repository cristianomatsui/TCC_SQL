      CREATE OR REPLACE FUNCTION insertShapeDistance() 
	  RETURNS VOID AS $$
      BEGIN
	  
        INSERT INTO shape_f_base (id_focus, id_feature, dist_l1, dist_l2, dist_linf) (
			select S1.id_ft, S2.id_ft, cube(S1.feature) <#> cube(S2.feature), cube(S1.feature) <->  cube(S2.feature), cube(S1.feature) <=>  cube(S2.feature)
			from shape S1, shape S2 
			where S1.is_focus = TRUE AND S1 <> S2																			
		) ;
		END;
      
         $$ LANGUAGE plpgsql;  
		 
		 
      CREATE OR REPLACE FUNCTION insertTextureDistance() 
	  RETURNS VOID AS $$
      BEGIN
	  
        INSERT INTO texture_f_base (id_focus, id_feature, dist_l1, dist_l2, dist_linf) (
			select T1.id_ft, T2.id_ft, cube(T1.feature) <#> cube(T2.feature), cube(T1.feature) <->  cube(T2.feature), cube(T1.feature) <=>  cube(T2.feature)
			from texture T1, texture T2 
			where T1.is_focus = TRUE AND T1 <> T2																			
		) ;
		END;
      
         $$ LANGUAGE plpgsql;  
		 
		 
      CREATE OR REPLACE FUNCTION insertColorDistance() 
	  RETURNS VOID AS $$
      BEGIN
	  
        INSERT INTO color_f_base (id_focus, id_feature, dist_l1, dist_l2, dist_linf) (
			select C1.id_ft, C2.id_ft, cube(C1.feature) <#> cube(C2.feature), cube(C1.feature) <->  cube(C2.feature), cube(C1.feature) <=>  cube(C2.feature)
			from color C1, color C2 
			where C1.is_focus = TRUE AND C1 <> C2																			
		) ;
		END;
      
         $$ LANGUAGE plpgsql;  
		 
		 
	CREATE OR REPLACE FUNCTION createShapeFocusBase(num integer)
	RETURNS VOID AS $$

	DECLARE fprev_id integer; fnext_id integer; distance float8; n_inserted integer := 0; border float8;
		BEGIN
			select id_ft into fprev_id from shape  -- SELECIONA UM DADO RANDOM
			order by random()
			LIMIT 1;
			LOOP
				select S2.id_ft, cube(S1.feature) <-> cube(S2.feature) as dist into fnext_id, distance from shape S1, shape S2 -- BASE CRIADA UTILIZANDO A DISTANCIA L2
				where S1.id_ft = fprev_id AND S2.is_focus = 'False'
				order by dist DESC
				LIMIT 1;
				
				update shape
				set is_focus = 'True'
				where id_ft = fnext_id;
				
				n_inserted = n_inserted + 1;
				num = num - 1;
				EXIT WHEN num = 0;
				IF n_inserted = 2 THEN
					select cube(S1.feature) <-> cube(S2.feature) into border from shape S1, shape S2  --CALCULA O VALOR DA BORDA
					where S1.id_ft = fprev_id and S2.id_ft = fnext_id;
					fprev_id = fnext_id;
						
					LOOP					
						select S2.id_ft, abs(border - (cube(S1.feature) <-> cube(S2.feature))) as err into fnext_id, distance from shape S1, shape S2
						where S1.id_ft = fprev_id AND S2.is_focus = 'False'
						order by err ASC	-- minimiza o erro
						LIMIT 1;
						
						update shape
						set is_focus = 'True'
						where id_ft = fnext_id;
						
						fprev_id = fnext_id;					
						num = num - 1;
						EXIT WHEN num <= 0;
					END LOOP;
				EXIT WHEN num = 0;
				END IF;
				
				fprev_id = fnext_id;
			END LOOP;
		END;$$
	LANGUAGE PLPGSQL;
	

	CREATE OR REPLACE FUNCTION selectTable(_tbl regclass) RETURNS setof shape AS $$
	DECLARE sql text;
	BEGIN
		sql := format('SELECT * FROM %s', _tbl);
	RETURN QUERY execute sql;
	END;$$
	LANGUAGE PLPGSQL;
	
	create type genericQuery as (cd_id int, distance FLOAT8);
	
	CREATE OR REPLACE FUNCTION rangeQuery (_tbl regclass, center_id integer, radius FLOAT8, metric TEXT) RETURNS SETOF genericQuery AS $$
	DECLARE sql text;
	BEGIN
		IF UPPER(metric) = 'L1' THEN
			sql := format('SELECT * FROM (SELECT T2.ID_FT, (cube(T1.feature) <#> cube(T2.feature)) AS dist FROM %s T1, %s T2 WHERE T1.id_ft = %s AND T1.ID_FT <> T2.ID_FT) as quer WHERE dist <= %s',_tbl, _tbl, center_id, radius);
						
		ELSIF UPPER(metric) = 'L2' THEN
			sql := format('SELECT * FROM (SELECT T2.ID_FT, (cube(T1.feature) <-> cube(T2.feature)) AS dist FROM %s T1, %s T2 WHERE T1.id_ft = %s AND T1.ID_FT <> T2.ID_FT) as quer WHERE dist <= %s',_tbl, _tbl, center_id, radius);
			
		ELSIF UPPER(metric) = 'LINF' THEN
			sql := format('SELECT * FROM (SELECT T2.ID_FT, (cube(T1.feature) <=> cube(T2.feature)) AS dist FROM %s T1, %s T2 WHERE T1.id_ft = %s AND T1.ID_FT <> T2.ID_FT) as quer WHERE dist <= %s',_tbl, _tbl, center_id, radius);
			
		END IF;
		
		RETURN QUERY execute sql;
		
	END;$$
	LANGUAGE PLPGSQL;
	
	
	