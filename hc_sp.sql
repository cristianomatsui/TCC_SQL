	
	
CREATE OR REPLACE FUNCTION rq_HC_L2 (center_id integer, radius FLOAT8) RETURNS SETOF genericQuery AS $$
	DECLARE sql text;
	BEGIN

		RETURN QUERY SELECT DISTINCT * FROM (SELECT T2.COD, (cube(T1.feature) <-> cube(T2.feature)) AS dist FROM HC_TABLE T1, HC_TABLE T2 WHERE T1.cod = center_id) as quer WHERE dist <= radius ORDER BY dist;
		
	END;$$
	LANGUAGE PLPGSQL;
	
	
CREATE OR REPLACE FUNCTION createHCFocusBase(num integer)
	RETURNS VOID AS $$

	DECLARE fprev_id integer; fnext_id integer; distance float8; n_inserted integer := 0; border float8;
		BEGIN
			PERFORM wipe_HC_focus_base();
			select cod into fprev_id from HC_TABLE  -- SELECIONA UM DADO RANDOM
			order by random()
			LIMIT 1;
			LOOP
				select S2.cod, cube(S1.feature) <-> cube(S2.feature) as dist into fnext_id, distance from HC_TABLE S1, HC_TABLE S2 -- BASE CRIADA UTILIZANDO A DISTANCIA L2
				where S1.cod = fprev_id AND S2.is_focus = 'False'
				order by dist DESC
				LIMIT 1;
				
				update HC_TABLE
				set is_focus = 'True'
				where cod = fnext_id;
				
				n_inserted = n_inserted + 1;
				num = num - 1;
				RAISE NOTICE 'Num = %, n_inserted = %', num, n_inserted;
				EXIT WHEN num = 0;
				IF n_inserted = 2 THEN
					select cube(S1.feature) <-> cube(S2.feature) into border from HC_TABLE S1, HC_TABLE S2  --CALCULA O VALOR DA BORDA
					where S1.cod = fprev_id and S2.cod = fnext_id;
					fprev_id = fnext_id;
						
					LOOP					
						select S2.cod, abs(border - (cube(S1.feature) <-> cube(S2.feature))) as err into fnext_id, distance from HC_TABLE S1, HC_TABLE S2
						where S1.cod = fprev_id AND S2.is_focus = 'False'
						order by err ASC	-- minimiza o erro
						LIMIT 1;
						
						update HC_TABLE
						set is_focus = 'True'
						where cod = fnext_id;
						
						fprev_id = fnext_id;					
						num = num - 1;
						EXIT WHEN num <= 0;
					END LOOP;
				EXIT WHEN num = 0;
				END IF;
				
				fprev_id = fnext_id;
			END LOOP;
		PERFORM insertHCDistance();
		RAISE NOTICE 'HC Focus Base created with % foci', n_inserted;
		END;$$
	LANGUAGE PLPGSQL;
	
	CREATE OR REPLACE FUNCTION insertHCDistance() 
	RETURNS VOID AS $$
	BEGIN

	INSERT INTO hc_f_base (id_focus, id_feature, dist_l1, dist_l2, dist_linf) (
		select S1.cod, S2.cod, cube(S1.feature) <#> cube(S2.feature), cube(S1.feature) <->  cube(S2.feature), cube(S1.feature) <=>  cube(S2.feature)
		from HC_TABLE S1, HC_TABLE S2 
		where S1.is_focus = TRUE AND S1 <> S2																			
	) ;
	END;

	 $$ LANGUAGE plpgsql; 
	 

CREATE OR REPLACE FUNCTION rangeOmniHCL2F1 (center_id integer, radius FLOAT8) RETURNS SETOF genericQuery AS $$
	DECLARE rec_id RECORD; is_ok BOOLEAN := FALSE;feature_aux FLOAT8[]; distance FLOAT8; dist_fc FLOAT8; cand INTEGER := 0;
	BEGIN
		
		select T1.feature into feature_aux FROM HC_TABLE T1 where T1.cod = center_id;
		select dist_l2 INTO dist_fc from HC_F_BASE where id_feature = center_id;
		
		
		FOR rec_id IN SELECT id_feature from HC_F_BASE WHERE ((dist_l2 < (radius + dist_fc)) AND (dist_l2 > dist_fc - radius)) LOOP
				cand = cand + 1;

				select (cube(feature_aux) <-> cube(feature)) INTO distance FROM HC_TABLE T1 WHERE rec_id.id_feature = T1.cod ;
				if (distance < radius) then
					RETURN NEXT (rec_id.id_feature, distance);			       
				end if;				
		END LOOP;
		
	RAISE NOTICE 'Focos: 1  Podas: %', (499999-cand);
				
	END;$$
	LANGUAGE PLPGSQL;
	
	
CREATE OR REPLACE FUNCTION rangeOmniHCL2F2 (center_id integer, radius FLOAT8) RETURNS SETOF genericQuery AS $$
	DECLARE rec_id RECORD; is_ok BOOLEAN := FALSE;feature_aux FLOAT8[]; distance FLOAT8; dist_fc FLOAT8[]; cand INTEGER := 0;
	BEGIN
		
		select T1.feature into feature_aux FROM HC_TABLE T1 where T1.cod = center_id;
		dist_fc = array(select distinct dist_l2 from HC_F_BASE where id_feature = center_id);
		--RAISE NOTICE 'dist_fc %  dist_fc2 %', dist_fc[1], dist_fc[2];
		
		FOR rec_id IN SELECT id_feature from HC_F_BASE WHERE ((dist_l2 < (radius + dist_fc[1])) AND (dist_l2 > dist_fc[1] - radius)) INTERSECT
					(SELECT id_feature from HC_F_BASE WHERE ((dist_l2 < (radius + dist_fc[2])) AND (dist_l2 > dist_fc[2] - radius))) LOOP
				cand = cand + 1;	
				select (cube(feature_aux) <-> cube(feature)) INTO distance FROM HC_TABLE T1 WHERE rec_id.id_feature = T1.cod ;
				if (distance < radius) then
					RETURN NEXT (rec_id.id_feature, distance);			       
				end if;				
		END LOOP;
		
		RAISE NOTICE 'Focos: 2  Podas: %', (499999-cand);
	
	END;$$
	LANGUAGE PLPGSQL;
	
CREATE OR REPLACE FUNCTION rangeOmniHCL2F3 (center_id integer, radius FLOAT8) RETURNS SETOF genericQuery AS $$
	DECLARE rec_id RECORD; is_ok BOOLEAN := FALSE;feature_aux FLOAT8[]; distance FLOAT8; dist_fc FLOAT8[]; cand INTEGER := 0;
	BEGIN
		
		select T1.feature into feature_aux FROM HC_TABLE T1 where T1.cod = center_id;
		dist_fc = array(select distinct dist_l2 from HC_F_BASE where id_feature = center_id);
		
		FOR rec_id IN SELECT id_feature from HC_F_BASE WHERE ((dist_l2 < (radius + dist_fc[1])) AND (dist_l2 > dist_fc[1] - radius)) INTERSECT
					(SELECT id_feature from HC_F_BASE WHERE ((dist_l2 < (radius + dist_fc[2])) AND (dist_l2 > dist_fc[2] - radius))) INTERSECT
					(SELECT id_feature from HC_F_BASE WHERE ((dist_l2 < (radius + dist_fc[3])) AND (dist_l2 > dist_fc[3] - radius))) LOOP
				
				cand = cand + 1;
				select (cube(feature_aux) <-> cube(feature)) INTO distance FROM HC_TABLE T1 WHERE rec_id.id_feature = T1.cod ;
				if (distance < radius) then
					RETURN NEXT (rec_id.id_feature, distance);			       
				end if;				
		END LOOP;
	
	RAISE NOTICE 'Focos: 3  Podas: %', (499999-cand);
	
	END;$$
	LANGUAGE PLPGSQL;
	
CREATE OR REPLACE FUNCTION rangeOmniHCL2F4 (center_id integer, radius FLOAT8) RETURNS SETOF genericQuery AS $$
	DECLARE rec_id RECORD; is_ok BOOLEAN := FALSE;feature_aux FLOAT8[]; distance FLOAT8; dist_fc FLOAT8[]; cand INTEGER := 0;
	BEGIN
		
		select T1.feature into feature_aux FROM HC_TABLE T1 where T1.cod = center_id;
		dist_fc = array(select distinct dist_l2 from HC_F_BASE where id_feature = center_id);
		
		FOR rec_id IN SELECT id_feature from HC_F_BASE WHERE ((dist_l2 < (radius + dist_fc[1])) AND (dist_l2 > dist_fc[1] - radius)) INTERSECT
					(SELECT id_feature from HC_F_BASE WHERE ((dist_l2 < (radius + dist_fc[2])) AND (dist_l2 > dist_fc[2] - radius))) INTERSECT
					(SELECT id_feature from HC_F_BASE WHERE ((dist_l2 < (radius + dist_fc[3])) AND (dist_l2 > dist_fc[3] - radius))) INTERSECT
					(SELECT id_feature from HC_F_BASE WHERE ((dist_l2 < (radius + dist_fc[4])) AND (dist_l2 > dist_fc[4] - radius))) LOOP
				
				cand = cand + 1;
				select (cube(feature_aux) <-> cube(feature)) INTO distance FROM HC_TABLE T1 WHERE rec_id.id_feature = T1.cod ;
				if (distance < radius) then
					RETURN NEXT (rec_id.id_feature, distance);			       
				end if;				
		END LOOP;
	
	RAISE NOTICE 'Focos: 4  Podas: %', (499999-cand);
	END;$$
	LANGUAGE PLPGSQL;
	
CREATE OR REPLACE FUNCTION rangeOmniHCL2F5 (center_id integer, radius FLOAT8) RETURNS SETOF genericQuery AS $$
	DECLARE rec_id RECORD; is_ok BOOLEAN := FALSE;feature_aux FLOAT8[]; distance FLOAT8; dist_fc FLOAT8[]; cand INTEGER := 0;
	BEGIN
		
		select T1.feature into feature_aux FROM HC_TABLE T1 where T1.cod = center_id;
		dist_fc = array(select distinct dist_l2 from HC_F_BASE where id_feature = center_id);
		
		FOR rec_id IN SELECT id_feature from HC_F_BASE WHERE ((dist_l2 < (radius + dist_fc[1])) AND (dist_l2 > dist_fc[1] - radius)) INTERSECT
					(SELECT id_feature from HC_F_BASE WHERE ((dist_l2 < (radius + dist_fc[2])) AND (dist_l2 > dist_fc[2] - radius))) INTERSECT
					(SELECT id_feature from HC_F_BASE WHERE ((dist_l2 < (radius + dist_fc[3])) AND (dist_l2 > dist_fc[3] - radius))) INTERSECT
					(SELECT id_feature from HC_F_BASE WHERE ((dist_l2 < (radius + dist_fc[4])) AND (dist_l2 > dist_fc[4] - radius))) INTERSECT
					(SELECT id_feature from HC_F_BASE WHERE ((dist_l2 < (radius + dist_fc[5])) AND (dist_l2 > dist_fc[5] - radius))) LOOP
				
				cand = cand + 1;
				select (cube(feature_aux) <-> cube(feature)) INTO distance FROM HC_TABLE T1 WHERE rec_id.id_feature = T1.cod ;
				if (distance < radius) then
					RETURN NEXT (rec_id.id_feature, distance);			       
				end if;				
		END LOOP;
	
	RAISE NOTICE 'Focos: 5  Podas: %', (499999-cand);
	END;$$
	LANGUAGE PLPGSQL;
	
	
CREATE OR REPLACE FUNCTION boxcounting (num_centers INTEGER) RETURNS VOID AS $$
	declare n_aux INTEGER[]; radius FLOAT8; rad_aux numeric; n_elements INTEGER := 0; center_id INTEGER;i INTEGER;count_aux FLOAT8 := 0;
	BEGIN
	
		FOR i in 1..num_centers LOOP
			n_aux[i] = 0;
		END LOOP;
	
		LOOP
			radius = 6.0;
			i = 1;
			select random_between(2, 499999) INTO center_id;
			RAISE NOTICE 'Center ID: % n_elem: %', center_id, n_elements;
			LOOP	
				select count(*) from rq_hc_l2(center_id, radius) INTO count_aux;
				RAISE NOTICE 'Count  %', count_aux;
				n_aux[i] = n_aux[i] + count_aux;
				radius = radius + 0.1;
				i = i + 1;
				EXIT WHEN i > num_centers;
			END LOOP;
			n_elements = n_elements + 1;
			EXIT WHEN n_elements = num_centers;
		END LOOP;
		
		FOR i IN 1..num_centers LOOP
			RAISE NOTICE 'n_aux[i] = %', n_aux[i];
			n_aux[i] = n_aux[i] / num_centers;
		END LOOP;
		
		rad_aux = 6.0;
		for i IN 1..num_centers LOOP
			INSERT INTO boxc_table VALUES (rad_aux, n_aux[i]);
			rad_aux = rad_aux + 0.1;
		END LOOP;
		
	END;$$
	LANGUAGE PLPGSQL;

CREATE OR REPLACE FUNCTION wipe_HC_focus_base() RETURNS VOID AS $$
	BEGIN
		update hc_table set is_focus = false where is_focus = true;
		delete from hc_f_base;
		RAISE NOTICE 'HC Focus Base wiped';
		
	END;$$
	LANGUAGE PLPGSQL;
	
	(49268, 20)		