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
		PERFORM wipe_shape_focus_base();
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
				RAISE NOTICE 'Num = %, n_inserted = %', num, n_inserted;
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
			PERFORM insertShapeDistance();
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
			sql := format('SELECT DISTINCT * FROM (SELECT T2.ID_FT, (cube(T1.feature) <#> cube(T2.feature)) AS dist FROM %s T1, %s T2 WHERE T1.id_ft = %s) as quer WHERE dist <= %s ORDER BY dist',_tbl, _tbl, center_id, radius);
						
		ELSIF UPPER(metric) = 'L2' THEN
			sql := format('SELECT DISTINCT * FROM (SELECT T2.ID_FT, (cube(T1.feature) <-> cube(T2.feature)) AS dist FROM %s T1, %s T2 WHERE T1.id_ft = %s) as quer WHERE dist <= %s ORDER BY dist',_tbl, _tbl, center_id, radius);
			
		ELSIF UPPER(metric) = 'LINF' THEN
			sql := format('SELECT DISTINCT * FROM (SELECT T2.ID_FT, (cube(T1.feature) <=> cube(T2.feature)) AS dist FROM %s T1, %s T2 WHERE T1.id_ft = %s) as quer WHERE dist <= %s ORDER BY dist',_tbl, _tbl, center_id, radius);
			
		END IF;
		
		RETURN QUERY execute sql;
		
	END;$$
	LANGUAGE PLPGSQL;
	
	CREATE OR REPLACE FUNCTION kNNQuery (_tbl regclass, center_id integer, k_neigh integer, metric TEXT) RETURNS SETOF genericQuery AS $$
	DECLARE sql text;
	BEGIN
		IF UPPER(metric) = 'L1' THEN
			sql := format('SELECT * FROM (SELECT T2.ID_FT, (cube(T1.feature) <#> cube(T2.feature)) AS dist FROM %s T1, %s T2 WHERE T1.id_ft = %s AND T1.ID_FT <> T2.ID_FT) as quer ORDER BY dist LIMIT %s',_tbl, _tbl, center_id, k_neigh);
		
		ELSIF UPPER(metric) = 'L2' THEN
			sql := format('SELECT * FROM (SELECT T2.ID_FT, (cube(T1.feature) <-> cube(T2.feature)) AS dist FROM %s T1, %s T2 WHERE T1.id_ft = %s AND T1.ID_FT <> T2.ID_FT) as quer ORDER BY dist LIMIT %s',_tbl, _tbl, center_id, k_neigh);
		
		ELSIF UPPER(metric) = 'LINF' THEN
			sql := format('SELECT * FROM (SELECT T2.ID_FT, (cube(T1.feature) <=> cube(T2.feature)) AS dist FROM %s T1, %s T2 WHERE T1.id_ft = %s AND T1.ID_FT <> T2.ID_FT) as quer ORDER BY dist LIMIT %s',_tbl, _tbl, center_id, k_neigh);
		
		END IF;
		
		RETURN QUERY execute sql;
		
	END;$$
	LANGUAGE PLPGSQL;
	
CREATE OR REPLACE FUNCTION rangeOmniShapeL1 (center_id integer, radius FLOAT8) RETURNS SETOF genericQuery AS $$
	DECLARE rec_id RECORD; rec_focus RECORD; is_ok BOOLEAN := FALSE; dist_aux FLOAT8;
	BEGIN
	
		DROP TABLE IF EXISTS focus_Table;
		DROP TABLE IF EXISTS fc_dist;
		DROP TABLE IF EXISTS candidates;
		DROP TABLE IF EXISTS ans_set;
		
		CREATE TEMPORARY TABLE focus_Table as SELECT * FROM SHAPE WHERE is_focus = TRUE;
		CREATE TEMPORARY TABLE fc_dist (id_focus INTEGER, dist FLOAT8);
		CREATE TEMPORARY TABLE candidates (id_ft INTEGER, feature FLOAT8[]);
		CREATE TEMPORARY TABLE ans_set (id_ft INTEGER, dist FLOAT8);

		INSERT INTO fc_dist(SELECT T1.id_ft, (cube(T1.feature) <#> cube(T2.feature)) FROM focus_Table T1, SHAPE T2 WHERE T2.id_ft = center_id);
		INSERT INTO ans_set (SELECT T1.id_focus, T1.dist FROM fc_dist T1 WHERE (T1.dist < radius)); --Focos com a distância até sq < raio automaticamente são respostas
		
		FOR rec_id IN SELECT id_ft, feature from SHAPE WHERE is_focus = false LOOP
			
			FOR rec_focus in SELECT fc_dist.id_focus, id_feature, dist_l1 AS dist_f, dist FROM shape_f_base JOIN fc_dist ON shape_f_base.id_focus = fc_dist.id_focus WHERE id_feature = rec_id.id_ft LOOP
		
				IF (abs(rec_focus.dist_f - rec_focus.dist) < radius) THEN
					is_ok = TRUE;
				ELSE is_ok = FALSE; EXIT;
				END IF;
			END LOOP;

			IF is_ok = TRUE THEN
				insert into candidates (select rec_id.id_ft, rec_id.feature);
			END IF;
		END LOOP;
		
		INSERT INTO ans_set (SELECT * FROM (SELECT * FROM (SELECT T1.id_ft, (cube(T1.feature) <#> cube(T2.feature)) as distance FROM candidates T1, SHAPE T2 WHERE T2.id_ft = center_id) AS QUER2) as quer WHERE distance < radius);
		
		RETURN QUERY EXECUTE 'SELECT * FROM ANS_SET ORDER BY DIST ASC';
		
	END;$$
	LANGUAGE PLPGSQL;

CREATE OR REPLACE FUNCTION rangeOmniShapeL2 (center_id integer, radius FLOAT8) RETURNS SETOF genericQuery AS $$ -- das funções é a mais atual
	DECLARE rec_id RECORD; rec_focus RECORD; is_ok BOOLEAN := FALSE; dist_aux FLOAT8;
	BEGIN
	
		DROP TABLE IF EXISTS fc_dist;
		DROP TABLE IF EXISTS candidates;
		DROP TABLE IF EXISTS ans_set;	

		CREATE TEMPORARY TABLE fc_dist (id_focus INTEGER, dist FLOAT8);
		CREATE TEMPORARY TABLE candidates (id_ft INTEGER, feature FLOAT8[]);
		CREATE TEMPORARY TABLE ans_set (id_ft INTEGER, dist FLOAT8);

		INSERT INTO fc_dist(SELECT T1.id_ft, (cube(T1.feature) <-> cube(T2.feature)) FROM SHAPE T1, SHAPE T2 WHERE T2.id_ft = center_id AND T1.is_focus = True);
		INSERT INTO ans_set (SELECT T1.id_focus, T1.dist FROM fc_dist T1 WHERE (T1.dist < radius)); --Focos com a distância até sq < raio automaticamente são respostas
		
		FOR rec_id IN SELECT id_ft, feature from SHAPE WHERE is_focus = false LOOP
			
			FOR rec_focus in SELECT fc_dist.id_focus, id_feature, dist_l2 AS dist_f, dist FROM shape_f_base JOIN fc_dist ON shape_f_base.id_focus = fc_dist.id_focus WHERE id_feature = rec_id.id_ft LOOP
		
				IF (abs(rec_focus.dist_f - rec_focus.dist) < radius) THEN
					is_ok = TRUE;
				ELSE is_ok = FALSE; EXIT;
				END IF;
			END LOOP;

			IF is_ok = TRUE THEN
				insert into candidates (select rec_id.id_ft, rec_id.feature);
			END IF;
		END LOOP;
		
		INSERT INTO ans_set (SELECT * FROM (SELECT * FROM (SELECT T1.id_ft, (cube(T1.feature) <-> cube(T2.feature)) as distance FROM candidates T1, SHAPE T2 WHERE T2.id_ft = center_id) AS QUER2) as quer WHERE distance < radius);
		
		RETURN QUERY EXECUTE 'SELECT * FROM ANS_SET ORDER BY DIST ASC';
		
	END;$$
	LANGUAGE PLPGSQL;	
			
		
CREATE OR REPLACE FUNCTION rangeOmni (_tbl regclass, center_id integer, radius FLOAT8, metric TEXT) RETURNS SETOF genericQuery AS $$
	DECLARE sql TEXT; rec_id RECORD; rec_focus RECORD; is_ok BOOLEAN := FALSE; var INTEGER; metricType TEXT; dist_aux FLOAT8;			
	BEGIN	

			
		DROP TABLE IF EXISTS focus_Table;
		DROP TABLE IF EXISTS fc_dist;
		DROP TABLE IF EXISTS candidates;
		DROP TABLE IF EXISTS ans_set;
		
		sql := format('CREATE TEMPORARY TABLE focus_Table as SELECT * FROM %s WHERE is_focus = TRUE', _tbl);
		execute sql;
		CREATE TEMPORARY TABLE fc_dist (id_focus INTEGER, dist FLOAT8);
		CREATE TEMPORARY TABLE candidates (id_ft INTEGER, feature FLOAT8[]);
		CREATE TEMPORARY TABLE ans_set (id_ft INTEGER, dist FLOAT8);
		
		IF UPPER(metric) = 'L1' THEN metricType := '<#>';
		ELSIF UPPER(metric) = 'L2' THEN metricType := '<->';
		ELSIF UPPER(metric) = 'LINF' THEN metricType := '<=>';
		ELSE RAISE EXCEPTION 'Métrica % inválida', metric USING HINT = 'Use l1, l2 ou linf';
		END IF;
		
		sql := format('INSERT INTO fc_dist(SELECT T1.id_ft, (cube(T1.feature) %s cube(T2.feature)) FROM focus_Table T1, %s T2 WHERE T2.id_ft = %s)',metricType, _tbl, center_id); --Calculo da distância sq até os focus
		execute sql;
		
		sql := format ('INSERT INTO ans_set (SELECT T1.id_focus, T1.dist FROM fc_dist T1 WHERE (T1.dist < %s))', radius); --Focos com a distância até sq < raio automaticamente são respostas
		execute sql;
					
		sql := format('SELECT id_ft, feature from %s WHERE is_focus = false', _tbl);
		FOR rec_id IN EXECUTE sql
		LOOP
		
			select rec_id.id_ft into var; --gambiarra
			sql := format('SELECT fc_dist.id_focus, id_feature, dist_%s AS dist_f, dist FROM %s_f_base JOIN fc_dist ON %s_f_base.id_focus = fc_dist.id_focus WHERE id_feature = %s',metric, _tbl, _tbl, var);
			FOR rec_focus in EXECUTE sql USING var --gambiarra
			LOOP 
			
				RAISE NOTICE 'rec.dist: %', rec_focus.dist_f;
				IF (abs(rec_focus.dist_f - rec_focus.dist) < radius) THEN
					is_ok = TRUE;
				ELSE is_ok = FALSE; EXIT;
				END IF;
			END LOOP;

			IF is_ok = TRUE THEN
				insert into candidates (select rec_id.id_ft, rec_id.feature);
			END IF;
		END LOOP;
		
		sql := format('INSERT INTO ans_set (SELECT * FROM (SELECT * FROM (SELECT T1.id_ft, (cube(T1.feature) %s cube(T2.feature)) as distance FROM candidates T1, %s T2 WHERE T2.id_ft = %s) AS QUER2) as quer WHERE distance < %s)',metricType, _tbl, center_id, radius);
		EXECUTE sql;
		
		RETURN QUERY EXECUTE 'SELECT * FROM ANS_SET ORDER BY DIST ASC';

		END;$$
	LANGUAGE PLPGSQL;
	
	CREATE OR REPLACE FUNCTION rangeKNN (_tbl regclass, center_id integer, k_param integer, metric TEXT) RETURNS SETOF genericQuery AS $$
		DECLARE sql TEXT; init_dist FLOAT8 := 100; is_ok BOOLEAN := FALSE;  --parâmetro inicial da rq
		BEGIN
			DROP TABLE IF EXISTS knn_set;
			CREATE TEMPORARY TABLE knn_set (id_ft INTEGER, dist FLOAT8);
			LOOP
				sql := format('INSERT INTO knn_set (SELECT * FROM rangeOmni(''%s'',%s,%s,''%s''))', _tbl, center_id, init_dist, metric); --kNN começa com uma rangeQuery
				EXECUTE SQL;
				IF ((SELECT COUNT (*) FROM knn_set) < k_param) THEN
					DELETE FROM knn_set;
					init_dist := 2*init_dist;
				ELSE
					EXIT;
				END IF;
			END LOOP;
			sql := format('SELECT * FROM knn_set ORDER BY DIST ASC LIMIT %s', k_param);
			RETURN QUERY EXECUTE sql;
		END;$$
	LANGUAGE PLPGSQL;

CREATE OR REPLACE FUNCTION rangeQueryShapeL2 (center_id integer, radius FLOAT8) RETURNS SETOF genericQuery AS $$

	BEGIN

		RETURN QUERY SELECT DISTINCT * FROM (SELECT T2.id_ft, (cube(T1.feature) <-> cube(T2.feature)) AS dist FROM SHAPE T1, SHAPE T2 WHERE T1.id_ft = center_id) as quer WHERE dist <= radius ORDER BY dist;
		
	END;$$
	LANGUAGE PLPGSQL;
	
CREATE OR REPLACE FUNCTION rangeQueryShapeLInf(center_id integer, radius FLOAT8) RETURNS SETOF genericQuery AS $$

	BEGIN

		RETURN QUERY SELECT DISTINCT * FROM (SELECT T2.id_ft, (cube(T1.feature) <=> cube(T2.feature)) AS dist FROM SHAPE T1, SHAPE T2 WHERE T1.id_ft = center_id) as quer WHERE dist <= radius ORDER BY dist;
		
	END;$$
	LANGUAGE PLPGSQL;
	
	
CREATE OR REPLACE FUNCTION rangeOmniShapeL2F1 (center_id integer, radius FLOAT8) RETURNS SETOF genericQuery AS $$
	DECLARE rec_id RECORD; is_ok BOOLEAN := FALSE;feature_aux FLOAT8[]; distance FLOAT8; dist_fc FLOAT8; cand INTEGER := 0;
	BEGIN
		
		select T1.feature into feature_aux FROM Shape T1 where T1.id_ft = center_id;
		select dist_l2 INTO dist_fc from SHAPE_F_BASE where id_feature = center_id;
		
		
		FOR rec_id IN SELECT id_feature from SHAPE_F_BASE WHERE ((dist_l2 < (radius + dist_fc)) AND (dist_l2 > dist_fc - radius)) LOOP
				cand = cand + 1;

				select (cube(feature_aux) <-> cube(feature)) INTO distance FROM SHAPE T1 WHERE rec_id.id_feature = T1.id_ft ;
				if (distance < radius) then
					RETURN NEXT (rec_id.id_feature, distance);			       
				end if;				
		END LOOP;
		
	RAISE NOTICE 'Focos: 1  Podas: %', (24997-cand);
				
	END;$$
	LANGUAGE PLPGSQL;
	
CREATE OR REPLACE FUNCTION rangeOmniShapeL2F2 (center_id integer, radius FLOAT8) RETURNS SETOF genericQuery AS $$
	DECLARE rec_id RECORD; is_ok BOOLEAN := FALSE;feature_aux FLOAT8[]; distance FLOAT8; dist_fc FLOAT8[]; cand INTEGER := 0;
	BEGIN
		
		select T1.feature into feature_aux FROM Shape T1 where T1.id_ft = center_id;
		dist_fc = array(select distinct dist_l2 from SHAPE_F_BASE where id_feature = center_id);
		
		
		FOR rec_id IN SELECT id_feature from SHAPE_F_BASE WHERE ((dist_l2 < (radius + dist_fc[1])) AND (dist_l2 > dist_fc[1] - radius)) INTERSECT
					(SELECT id_feature from SHAPE_F_BASE WHERE ((dist_l2 < (radius + dist_fc[2])) AND (dist_l2 > dist_fc[2] - radius))) LOOP
				cand = cand + 1;

				select (cube(feature_aux) <-> cube(feature)) INTO distance FROM SHAPE T1 WHERE rec_id.id_feature = T1.id_ft ;
				if (distance < radius) then
					RETURN NEXT (rec_id.id_feature, distance);			       
				end if;				
		END LOOP;
		
	RAISE NOTICE 'Focos: 2  Podas: %', (24997-cand);
				
	END;$$
	LANGUAGE PLPGSQL;
	
CREATE OR REPLACE FUNCTION rangeOmniShapeL2F3 (center_id integer, radius FLOAT8) RETURNS SETOF genericQuery AS $$
	DECLARE rec_id RECORD; is_ok BOOLEAN := FALSE;feature_aux FLOAT8[]; distance FLOAT8; dist_fc FLOAT8[]; cand INTEGER := 0;
	BEGIN
		
		select T1.feature into feature_aux FROM Shape T1 where T1.id_ft = center_id;
		dist_fc = array(select distinct dist_l2 from SHAPE_F_BASE where id_feature = center_id);
		
		
		FOR rec_id IN SELECT id_feature from SHAPE_F_BASE WHERE ((dist_l2 < (radius + dist_fc[1])) AND (dist_l2 > dist_fc[1] - radius)) INTERSECT
					(SELECT id_feature from SHAPE_F_BASE WHERE ((dist_l2 < (radius + dist_fc[2])) AND (dist_l2 > dist_fc[2] - radius))) INTERSECT
					(SELECT id_feature from SHAPE_F_BASE WHERE ((dist_l2 < (radius + dist_fc[3])) AND (dist_l2 > dist_fc[3] - radius)))  LOOP
				cand = cand + 1;

				select (cube(feature_aux) <-> cube(feature)) INTO distance FROM SHAPE T1 WHERE rec_id.id_feature = T1.id_ft ;
				if (distance < radius) then
					RETURN NEXT (rec_id.id_feature, distance);			       
				end if;				
		END LOOP;
		
	RAISE NOTICE 'Focos: 3  Podas: %', (24997-cand);
				
	END;$$
	LANGUAGE PLPGSQL;
	
CREATE OR REPLACE FUNCTION rangeOmniShapeL2F4 (center_id integer, radius FLOAT8) RETURNS SETOF genericQuery AS $$
	DECLARE rec_id RECORD; is_ok BOOLEAN := FALSE;feature_aux FLOAT8[]; distance FLOAT8; dist_fc FLOAT8[]; cand INTEGER := 0;
	BEGIN
		
		select T1.feature into feature_aux FROM Shape T1 where T1.id_ft = center_id;
		dist_fc = array(select distinct dist_l2 from SHAPE_F_BASE where id_feature = center_id);
		RAISE NOTICE 'dist1 % dist2 % dist3 % dist4 %', dist_fc[1], dist_fc[2], dist_fc[3], dist_fc[4];
		
		FOR rec_id IN SELECT id_feature from SHAPE_F_BASE WHERE ((dist_l2 < (radius + dist_fc[1])) AND (dist_l2 > dist_fc[1] - radius)) INTERSECT
					(SELECT id_feature from SHAPE_F_BASE WHERE ((dist_l2 < (radius + dist_fc[2])) AND (dist_l2 > dist_fc[2] - radius))) INTERSECT
					(SELECT id_feature from SHAPE_F_BASE WHERE ((dist_l2 < (radius + dist_fc[3])) AND (dist_l2 > dist_fc[3] - radius))) INTERSECT
					(SELECT id_feature from SHAPE_F_BASE WHERE ((dist_l2 < (radius + dist_fc[4])) AND (dist_l2 > dist_fc[4] - radius))) LOOP
				cand = cand + 1;

				select (cube(feature_aux) <-> cube(feature)) INTO distance FROM SHAPE T1 WHERE rec_id.id_feature = T1.id_ft ;
				if (distance < radius) then
					RETURN NEXT (rec_id.id_feature, distance);			       
				end if;				
		END LOOP;
		
	RAISE NOTICE 'Focos: 4  Podas: %', (24997-cand);
				
	END;$$
	LANGUAGE PLPGSQL;
	
CREATE OR REPLACE FUNCTION rangeOmniShapeL2F5 (center_id integer, radius FLOAT8) RETURNS SETOF genericQuery AS $$
	DECLARE rec_id RECORD; is_ok BOOLEAN := FALSE;feature_aux FLOAT8[]; distance FLOAT8; dist_fc FLOAT8[]; cand INTEGER := 0;
	BEGIN
		
		select T1.feature into feature_aux FROM Shape T1 where T1.id_ft = center_id;
		dist_fc = array(select distinct dist_l2 from SHAPE_F_BASE where id_feature = center_id);
		--RAISE NOTICE 'dist1 % dist2 % dist3 % dist4 %', dist_fc[1], dist_fc[2], dist_fc[3], dist_fc[4];
		
		FOR rec_id IN SELECT id_feature from SHAPE_F_BASE WHERE ((dist_l2 < (radius + dist_fc[1])) AND (dist_l2 > dist_fc[1] - radius)) INTERSECT
					(SELECT id_feature from SHAPE_F_BASE WHERE ((dist_l2 < (radius + dist_fc[2])) AND (dist_l2 > dist_fc[2] - radius))) INTERSECT
					(SELECT id_feature from SHAPE_F_BASE WHERE ((dist_l2 < (radius + dist_fc[3])) AND (dist_l2 > dist_fc[3] - radius))) INTERSECT
					(SELECT id_feature from SHAPE_F_BASE WHERE ((dist_l2 < (radius + dist_fc[4])) AND (dist_l2 > dist_fc[4] - radius))) INTERSECT
					(SELECT id_feature from SHAPE_F_BASE WHERE ((dist_l2 < (radius + dist_fc[5])) AND (dist_l2 > dist_fc[5] - radius))) LOOP
				cand = cand + 1;

				select (cube(feature_aux) <-> cube(feature)) INTO distance FROM SHAPE T1 WHERE rec_id.id_feature = T1.id_ft ;
				if (distance < radius) then
					RETURN NEXT (rec_id.id_feature, distance);			       
				end if;				
		END LOOP;
		
	RAISE NOTICE 'Focos: 5  Podas: %', (24997-cand);
				
	END;$$
	LANGUAGE PLPGSQL;
	
CREATE OR REPLACE FUNCTION wipe_shape_focus_base () RETURNS VOID AS $$
	BEGIN
		update shape set is_focus = false where is_focus = true;
		delete from shape_f_base;
		RAISE NOTICE 'Shape Focus Base wiped';
		
	END;$$
	LANGUAGE PLPGSQL;
	


	
CREATE OR REPLACE FUNCTION rangeOmniShapeL1F1 (center_id integer, radius FLOAT8) RETURNS SETOF genericQuery AS $$
	DECLARE rec_id RECORD; is_ok BOOLEAN := FALSE;feature_aux FLOAT8[]; distance FLOAT8; dist_fc FLOAT8; cand INTEGER := 0;
	BEGIN
		
		select T1.feature into feature_aux FROM Shape T1 where T1.id_ft = center_id;
		select dist_l1 INTO dist_fc from SHAPE_F_BASE where id_feature = center_id;
		
		
		FOR rec_id IN SELECT id_feature from SHAPE_F_BASE WHERE ((dist_l1 < (radius + dist_fc)) AND (dist_l1 > dist_fc - radius)) LOOP
				cand = cand + 1;

				select (cube(feature_aux) <#> cube(feature)) INTO distance FROM SHAPE T1 WHERE rec_id.id_feature = T1.id_ft ;
				if (distance < radius) then
					RETURN NEXT (rec_id.id_feature, distance);			       
				end if;				
		END LOOP;
		
	RAISE NOTICE 'Focos: 1  Podas: %', (24997-cand);
				
	END;$$
	LANGUAGE PLPGSQL;
	
CREATE OR REPLACE FUNCTION rangeOmniShapeL1F2 (center_id integer, radius FLOAT8) RETURNS SETOF genericQuery AS $$
	DECLARE rec_id RECORD; is_ok BOOLEAN := FALSE;feature_aux FLOAT8[]; distance FLOAT8; dist_fc FLOAT8[]; cand INTEGER := 0;
	BEGIN
		
		select T1.feature into feature_aux FROM Shape T1 where T1.id_ft = center_id;
		dist_fc = array(select distinct dist_l1 from SHAPE_F_BASE where id_feature = center_id);
		
		
		FOR rec_id IN SELECT id_feature from SHAPE_F_BASE WHERE ((dist_l1 < (radius + dist_fc[1])) AND (dist_l1 > dist_fc[1] - radius)) INTERSECT
					(SELECT id_feature from SHAPE_F_BASE WHERE ((dist_l1 < (radius + dist_fc[2])) AND (dist_l1 > dist_fc[2] - radius))) LOOP
				cand = cand + 1;

				select (cube(feature_aux) <#> cube(feature)) INTO distance FROM SHAPE T1 WHERE rec_id.id_feature = T1.id_ft ;
				if (distance < radius) then
					RETURN NEXT (rec_id.id_feature, distance);			       
				end if;				
		END LOOP;
		
	RAISE NOTICE 'Focos: 2  Podas: %', (24997-cand);
				
	END;$$
	LANGUAGE PLPGSQL;	
	

CREATE OR REPLACE FUNCTION rangeOmniShapeL1F3 (center_id integer, radius FLOAT8) RETURNS SETOF genericQuery AS $$
	DECLARE rec_id RECORD; is_ok BOOLEAN := FALSE;feature_aux FLOAT8[]; distance FLOAT8; dist_fc FLOAT8[]; cand INTEGER := 0;
	BEGIN
		
		select T1.feature into feature_aux FROM Shape T1 where T1.id_ft = center_id;
		dist_fc = array(select distinct dist_l1 from SHAPE_F_BASE where id_feature = center_id);
		
		
		FOR rec_id IN SELECT id_feature from SHAPE_F_BASE WHERE ((dist_l1 < (radius + dist_fc[1])) AND (dist_l1 > dist_fc[1] - radius)) INTERSECT
					(SELECT id_feature from SHAPE_F_BASE WHERE ((dist_l1 < (radius + dist_fc[2])) AND (dist_l1 > dist_fc[2] - radius))) INTERSECT
					(SELECT id_feature from SHAPE_F_BASE WHERE ((dist_l1 < (radius + dist_fc[3])) AND (dist_l1 > dist_fc[3] - radius)))LOOP
				cand = cand + 1;

				select (cube(feature_aux) <#> cube(feature)) INTO distance FROM SHAPE T1 WHERE rec_id.id_feature = T1.id_ft ;
				if (distance < radius) then
					RETURN NEXT (rec_id.id_feature, distance);			       
				end if;				
		END LOOP;
		
	RAISE NOTICE 'Focos: 3  Podas: %', (24997-cand);
				
	END;$$
	LANGUAGE PLPGSQL;

CREATE OR REPLACE FUNCTION rangeOmniShapeL1F4 (center_id integer, radius FLOAT8) RETURNS SETOF genericQuery AS $$
	DECLARE rec_id RECORD; is_ok BOOLEAN := FALSE;feature_aux FLOAT8[]; distance FLOAT8; dist_fc FLOAT8[]; cand INTEGER := 0;
	BEGIN
		
		select T1.feature into feature_aux FROM Shape T1 where T1.id_ft = center_id;
		dist_fc = array(select distinct dist_l1 from SHAPE_F_BASE where id_feature = center_id);
		
		
		FOR rec_id IN SELECT id_feature from SHAPE_F_BASE WHERE ((dist_l1 < (radius + dist_fc[1])) AND (dist_l1 > dist_fc[1] - radius)) INTERSECT
					(SELECT id_feature from SHAPE_F_BASE WHERE ((dist_l1 < (radius + dist_fc[2])) AND (dist_l1 > dist_fc[2] - radius))) INTERSECT
					(SELECT id_feature from SHAPE_F_BASE WHERE ((dist_l1 < (radius + dist_fc[3])) AND (dist_l1 > dist_fc[3] - radius))) INTERSECT
					(SELECT id_feature from SHAPE_F_BASE WHERE ((dist_l1 < (radius + dist_fc[4])) AND (dist_l1 > dist_fc[4] - radius)))LOOP
				cand = cand + 1;

				select (cube(feature_aux) <#> cube(feature)) INTO distance FROM SHAPE T1 WHERE rec_id.id_feature = T1.id_ft ;
				if (distance < radius) then
					RETURN NEXT (rec_id.id_feature, distance);			       
				end if;				
		END LOOP;
		
	RAISE NOTICE 'Focos: 4  Podas: %', (24997-cand);
				
	END;$$
	LANGUAGE PLPGSQL;

CREATE OR REPLACE FUNCTION rangeOmniShapeL1F5 (center_id integer, radius FLOAT8) RETURNS SETOF genericQuery AS $$
	DECLARE rec_id RECORD; is_ok BOOLEAN := FALSE;feature_aux FLOAT8[]; distance FLOAT8; dist_fc FLOAT8[]; cand INTEGER := 0;
	BEGIN
		
		select T1.feature into feature_aux FROM Shape T1 where T1.id_ft = center_id;
		dist_fc = array(select distinct dist_l1 from SHAPE_F_BASE where id_feature = center_id);
		
		
		FOR rec_id IN SELECT id_feature from SHAPE_F_BASE WHERE ((dist_l1 < (radius + dist_fc[1])) AND (dist_l1 > dist_fc[1] - radius)) INTERSECT
					(SELECT id_feature from SHAPE_F_BASE WHERE ((dist_l1 < (radius + dist_fc[2])) AND (dist_l1 > dist_fc[2] - radius))) INTERSECT
					(SELECT id_feature from SHAPE_F_BASE WHERE ((dist_l1 < (radius + dist_fc[3])) AND (dist_l1 > dist_fc[3] - radius))) INTERSECT
					(SELECT id_feature from SHAPE_F_BASE WHERE ((dist_l1 < (radius + dist_fc[4])) AND (dist_l1 > dist_fc[4] - radius)))INTERSECT
					(SELECT id_feature from SHAPE_F_BASE WHERE ((dist_l1 < (radius + dist_fc[5])) AND (dist_l1 > dist_fc[5] - radius)))LOOP
				cand = cand + 1;

				select (cube(feature_aux) <#> cube(feature)) INTO distance FROM SHAPE T1 WHERE rec_id.id_feature = T1.id_ft ;
				if (distance < radius) then
					RETURN NEXT (rec_id.id_feature, distance);			       
				end if;				
		END LOOP;
		
	RAISE NOTICE 'Focos: 5  Podas: %', (24997-cand);
				
	END;$$
	LANGUAGE PLPGSQL;		
	
	
CREATE OR REPLACE FUNCTION rangeOmniShapeLInfF1 (center_id integer, radius FLOAT8) RETURNS SETOF genericQuery AS $$
	DECLARE rec_id RECORD; is_ok BOOLEAN := FALSE;feature_aux FLOAT8[]; distance FLOAT8; dist_fc FLOAT8; cand INTEGER := 0;
	BEGIN
		
		select T1.feature into feature_aux FROM Shape T1 where T1.id_ft = center_id;
		select dist_linf INTO dist_fc from SHAPE_F_BASE where id_feature = center_id;
		
		
		FOR rec_id IN SELECT id_feature from SHAPE_F_BASE WHERE ((dist_linf < (radius + dist_fc)) AND (dist_linf > dist_fc - radius)) LOOP
				cand = cand + 1;

				select (cube(feature_aux) <=> cube(feature)) INTO distance FROM SHAPE T1 WHERE rec_id.id_feature = T1.id_ft ;
				if (distance < radius) then
					RETURN NEXT (rec_id.id_feature, distance);			       
				end if;				
		END LOOP;
		
	RAISE NOTICE 'Focos: 1  Podas: %', (24997-cand);
				
	END;$$
	LANGUAGE PLPGSQL;
	
CREATE OR REPLACE FUNCTION rangeOmniShapeLInfF2 (center_id integer, radius FLOAT8) RETURNS SETOF genericQuery AS $$
	DECLARE rec_id RECORD; is_ok BOOLEAN := FALSE;feature_aux FLOAT8[]; distance FLOAT8; dist_fc FLOAT8[]; cand INTEGER := 0;
	BEGIN
		
		select T1.feature into feature_aux FROM Shape T1 where T1.id_ft = center_id;
		dist_fc = array(select distinct dist_linf from SHAPE_F_BASE where id_feature = center_id);
		
		
		FOR rec_id IN SELECT id_feature from SHAPE_F_BASE WHERE ((dist_linf < (radius + dist_fc[1])) AND (dist_linf > dist_fc[1] - radius)) INTERSECT
					(SELECT id_feature from SHAPE_F_BASE WHERE ((dist_linf < (radius + dist_fc[2])) AND (dist_linf > dist_fc[2] - radius))) LOOP
				--cand = cand + 1;

				select (cube(feature_aux) <=> cube(feature)) INTO distance FROM SHAPE T1 WHERE rec_id.id_feature = T1.id_ft ;
				if (distance < radius) then
					RETURN NEXT (rec_id.id_feature, distance);			       
				end if;				
		END LOOP;
		
	--RAISE NOTICE 'Focos: 2  Podas: %', (24997-cand);
				
	END;$$
	LANGUAGE PLPGSQL;	
	
CREATE OR REPLACE FUNCTION rangeOmniShapeLInfF3 (center_id integer, radius FLOAT8) RETURNS SETOF genericQuery AS $$
	DECLARE rec_id RECORD; is_ok BOOLEAN := FALSE;feature_aux FLOAT8[]; distance FLOAT8; dist_fc FLOAT8[]; cand INTEGER := 0;
	BEGIN
		
		select T1.feature into feature_aux FROM Shape T1 where T1.id_ft = center_id;
		dist_fc = array(select distinct dist_linf from SHAPE_F_BASE where id_feature = center_id);
		
		
		FOR rec_id IN SELECT id_feature from SHAPE_F_BASE WHERE ((dist_linf < (radius + dist_fc[1])) AND (dist_linf > dist_fc[1] - radius)) INTERSECT
					(SELECT id_feature from SHAPE_F_BASE WHERE ((dist_linf < (radius + dist_fc[2])) AND (dist_linf > dist_fc[2] - radius))) INTERSECT
					(SELECT id_feature from SHAPE_F_BASE WHERE ((dist_linf < (radius + dist_fc[3])) AND (dist_linf > dist_fc[3] - radius))) LOOP
				cand = cand + 1;

				select (cube(feature_aux) <=> cube(feature)) INTO distance FROM SHAPE T1 WHERE rec_id.id_feature = T1.id_ft ;
				if (distance < radius) then
					RETURN NEXT (rec_id.id_feature, distance);			       
				end if;				
		END LOOP;
		
	RAISE NOTICE 'Focos: 3  Podas: %', (24997-cand);
				
	END;$$
	LANGUAGE PLPGSQL;

CREATE OR REPLACE FUNCTION rangeOmniShapeLInfF4 (center_id integer, radius FLOAT8) RETURNS SETOF genericQuery AS $$
	DECLARE rec_id RECORD; is_ok BOOLEAN := FALSE;feature_aux FLOAT8[]; distance FLOAT8; dist_fc FLOAT8[]; cand INTEGER := 0;
	BEGIN
		
		select T1.feature into feature_aux FROM Shape T1 where T1.id_ft = center_id;
		dist_fc = array(select distinct dist_linf from SHAPE_F_BASE where id_feature = center_id);
		
		
		FOR rec_id IN SELECT id_feature from SHAPE_F_BASE WHERE ((dist_linf < (radius + dist_fc[1])) AND (dist_linf > dist_fc[1] - radius)) INTERSECT
					(SELECT id_feature from SHAPE_F_BASE WHERE ((dist_linf < (radius + dist_fc[2])) AND (dist_linf > dist_fc[2] - radius))) INTERSECT
					(SELECT id_feature from SHAPE_F_BASE WHERE ((dist_linf < (radius + dist_fc[3])) AND (dist_linf > dist_fc[3] - radius))) INTERSECT
					(SELECT id_feature from SHAPE_F_BASE WHERE ((dist_linf < (radius + dist_fc[4])) AND (dist_linf > dist_fc[4] - radius))) LOOP
				cand = cand + 1;

				select (cube(feature_aux) <=> cube(feature)) INTO distance FROM SHAPE T1 WHERE rec_id.id_feature = T1.id_ft ;
				if (distance < radius) then
					RETURN NEXT (rec_id.id_feature, distance);			       
				end if;				
		END LOOP;
		
	RAISE NOTICE 'Focos: 4  Podas: %', (24997-cand);
				
	END;$$
	LANGUAGE PLPGSQL;

CREATE OR REPLACE FUNCTION rangeOmniShapeLInfF5 (center_id integer, radius FLOAT8) RETURNS SETOF genericQuery AS $$
	DECLARE rec_id RECORD; is_ok BOOLEAN := FALSE;feature_aux FLOAT8[]; distance FLOAT8; dist_fc FLOAT8[]; cand INTEGER := 0;
	BEGIN
		
		select T1.feature into feature_aux FROM Shape T1 where T1.id_ft = center_id;
		dist_fc = array(select distinct dist_linf from SHAPE_F_BASE where id_feature = center_id);
		
		
		FOR rec_id IN SELECT id_feature from SHAPE_F_BASE WHERE ((dist_linf < (radius + dist_fc[1])) AND (dist_linf > dist_fc[1] - radius)) INTERSECT
					(SELECT id_feature from SHAPE_F_BASE WHERE ((dist_linf < (radius + dist_fc[2])) AND (dist_linf > dist_fc[2] - radius))) INTERSECT
					(SELECT id_feature from SHAPE_F_BASE WHERE ((dist_linf < (radius + dist_fc[3])) AND (dist_linf > dist_fc[3] - radius))) INTERSECT
					(SELECT id_feature from SHAPE_F_BASE WHERE ((dist_linf < (radius + dist_fc[4])) AND (dist_linf > dist_fc[4] - radius))) INTERSECT
					(SELECT id_feature from SHAPE_F_BASE WHERE ((dist_linf < (radius + dist_fc[5])) AND (dist_linf > dist_fc[5] - radius))) LOOP
				cand = cand + 1;

				select (cube(feature_aux) <=> cube(feature)) INTO distance FROM SHAPE T1 WHERE rec_id.id_feature = T1.id_ft ;
				if (distance < radius) then
					RETURN NEXT (rec_id.id_feature, distance);			       
				end if;				
		END LOOP;
		
	RAISE NOTICE 'Focos: 5  Podas: %', (24997-cand);
				
	END;$$
	LANGUAGE PLPGSQL;		
	