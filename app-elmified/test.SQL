-- Query for disease-treatedby-hospital
-- SELECT d.virus_name, d.duration, d.incubation
-- FROM disease d LEFT OUTER JOIN treatment_for t ON t.virus_name=d.virus_name
-- WHERE t.hospital_name='Lenox Hill Hospital'


-- Query for medic info
-- SELECT *
-- FROM medic
-- WHERE mid=9000





SELECT geoInfo.pid, pat.forename, pat.surname, SQRT((geoInfo.latDiff*geoInfo.latDiff)+(geoInfo.longDiff*geoInfo.longDiff)) AS distance
-- FROM
--   ( SELECT medNum.mid, patNum.pid, (medNum.latitude - patNum.latitude) AS latDiff, (medNum.longitude - patNum.longitude) AS longDiff
--     FROM (SELECT medic.mid, MedLat.latitude, MedLong.longitude
--            FROM (SELECT medic1.mid, CAST (medic1.latitude as DECIMAL(30,15)) AS latitude
--                 FROM medic AS medic1)
--            as MedLat,
--           (SELECT medic2.mid, CAST (medic2.longitude as DECIMAL(30,15)) longitude
--            FROM medic AS medic2)
--            as MedLong, medic
--     WHERE medic.mid= MedLat.mid AND medic.mid = MedLong.mid)
--     AS medNum
--   CROSS JOIN
--     (SELECT patient.pid, PatLat.latitude, PatLong.longitude
--      FROM (SELECT patient1.pid, CAST (patient1.latitude as DECIMAL(30,15)) AS latitude
--            FROM patient
--            AS patient1)
--            as PatLat,
--           (SELECT patient2.pid, CAST (patient2.longitude as DECIMAL(30,15)) longitude
--            FROM patient
--            AS patient2)
--            as PatLong,
--            patient
--            WHERE patient.pid = PatLat.pid AND patient.pid = PatLong.pid) AS patNum)
--            AS geoInfo,
--            patient AS pat
-- WHERE geoInfo.mid =9000 AND pat.pid = geoInfo.pid
-- ORDER BY distance ASC
-- LIMIT 5;
--
-- SELECT %s
FROM
    (   SELECT medNum.mid, patNum.pid, (medNum.latitude - patNum.latitude) AS latDiff, (medNum.longitude - patNum.longitude) AS longDiff
        FROM (  SELECT medic.mid, MedLat.latitude, MedLong.longitude
                FROM (  SELECT medic1.mid, CAST (medic1.latitude as DECIMAL(30,15)) AS latitude
                        FROM medic AS medic1)
             as MedLat,
             (   SELECT medic2.mid, CAST (medic2.longitude as DECIMAL(30,15)) longitude
                FROM medic AS medic2)
             as MedLong,
             medic
        WHERE medic.mid= MedLat.mid AND medic.mid = MedLong.mid)
        AS medNum
    CROSS JOIN
        (   SELECT patient.pid, PatLat.latitude, PatLong.longitude
            FROM (  SELECT patient1.pid, CAST (patient1.latitude as DECIMAL(30,15)) AS latitude
                    FROM patient AS patient1)
                 as PatLat,
                 (  SELECT patient2.pid, CAST (patient2.longitude as DECIMAL(30,15)) longitude
                    FROM patient AS patient2)
                as PatLong,
                patient
            WHERE patient.pid = PatLat.pid AND patient.pid = PatLong.pid) AS patNum )
        AS geoInfo,
        patient AS pat
WHERE geoInfo.mid = 9000 AND pat.pid = geoInfo.pid
ORDER BY distance ASC
LIMIT 5