-- Query for disease-treatedby-hospital
-- SELECT d.virus_name, d.duration, d.incubation
-- FROM disease d LEFT OUTER JOIN treatment_for t ON t.virus_name=d.virus_name
-- WHERE t.hospital_name='Lenox Hill Hospital'


-- Query for medic info
SELECT *
FROM medic
WHERE mid=9000