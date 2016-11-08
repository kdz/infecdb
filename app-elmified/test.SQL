SELECT d.virus_name, d.duration, d.incubation
FROM disease d LEFT OUTER JOIN treatment_for t ON t.virus_name=d.virus_name
WHERE t.hospital_name='Lenox Hill Hospital'