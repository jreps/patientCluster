select count(distinct subject_id) num_of_sub
from @database.cohort a inner join @database.person b
on a.subject_id=b.person_id
where a.cohort_definition_id=@cohortID
and gender_concept_id=@gender 
and datediff(year, datefromparts(year_of_birth,isnull(month_of_birth,1),1), cohort_start_date) between @agelower and @ageupper;

