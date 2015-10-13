-- cohortCluster

-- this script finds all the people in the cluster for the given agegroup/gender
-- extracts their historyVal years' of history 
--  and using ancestor codes creates the table:
-- person_id, defintion1,..., definitionN with the name involving cocept_id, agegroup and gender


-- need to ensure the records I take are valid and occur during observational period
With cohort_tab as (SELECT distinct a.*
from @database.cohort a inner join @database.person b
on a.subject_id=b.person_id
and datediff(year, datefromparts(b.year_of_birth,isnull(b.month_of_birth,1),1), cohort_start_date) between @agelower and @ageupper
and gender_concept_id=@gender
and a.cohort_definition_id =@cohortID)

,sub_cohort as (select a.subject_id, a.cohort_start_date, b.observation_period_start_date, b.observation_period_end_date
from cohort_tab a inner join @database.observation_period b
on a.subject_id=b.person_id
where a.cohort_definition_id=@cohortID
and a.cohort_start_date between b.observation_period_start_date and b.observation_period_end_date)

,person_history as
(select a.person_id, condition_concept_id concept_id
from @database.condition_era a inner join sub_cohort b
on a.person_id=b.subject_id where 
(datediff(day, a.condition_era_start_date, b.cohort_start_date) between 0 and @history 
or b.cohort_start_date between a.condition_era_start_date and a.condition_era_end_date
or datediff(day, a.condition_era_end_date, b.cohort_start_date) between 0 and @history)
and (a.condition_era_start_date between observation_period_start_date and observation_period_end_date or
a.condition_era_end_date between observation_period_start_date and observation_period_end_date )
union 
select a.person_id, drug_concept_id concept_id
from @database.drug_era a inner join sub_cohort b
on a.person_id=b.subject_id where 
(datediff(day, a.drug_era_start_date, b.cohort_start_date) between 0 and @history 
or b.cohort_start_date between a.drug_era_start_date and a.drug_era_end_date
or datediff(day, a.drug_era_end_date, b.cohort_start_date) between 0 and @history)
and (a.drug_era_start_date between observation_period_start_date and observation_period_end_date or
a.drug_era_end_date between observation_period_start_date and observation_period_end_date )
union
select a.person_id, observation_concept_id concept_id
from @database.observation a inner join sub_cohort b
on a.person_id=b.subject_id 
where datediff(day, a.observation_date, b.cohort_start_date) between 0 and @history
and a.observation_date between observation_period_start_date and observation_period_end_date )

-- previously created a table identifying covariate concept_ids called JMR_covariates
,covariate_concepts as
(select distinct covariate, concept_id
from ( select a.covariate, b.descendant_concept_id concept_id
from @outDatabase.JMR_covariates a inner join vocabulary.dbo.concept_ancestor b
on b.ancestor_concept_id=a.concept_id
union
select * from @outDatabase.JMR_covariates
) temp)

-- this gets us the patient's items in the specified history time prior to cohort
-- that occur relatively frequently
,history as (select a.person_id, b.covariate, b.concept_id 
from person_history a inner join covariate_concepts b
on a.concept_id=b.concept_id)

-- now pivot by definitions
@finalSelect



















