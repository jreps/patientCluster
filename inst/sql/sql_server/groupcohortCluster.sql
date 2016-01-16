-- cohortCluster
{DEFAULT @cdm_database = 'CDM4_SIM' }
{DEFAULT @cohort_database_schema = 'CDM4_SIM' }
{DEFAULT @cohort_table = 'cohort' }
{DEFAULT @cohort_ids = '1' }
{DEFAULT @cdm_version == '4'}
{DEFAULT @cohort_definition_id = 'cohort_concept_id'}
{DEFAULT @start== 1}
{DEFAULT @end== 180}
{DEFAULT @min_count== 10}
{DEFAULT @use_gender== FALSE}
{DEFAULT @use_age== FALSE}
{DEFAULT @use_desc== TRUE}

USE @cdm_database;
-- this script finds all the people in the cluster for the given agegroup/gender
-- extracts their historyVal years' of history
-- for each condition concept_id that occurs for minobs number of patients

-- need to ensure the records I take are valid and occur during observational period
IF OBJECT_ID('tempdb..#cohort_tab', 'U') IS NOT NULL
	DROP TABLE #cohort_tab;

SELECT distinct  a.*, datediff(year, datefromparts(b.year_of_birth,isnull(b.month_of_birth,1),1), cohort_start_date) age, gender_concept_id gender
into #cohort_tab
from @cohort_database_schema.@cohort_table a inner join person b
on a.subject_id=b.person_id
{@use_age}?{and datediff(year, datefromparts(b.year_of_birth,isnull(b.month_of_birth,1),1), cohort_start_date) between @agelower and @ageupper}:{}
{@use_gender}?{and gender_concept_id=@gender}
and a.@cohort_definition_id =@cohort_ids;

IF OBJECT_ID('tempdb..#sub_cohort', 'U') IS NOT NULL
	DROP TABLE #sub_cohort;

select row_number() over (order by subject_id, cohort_start_date) row_id, age, gender,
a.subject_id, a.cohort_start_date, b.observation_period_start_date, b.observation_period_end_date
into #sub_cohort
from #cohort_tab a inner join observation_period b
on a.subject_id=b.person_id
where a.cohort_start_date between b.observation_period_start_date and b.observation_period_end_date;

IF OBJECT_ID('tempdb..#history', 'U') IS NOT NULL
	DROP TABLE #history;

select DISTINCT row_id, subject_id, concept_id
into #history from
(select row_id, b.subject_id, condition_concept_id concept_id
from condition_era a inner join #sub_cohort b
on a.person_id=b.subject_id where
(datediff(day, a.condition_era_start_date, b.cohort_start_date) between @start and @end
or b.cohort_start_date between a.condition_era_start_date and a.condition_era_end_date
or datediff(day, a.condition_era_end_date, b.cohort_start_date) between @start and @end)
and (a.condition_era_start_date between observation_period_start_date and observation_period_end_date or
a.condition_era_end_date between observation_period_start_date and observation_period_end_date )
union
select row_id, b.subject_id, drug_concept_id concept_id
from drug_era a inner join #sub_cohort b
on a.person_id=b.subject_id where
(datediff(day, a.drug_era_start_date, b.cohort_start_date) between @start and @end
or b.cohort_start_date between a.drug_era_start_date and a.drug_era_end_date
or datediff(day, a.drug_era_end_date, b.cohort_start_date) between @start and @end)
and (a.drug_era_start_date between observation_period_start_date and observation_period_end_date or
a.drug_era_end_date between observation_period_start_date and observation_period_end_date )
union
select row_id, b.subject_id, observation_concept_id concept_id
from observation a inner join #sub_cohort b
on a.person_id=b.subject_id
where datediff(day, a.observation_date, b.cohort_start_date) between @start and @end
and a.observation_date between observation_period_start_date and observation_period_end_date ) temp;

-- previously created a table identifying covariate concept_ids called JMR_covariates
{@use_desc}?{
IF OBJECT_ID('tempdb..#covariate_concepts', 'U') IS NOT NULL
	DROP TABLE #covariate_concepts;

select distinct covariate, concept_id
into #covariate_concepts
from ( select a.covariate, b.descendant_concept_id concept_id
from #covariateGroups a inner join vocabulary.dbo.concept_ancestor b
on b.ancestor_concept_id=a.concept_id
union
select * from #covariateGroups
) temp;}:{select distinct covariate, concept_id from #covariateGroups;}


IF OBJECT_ID('tempdb..#counts', 'U') IS NOT NULL
	DROP TABLE #counts;

select concept_id, count(distinct row_id) tot
into #counts
from #history group by concept_id;

-- this gets us the patient's items in the specified history time prior to cohort
-- that occur relatively frequently
IF OBJECT_ID('tempdb..#covariates', 'U') IS NOT NULL
	DROP TABLE #covariates;

select row_id, a.subject_id, b.covariate, b.concept_id
into #covariates
from #history a inner join #covariate_concepts b
on a.concept_id=b.concept_id;




















