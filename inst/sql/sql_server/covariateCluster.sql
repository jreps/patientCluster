-- strata_extractor
{DEFAULT @cdm_database = 'CDM4_SIM' }
{DEFAULT @work_database_schema = 'CDM4_SIM.dbo' }
{DEFAULT @cdm_version == '4'}
{DEFAULT @cohort_definition_id = 'cohort_concept_id'}
{DEFAULT @min_count== 10}
{DEFAULT @use_before== TRUE}
{DEFAULT @start== 1}
{DEFAULT @end== 180}
{DEFAULT @use_min_obs== TRUE}
{DEFAULT @min_obs== 100}
{DEFAULT @use_drug== TRUE}
{DEFAULT @use_top_ings== FALSE}
{DEFAULT @top_ing== 100}


USE @cdm_database;

-- this script finds all the drug ingredients that occur prior/after to conditions
IF OBJECT_ID('tempdb..#ing_temp', 'U') IS NOT NULL
	DROP TABLE #ing_temp;

SELECT [CONCEPT_ID] ingredient
      ,[CONCEPT_NAME]
      ,[VALID_START_DATE]
      ,[VALID_END_DATE]
      ,[INVALID_REASON]
      into #ing_temp
  FROM [CDM_Optum].[dbo].[CONCEPT]
  where CONCEPT_CLASS = 'Ingredient'  and VOCABULARY_ID=8 and coalesce(invalid_reason, 'non')= 'non'

-- first get ingredients and then decesdants
IF OBJECT_ID('tempdb..#ingredience', 'U') IS NOT NULL
	DROP TABLE #ingredience;

select distinct c.person_id, a.ingredient, c.drug_era_start_date drug_date
into #ingredience
from #ing_temp a left outer join concept_ancestor b
on b.ancestor_concept_id=a.ingredient
inner join drug_era c
on isnull(b.descendant_concept_id,a.ingredient)=c.drug_concept_id;

-- get first time pres in 6 months:
IF OBJECT_ID('tempdb..#ingredience2', 'U') IS NOT NULL
	DROP TABLE #ingredience2;

select * into #ingredience2
  from #ingredience where not exists
(select 1 from #ingredience b where b.person_id=#ingredience.person_id
 and b.ingredient=#ingredience.ingredient and
 datediff(day, b.drug_date, #ingredience.drug_date) between 1 and 180 );

-- only get common conditions
IF OBJECT_ID('tempdb..#covariateCount', 'U') IS NOT NULL
	DROP TABLE #covariateCount;
select a.condition_concept_id, a.person_id, a.condition_era_start_date
into #covariateCount
from condition_era a
inner join
(select condition_concept_id from condition_era
group by condition_concept_id having count(person_id)>=1000) coi
on a.condition_concept_id=coi.condition_concept_id;


-- get drug before/after counts
IF OBJECT_ID('tempdb..#covariateClust', 'U') IS NOT NULL
	DROP TABLE #covariateClust;

select condition_concept_id, c.ingredient ingredience_concept_id,
sum(case when datediff(day, a.condition_era_start_date, c.drug_date ) between
{@use_before}?{-@end and -@start}:{@start and @end}
then 1 else 0 end ) value
into #covariateClust
from
observation_period d inner join person b
on d.person_id=b.person_id
inner join #ingredience2 c
on c.person_id=b.person_id
{@use_min_obs}?{inner join  (select ingredient, count(distinct person_id) tot from #ingredience2
 group by ingredient having count(distinct person_id) > @min_obs) temp on temp.ingredient=c.ingredient}:{
 {@use_top_ing}?{inner join  (select top @top_ing ingredient from #ingredience2
 group by ingredient order by count(distinct person_id) desc) temp on temp.ingredient=c.ingredient}
 }
inner join
(
select condition_concept_id, person_id, condition_era_start_date from #covariateCount)a
on a.person_id=b.person_id
{@use_include}?{inner join #include on #include.concept_id=a.condition_concept_id}
where datediff(day, a.condition_era_start_date, c.drug_date ) between {@use_before}?{-@end and -@start}:{@start and @end}
and a.condition_era_start_date between d.observation_period_start_date and d.observation_period_end_date
group by condition_concept_id, c.ingredient;
