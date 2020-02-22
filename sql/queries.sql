--Select state_code, AVG(lender_credits::decimal) filter (where lender_credits != 'Exempt' and lender_credits != '') as lender_credits
--Select state_code, ((count(*) filter (where hoepa_status = 1))::decimal  / count(*)) as hoepa_status
Create view modes as 
SELECT census_tract, mode() WITHIN GROUP (ORDER BY lei) AS mode_lender 
FROM home 
Group By census_tract;

Select home.census_tract, ((count(*) filter (where home.lei = modes.mode_lender))::decimal  / count(*)) as market_share
FROM home, modes
Where home.census_tract = modes.census_tract
Group by home.census_tract;


