CREATE VIEW total as 
SELECT race, state_code, (Count(*) *1.0) as counts
FROM home 
GROUP BY race, state_code;

CREATE VIEW denied as 
SELECT home.race, state_code, (Count(*) *1.0) as counts
FROM home
Where action_taken = 3
GROUP BY home.race, state_code;

Select denied.race, denied.state_code, (denied.counts / total.counts) as percent_denied
From total, denied
Where total.race = denied.race and denied.state_code = total.state_code
order by denied.race, 3 DESC;  
