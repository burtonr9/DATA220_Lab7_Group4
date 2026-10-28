-- W2, given: average daily trips on snow days and on other days.
-- The snow-day label comes from SNOW_DAY_CM in config.R. Change that decision, rerun
-- run_all.R, and compare this result with the one before.
-- Note: a difference here shows an ASSOCIATION between snow and trail use; it does not
-- prove that snow causes the change (cold, weekdays and holidays also matter).
SELECT snow_day,
       COUNT(*)             AS counter_days,
       ROUND(AVG(trips), 1) AS avg_daily_trips
FROM trail_weather
GROUP BY snow_day
ORDER BY snow_day
