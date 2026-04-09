Quick R scripts that pull PBP data from the MLB stats API and then will put together a pitch plot for a pitcher.

The novel thing here is the formula in the pitch plot to convert the x/y coordinates from the MLB Stats API (which correspond to pixels on the Gameday viewer) to x/y coordinates with a strike zone.

This is experimental. Many parks in MiLB now have Hawkeye systems and these are actual pitch trajectories, but some still rely on stringers to manually input pitch locations on a tablet.
