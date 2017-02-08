%% Describes client progress on tracker
-record(tracker_progress, {uploaded = 0 :: integer(),
                           downloaded = 0 :: integer(),
                           left = 0:: integer()}).

-type tracker_progress() :: #tracker_progress{}.
