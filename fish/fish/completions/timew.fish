# taken from https://fishshell.com/docs/current/index.html#completion-own
set completions 'continue' 'stop' 'modify' 'summary' 'week' 'day' 'track' 'cancel'
set hints ':ids' ':yesteray' ':day' 'week' ':month' ':quarter' ':year' ':lastweek' ':lastmonth' ':lastquarter' ':lastyear'
set meta_information 'tag' 'untag' 'tags' 'annotate'

# don't complete files
complete -c timew -f

complete -c timew -n "not __fish_seen_subcommand_from $completions" -a "$completions $meta_information"

complete -c timew -n "not __fish_seen_subcommand_from $completions" -a start -d 'Start time tracking'
complete -c timew -n "not __fish_seen_subcommand_from $completions" -a stop -d 'Stop time tracking'
complete -c timew -n "not __fish_seen_subcommand_from $completions" -a track -d 'Add intervals to the database'
complete -c timew -n "not __fish_seen_subcommand_from $completions" -a cancel -d 'Cancel time tracking'
complete -c timew -n "not __fish_seen_subcommand_from $completions" -a continue -d 'Resume tracking of existing interval'
complete -c timew -n "not __fish_seen_subcommand_from $completions" -a summary -d 'Display time tracking summary'
complete -c timew -n "not __fish_seen_subcommand_from $completions" -a tag -d 'Add tags to intervals'
# complete -c timew -n "not __fish_seen_subcommand_from $completions" -a untag -d 'Remove tag from task'
# complete -c timew -n "not __fish_seen_subcommand_from $completions" -a week -d 'See summary of the week thus far'
# complete -c timew -n "not __fish_seen_subcommand_from $completions" -a day -d 'See summary of the day thus far'

complete -c timew -n "__fish_seen_subcommand_from summary" -a "$hints"

# This is a work in progress
# complete -c timew -n "__fish_seen_subcommand_from tag" -a "(timew tags | awk '{print $0}')"