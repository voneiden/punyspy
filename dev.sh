tmux \
    new-session  'sh elm-watch.sh' \; \
    split-window 'five-server ./app/' \; \
    split-window 'sh sass-watch.sh' \; \

#detach-client
    #split-window 'sh boiler-watch.sh' \; \

