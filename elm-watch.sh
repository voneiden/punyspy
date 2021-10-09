while true; do
    find src/ -name "*.elm" | entr -d elm make src/Main.elm --output=app/punyspy.js --debug;
    sleep 0.5;
done
