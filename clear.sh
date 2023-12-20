find \( -name '*.exe' -or -name '*.dll' -or -name '*.class' -or -name '*.bak' -or -name '__pycache__' \) -print0 | xargs -0 rm -rf
#  -or -name '*input.txt' -or -name '*output.txt'