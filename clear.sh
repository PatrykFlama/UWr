find \( -name '*.exe' -or -name '*.dll' -or -name '*.class' -or -name '*.bak' \) -print0 | xargs -0 rm
#  -or -name '*input.txt' -or -name '*output.txt'