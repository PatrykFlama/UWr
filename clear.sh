find \( -name '*.exe' -or -name '*.dll' -or -name '*.class' -or -name '*.bak' -or -name '*input.txt' -or -name '*output.txt' \) -print0 | xargs -0 rm
