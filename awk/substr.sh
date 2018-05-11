String=12345678
echo | awk '{print substr("${String}",3,4)}'

# Note different string indexing system:
# Bash numbers first character of string as 0.
# Awk numbers first character of string as 1.
