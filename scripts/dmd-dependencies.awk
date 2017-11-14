BEGIN {
    FS = " *: *"
}

#{
#    print "ROBB: " $1 " : " $3
#    next
#}

$1 ~ /^(munstead\.|[a-z]+$)/ && $3 ~ /^munstead\./ {
    gsub(/\./, "/");
    gsub(" ", "");

    $1 = ($1 ~ /munstead/ ? "src" : "tools") "/" $1 ".o"

    $3 = ($3 ~ /munstead/ ? "src" : "tools") "/" $3 ".o"

    print $1 ": " $3
}

{
    next
}