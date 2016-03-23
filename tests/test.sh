#!/bin/bash

BASEDIR=`dirname "$0"`
COLORDIFF=`which colordiff`
DIFF=`which diff`
PRINTF=`which printf`

cd "$BASEDIR"

if [ -x "$COLORDIFF" ]
then
    DIFF="$COLORDIFF"
elif [ ! -x "$DIFF" ]
then
    echo "Cannot find the diff command. Exiting."
    exit 1
fi

if [ ! -x "$PRINTF" ]
then
    echo "Could not find the printf command. Falling back on echo -- expect ugly output."
    PRINTF="echo"
fi

if [ -x "$BASEDIR/../main.native" ]
then
    ECAML="$BASEDIR/../main.native"
elif [ -x "$BASEDIR/../main.byte" ]
then
    ECAML="$BASEDIR/../main.byte"
else
    echo "Cannot find the ECaml executable. Compile ECaml first."
    exit 1
fi

VALIDATE=0
if [ "$1" = "-v" ]
then
    VALIDATE=1
fi

for FILE in $BASEDIR/*.eml
do
    while :
    do
        "$ECAML" "-n" "$FILE" >"$FILE.out" 2>&1
        if [ -f $FILE.ref ]
        then
            RESULT=`"$DIFF" "$FILE.ref" "$FILE.out"`
            if [ "$?" = "0" ]
            then
                $PRINTF "Test: $FILE                        \r"
                rm "$FILE.out"
            else
                echo "FAILED:  $FILE                          "
                if [ "$VALIDATE" = "1" ]
                then
                    # Is it about whitespace?
                    "$DIFF" --ignore-blank-lines --ignore-all-space "$FILE.ref" "$FILE.out" 2>/dev/null 1>/dev/null
                    if [ "$?" = "0" ]
                    then
                        ONLYWHITE="\n[Note: only white space changed]"
                    else
                        ONLYWHITE=""
                    fi
                    "$DIFF" --unified "$FILE.ref" "$FILE.out"
                    echo "$ONLYWHITE"
                    echo "Validate $FILE.out as new .ref?"
                    read -p "[y:yes, n:no, q:quit, r:rerun) [n] " ans
                    if [ "$ans" = "y" -o "$ans" = "Y" ]
                    then
                        mv "$FILE.out" "$FILE.ref"
                        echo "Validated: $FILE"
                    elif [ "$ans" = "q" -o "$ans" = "Q" ]
                    then
                        exit 0
                    elif [ "$ans" = "r" -o "$and" = "R" ]
                    then
                        continue
                    fi
                fi
            fi

        else
            mv "$FILE.out" "$FILE.ref"
            echo "Created: $FILE.ref                        "
        fi
        break
    done
done
echo "Done.                                       "
