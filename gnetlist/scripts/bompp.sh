#!/bin/sh
# ha ha

if [ -z "$1" ] ; then
    echo "usage $0 geda-bom  # output on stdout"
    exit -1
fi    
cat $1 | /usr/bin/awk '{print toupper($2)" "toupper($3)" "toupper($4)" "toupper($1)}'\
| sort +0 | /usr/bin/awk '\
BEGIN {\
f1="";f2="";f3="";\
format1="%3d %-19s %-10s %-10s ";\
format4="%-46s";\
format2="%s, ";\
format3="%s\n";\
format5="%s,\n";\
urefsperline=5;\
printf(format1 format3, 0, "Device", "Value", "Footprint", "Urefs");\
}\
/^DEVICE/{}\
!/^DEVICE/{\
    if($1==f1 && $2==f2 && $3==f3) {\
        array[ct++]=$4;\
    } else {\
        if(f1!="" && f2!="" && f3!="") { \
            printf(format1, ct, f1, f2, f3);\
	    for(i=0; i<ct-1; i++) {\
	        if((i+1)%urefsperline) {\
		    printf(format2, array[i]);\
		} else {\
		    printf(format5 format4, array[i], "");\
		}\
	    }\
	    printf(format3, array[i]);\
        }\
        f1=$1;f2=$2;f3=$3;ct=0;array[ct++]=$4;\
    }\
}\
END{\
    if(f1!="" && f2!="" && f3!="") { \
        printf(format1, ct, f1, f2, f3);\
	for(i=0; i<ct-1; i++) {\
	    if((i+1)%urefsperline) {\
	    printf(format2, array[i]);\
	} else {\
	    printf(format5 format4, array[i], "");\
	}\
    }\
    printf(format3, array[i]);\
}\
}'

