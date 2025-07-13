#!/bin/sh

FILE_DIR="./maphacks/"

FILE_EXT="mhk"

#OLDSTRING="// tile9999 (bla.dot "string" space)"
#OLD_REGEX="\/\/ tile9999 \(bla\.dot \"string\" space\)"

#NEWSTRING="// tile9999 (XYZ.dOT "stR" space .newdot)"
#NEW_REGEX="\/\/ tile9999 \(XYZ\.dOT \"stR\" space \.newdot\)"

#OLD_REGEX="\/\/ Curr\. HRP\: r5\.4\.726"
#NEW_REGEX="\/\/ Curr\. HRP\: r5\.4\.726"

OLD_REGEX="\/\/ tile4864 fem10 sitting babes"
NEW_REGEX="\/\/ tile4864 fem10 \(sitting babes\)"

EXT_LIST="./${FILE_EXT}-all.list"

MATCH_LIST="./${FILE_EXT}-match.list"

exit_on_error() {
  echo "Error: ${@}"
  exit 1
}

if [ "$1" = "" ] ; then
  echo "Edit me (${0}) and enter working directory,\
 file extension, old and new string as regular expressions."
  echo "   Run  \"${0} list\"   to create   a  list of matching files."
  echo "   Run  \"${0} dolist\" to process the list of matching files."
  exit
fi
#  if [ ! "$1" = "list" ] &&  [ "$1" = "dolist" ] ; then
#  fi

if [ ! "$1" = "dolist" ] ; then
  find "${FILE_DIR}" | grep --extended-regexp "\.${FILE_EXT}$" | sort \
    > "${EXT_LIST}"

  if [ ! -f        ${EXT_LIST} ] ; then
    exit_on_error "${EXT_LIST} is missing."
  fi

  FILE_SIZE=$(stat -c%s "${EXT_LIST}")
  if [ $FILE_SIZE = 0 ] ; then
    exit_on_error "No files found with extension \"mhk\"."
    rm -f "${EXT_LIST}"
  fi

  rm -f "${MATCH_LIST}"

  while read MHK_FILE; do # < "${EXT_LIST}"
    #echo ${MHK_FILE}
    #BASH#cat "${MHK_FILE}" | while read MHK_LINE; do
    #  if [[ ${MHK_LINE} =~ ${OLD_REGEX} ]] ; then
    #    echo "${MHK_FILE}" >> "${MATCH_LIST}"
    #    #continue
    #    break
    #  fi
    #done
    grep --extended-regexp --files-with-matches \
     "${OLD_REGEX}" "${MHK_FILE}" >> "${MATCH_LIST}"
    #dos2unix "${MHK_FILE}"
    #unix2dos "${MHK_FILE}"
  done < "${EXT_LIST}"

  FILE_SIZE=$(stat -c%s "${MATCH_LIST}")
  if [ $FILE_SIZE = 0 ] ; then
    rm -f "${MATCH_LIST}"
    exit_on_error \
	"No ${FILE_EXT} files found containing regular expression \"${OLD_REGEX}\"."
  fi
fi

if [ "$1" = "list" ] ; then
  echo "List ${MATCH_LIST} created. Run \"${0} dolist\" to process."
  exit
fi

if [ ! -f        ${MATCH_LIST} ] ; then
  exit_on_error "${MATCH_LIST} is missing."
fi

FILE_SIZE=$(stat -c%s "${MATCH_LIST}")
if [ $FILE_SIZE = 0 ] ; then
  #rm -f "${MATCH_LIST}"
  exit_on_error "${MATCH_LIST} is empty."
fi

while read MATCH_FILE; do # < ${MATCH_LIST}
  #echo ${MATCH_FILE}

  #cat "${MATCH_FILE}" | perl -ple s/"(.*)( \[iittc .*)( \(multiplayer\))(\])\\r"/"\$1\$3\$2\$4"/ \

  #cat "${MATCH_FILE}" | perl -ple s/"(.* )(\()(iittc .*)(\))(.*)\\r"/"\$1\[\$3\]\$5"/ \

  cat "${MATCH_FILE}" | perl -ple s/"${OLD_REGEX}"/"${NEW_REGEX}"/ \
  \
    > "${MATCH_FILE}.tmp"
  mv  "${MATCH_FILE}.tmp" "${MATCH_FILE}"
  #dos2unix "${MATCH_FILE}"
  #unix2dos "${MATCH_FILE}"
done <  ${MATCH_LIST}

#rm -f "${EXT_LIST}"
#rm -f "${MATCH_LIST}"
mv "${MATCH_LIST}" "${MATCH_LIST}.done"

exit 0
