# problemes restant : - indexation
#                     - changement depuis tabulation vers 4 espaces / tab
BEGIN               { code=0 ; quoting=0 ; text=1; defns[0] = 0
                      ulist[0] = 0 }
/^@begin code/      { code=1; }
/^@end code/        { code=0 ; 
                      printf "}\n@end format\n"; lastdefnlabel = "" }
/^@begin docs/      { text=0 }
/^@end docs/        { }
/^@text /           { line = substr($0, 7) ; text += length - 6
                      if (code)          printf "%s", escape_brace_bslash(line)
                      else if (quoting)  printf "%s", TeXliteral(line)
                           else          printf "%s", line
                    } 
/^@nl$/             { if (!code) {if (text==0) printf "\n"
                                  text=1}
                      if (quoting) printf "\n"
                      printf "\n" 
                    }
/^@defn /           { name = substr($0, 7); 
                      if (lastxreflabel != "") {
			ref = lastxreflabel;
			label =  label2tag(lastxreflabel);
		      } else {
			ref = ""; label = "";
		      }
		      printf "@format\n";
		      printf "@i{<<%s>>%s=}@t{", TeXliteral(name), defns[name];
		      lastdefnlabel = lastxreflabel;
                      lastxreflabel = lastxrefref = "";
                      defns[name] = "+";
                    }
/^@use /            { printf "@i{<<%s>>}", 
			TeXliteral(convquotes(substr($0, 6)))
#			(lastxrefref != "" ? ("~" label2tag(lastxrefref)) : "") 
                    }
/^@quote$/          { quoting = 1 ; printf "@code{" }
/^@endquote$/       { quoting = 0 ; printf "}" }
/^@file /           { filename = substr($0, 7); lastxreflabel = lastxrefref = ""
#                      if (!delay) printf "\\nwfilename{%s}", filename 
                    }
/^@literal /        { printf "%s", substr($0, 10) }
/^@header latex /   { }
/^@header tex /     { }
/^@trailer latex$/  { }
/^@trailer tex$/    { }
/^@xref label /     { }
/^@xref ref /       { }
/^@xref begindefs$/ { }
/^@xref defitem /   { }
/^@xref enddefs$/   { }
/^@xref beginuses$/ { }
/^@xref useitem /   { }
/^@xref enduses$/   { }
/^@xref notused /   { }
/^@xref nextdef /   { } 
/^@xref prevdef /   { } 
/^@xref beginchunks$/{ }
/^@xref chunkbegin /{ }
/^@xref chunkuse /  { }
/^@xref chunkdefn / { }
/^@xref chunkend$/  { }
/^@xref endchunks$/ { }
/^@index nl$/       { }
/^@index defn /     { }
/^@index localdefn /{ }
/^@index use /      { }
/^@index begindefs$/{ }
/^@index isused /   { }
/^@index defitem /  { }
/^@index enddefs$/  { }
/^@index beginuses$/{ }
/^@index isdefined /{ }
/^@index useitem /  { }
/^@index enduses$/  { }
/^@index beginindex$/{ }
/^@index entrybegin /{ }
/^@index entryuse /  { }
/^@index entrydefn / { }
/^@index entryend$/  { }
/^@index endindex$/  { }

END                { printf "\n" }

      function label2tag(label) {
        return "{@nwtagstyle{}@subpageref{" label "}}"
      }
      function escape_brace_bslash(line) {
	gsub(/@/,  "@@", line)
        gsub(/}/,  "@}", line)
        gsub(/{/,  "@{", line)
#        gsub(/[\\{}]/, "\n&", line)
        gsub(/\n/, "\\", line)
#        gsub(/@/, "@@", line)
        return line
      }
      function convquotes(s, r, i) {
        r = ""
        while (i = index(s, "[[")) {
          r = r substr(s, 1, i-1) "\\code{}"
          s = substr(s, i+2)
          if (i = match(s, "\\]\\]+")) {
            r = r TeXliteral(substr(s, 1, i-1+RLENGTH-2)) "@edoc{}"
            s = substr(s, i+RLENGTH)
          } else {
            r = r s "@edoc{}"
            s = ""
          }
        }
        return r s
      }
      function indexlabel(ident, l) {
        l = ident
        gsub(/:/,  ":col", l)         # must be first  (colon)
        gsub(/ /,  ":sp",  l)      # space
        gsub(/#/,  ":has", l)     # hash
        gsub(/\$/, ":do",  l)      # dollar
        gsub(/%/,  ":pe",  l)      # percent
        gsub(/&/,  ":am",  l)      # ampersand
        gsub(/,/,  ":com", l)     # commad
        gsub(/\\/, ":bs",  l)      # backslash
        gsub(/\^/, ":hat", l)     # hat
        gsub(/_/,  ":un",  l)      # underscore
        gsub(/{/,  ":lb",  l)      # left brace
        gsub(/}/,  ":rb",  l)      # right brace
        gsub(/~/,  ":ti",  l)      # tilde
        return l
      }
      function TeXliteral(arg) {
#        gsub(/_/,  "@_{}",  arg)
        gsub(/\\/, "<\\char92>",  arg)
#        gsub(/}/,  "<\\char125}", arg)
#        gsub(/{/,  "{\\char123}", arg)
#        gsub(/{/,  "@{{}", arg)
#        gsub(/<\\char/, "{\\char", arg)
        gsub(/{\\char92>/, "{\\char92}", arg)
        gsub(/\$/, "{\\char36}",  arg)
        gsub(/&/,  "{\\char38}",  arg)
        gsub(/#/,  "{\\char35}",  arg)
        gsub(/\^/, "{\\char94}",  arg)
#        gsub(/_/,  "{\\char95}",  arg)
        gsub(/%/,  "{\\char37}",  arg)
        gsub(/~/,  "{\\char126}", arg)
#        gsub(/ /,  "\\ ",         arg)
        return arg
      }
